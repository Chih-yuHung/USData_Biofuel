library(dplyr)
library(tidyr)
library(stringr)

Fertilizer_form <- read.csv("Outputs/3.2_Fertilizer_form - Copy.csv")
Fertilizer_use <- read.csv("Outputs/3.4_Fertilizer_Use_County_Crop - Copy.csv") %>%
  select(-AREA,-RATE_kg_ha)
CEAP_Fertilization <- read_xlsx("Inputs/Fertilization practice_CEAP.xlsx")


# ── 1a.  State‑level fertilizer‑form shares ────────────────────────────────
fert_form_clean <- Fertilizer_form %>%
  mutate(
    FORM = str_replace_all(FORM,                         # normalise names
                           c("Urea ammonium nitrate" = "UAN",
                             "Anhydrous ammonia"      = "AA")),
    Fertilizer_ID = recode(FORM,
                           "Urea"  = 1L,
                           "UAN"   = 2L,
                           "AA"    = 3L,
                           "Other N sources" = 4L,
                           .default = NA_integer_)
  ) %>%
  select(STATE, CROP, CLASS, Fertilizer_ID, PERCENTAGE)

## 1b.  County‑year totals of N applied by crop class ─────────────────
fert_use_agg <- Fertilizer_use %>%
  select(STATE, COUNTY, YEAR, CROP, CLASS, FERT_kg) %>%
  group_by(STATE, COUNTY, YEAR, CROP, CLASS) %>%           # drop DOUBLE
  summarise(FERT_kg = sum(FERT_kg, na.rm = TRUE), .groups = "drop")

# ── 1c.  CEAP region‑level incorporation fractions (CEAP stratum 2 only) ─
ceap_frac <- CEAP_Fertilization %>%
  filter(CEAP == 2) %>%                                    # keep stratum 2
  transmute(Region,                                         # two‑letter CEAP code
            inc_type   = Incorporation_type,               # All/Some/None
            inc_share  = Incorporation_percent/100)        # convert to fraction

# ── 1d.  attach CEAP code to every state name ─────────────────────────────
state2ceap <- state_crosswalk %>%
  select(full, CEAP) %>%
  rename(STATE = full, Region = CEAP)

#  ceap_frac_state: per‐state incorporated / some / none shares -----------
ceap_frac_state <- state2ceap %>%
  left_join(ceap_frac, by = "Region") %>%                       # adds inc_type & inc_share
  mutate(STATE = toupper(STATE))


## ── 1.  CEAP → state: keep All & Some separately ------------------------
incorp_state <- ceap_frac_state %>%          # CEAP = 2 only, from earlier step
  summarise(A = sum(inc_share[inc_type == "All" ], na.rm=TRUE),
            S = sum(inc_share[inc_type == "Some"], na.rm=TRUE),
            .by = STATE)                     # A = “all”, S = “some” (0–1 scale)

## 2.  put fertiliser shares (p1 … p4) in wide form ------------------------
fert_share_wide <- fert_form_clean %>%                    # still state‑level
  select(STATE, CROP, CLASS, Fertilizer_ID, PERCENTAGE) %>%
  mutate(Fertilizer_ID = factor(Fertilizer_ID, levels = 1:4)) %>%
  pivot_wider(names_from = Fertilizer_ID,
              values_from = PERCENTAGE,
              values_fill = 0,
              names_prefix = "p")        


## ── 2.  Join with fertiliser shares and apply the new logic -------------
state_crop_class <- fert_share_wide %>%      # p1 … p4 = Urea,UAN,AA,Other
  left_join(incorp_state, by = "STATE") %>%          # add A,S
  replace_na(list(A = 0, S = 0)) %>%
  rowwise() %>%
  mutate(
    # ------------------------------------------------------------------
    # demanded AA incorporation & available capacity
    need_AA   = p3,
    cap_total = A + S,
    # ------------------------------------------------------------------
    # AA takes first from A, then from S
    inc3 = need_AA,                                # AA gets all it needs
    A_left = ifelse(need_AA >= A, 0, A - need_AA), # "All" left after AA
    S_drawn = pmax(need_AA - A, 0),           # what AA had to draw from “Some”
    S_left  = pmax(S - S_drawn, 0),           # remainder of “Some”
    # ------------------------------------------------------------------
    # what can other ferts still incorporate?
    addl_incorp = A_left + 0.5 * S_left,         # ½ of remaining “Some”
    n_avail     = (p1 > 0) + (p2 > 0) + (p4 > 0),   # how many (1/2/4) present
    inc_other   = addl_incorp / n_avail,           # split evenly to 1,2,4
    #inc_other   = addl_incorp / 3,               # split evenly to 1,2,4
    # cap at each fertiliser’s total share
    inc1 = pmin(p1, inc_other),
    inc2 = pmin(p2, inc_other),
    inc4 = pmin(p4, inc_other),
    # ------------------------------------------------------------------
    broadcast1 = p1 - inc1,
    broadcast2 = p2 - inc2,
    broadcast3 = p3 - inc3,        # will be zero except if AA > capacity (rare)
    broadcast4 = p4 - inc4,
    .keep = "unused"
  ) %>%
  ungroup() %>%
  rename_with(~ gsub("_", "", .x),              # drop underscores
              starts_with(c("broadcast_", "inc_"))) %>%
  select(-incother)                             # drop incother

## ── 3. reshape to long form with Method_ID & Proportion ──────────────
inc_long <- state_crop_class %>%                       # from step2
  select(STATE, CROP, CLASS,
         starts_with("broadcast"), starts_with("inc")) %>%
  pivot_longer(
    cols           = everything()[-(1:3)],             # all *_1 … *_4
    names_to       = c("type", "Fertilizer_ID"),        #   split name
    names_pattern  = "(broadcast|inc)_?(\\d)",            #   by regex
    values_to      = "share_abs"
  ) %>%
  mutate(
    Fertilizer_ID = as.integer(Fertilizer_ID),          # "1","2",… → int
    Method_ID     = if_else(type == "broadcast", 1L, 2L)
  ) %>%
  ## convert absolute share to proportion within each fertiliser
  group_by(STATE, CROP, CLASS, Fertilizer_ID) %>%
  mutate(
  total_share = sum(share_abs, na.rm = TRUE)) %>%
  mutate(Proportion = ifelse(total_share >0, share_abs / total_share, 0)) %>%
  ungroup() %>%
  select(STATE, CROP, CLASS, Fertilizer_ID, Method_ID, Proportion) %>%
  arrange(STATE, CROP, CLASS, Fertilizer_ID, Method_ID)


## ── 4.  expand to county × year ----------------------------------------
final_table <- fert_use_agg %>%                  # keys STATE,COUNTY,YEAR,CROP,CLASS
  distinct(STATE, COUNTY, YEAR, CROP, CLASS) %>%
  left_join(inc_long, by = c("STATE", "CROP", "CLASS")) %>%
  arrange(STATE, COUNTY, YEAR, CROP, CLASS, Fertilizer_ID, Method_ID)

## final_table  contains:
## STATE | COUNTY | YEAR | CROP | CLASS | Fertilizer_ID | Method_ID | Proportion
