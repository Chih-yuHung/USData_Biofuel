# Soil texture (0–30 cm) by county, contiguous USA
# =================================================
# Author: <you> | Date: <today>

# ── 1. Libraries ─────────────────────────────────
library(soilDB)      # SDA API=
library(sf)          # spatial features
library(dplyr)       # data wrangling
library(purrr)       # map_* helpers
library(stringr)     # str_* helpers

# ── 2. Helper functions ──────────────────────────

## 2·1  soilDB-version-agnostic wrapper ----------
fetchSDA <- function(sql, ...) {
  fml <- names(formals(soilDB::fetchSDA_spatial))
  if ("WHERE" %in% fml) {
    soilDB::fetchSDA_spatial(WHERE = sql, ...)      # new API (>=2.7-6)
  } else {
    soilDB::fetchSDA_spatial(sql, ...)              # old API (<=2.7-3)
  }
}

## 2·2  Safe download of ONE survey-area polygon --
# ── NEW safe_fetch_area() ───────────────────────────────────────────
safe_fetch_area <- function(sym) {
  
  # 1 ─ try SDA -> WKT text (bypasses JSON size limit)
  out <- try(
    fetchSDA(
      sprintf("areasymbol = '%s'", sym),
      duplicates = TRUE,
      as_text    = TRUE,   # <-- key flag
      cache      = TRUE),
    silent = TRUE)
  
  if (!inherits(out, "try-error") && nrow(out)) {
    return(st_make_valid(out))
  }
  
  # 2 ─ if that failed, try local gSSURGO for the STATE
  state_code <- substr(sym, 1, 2)
  message("  – SDA failed for ", sym, "; trying gSSURGO …")
  
  out2 <- try(
    fetchGSSURGO(state_code,
                 cache = TRUE,  # huge download only once
                 what  = "spatial"),
    silent = TRUE)
  
  if (!inherits(out2, "try-error")) {
    # gSSURGO returns ALL areas for the state; subset the one we need
    out2 <- out2[out2$areasymbol == sym, ]
    if (nrow(out2)) return(st_make_valid(out2))
  }
  
  # 3 ─ otherwise give back an empty but *valid* sf
  message("  – dropped ", sym)
  st_sf(mukey = character(0), geometry = st_sfc(crs = 4326))
}


## 2·3  Return ALL polygons for one state ---------
get_state_mukey <- function(state_code) {
  cat("\n▶", state_code, " ")
  # List survey areas for this state
  q <- sprintf("SELECT areasymbol FROM legend WHERE areasymbol LIKE '%s%%'", state_code)
  areas <- soilDB::SDA_query(q)$areasymbol
  # Download each survey area
  map_dfr(areas, safe_fetch_area)
}

# ── 3. Static inputs ────────────────────────────

state_abbrs <- c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN","IA",
                 "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
                 "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
                 "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
continental_fips <- setdiff(sprintf("%02d", 1:56),
                            c("02","15","60","66","69","72","78"))

# county boundaries (TIGER/Line shapefile, EPSG:4269)
county_sf <- st_read(list.files("Soil/tl_2022_us_county",
                                pattern = "\\.shp$", full.names = TRUE),
                     quiet = TRUE) |>
  filter(STATEFP %in% continental_fips) |>
  select(STATEFP, COUNTYFP, NAME, geometry)

# ── 4. Main loop ────────────────────────────────
final_county_texture <- vector("list", length(state_abbrs))
names(final_county_texture) <- state_abbrs

for (st in state_abbrs) {
  
  # 4·1  polygons  --------------------------------
  mukey_sf <- get_state_mukey(st)
  if (nrow(mukey_sf) == 0) next
  
  mukey_sf <- st_transform(mukey_sf, st_crs(county_sf))
  
  # 4·2  join polygons → counties -----------------
  mukey_df <- st_join(mukey_sf, county_sf,
                      join = st_intersects, left = FALSE) |>
    st_drop_geometry() |>
    select(mukey, STATEFP, COUNTYFP, county_name = NAME) |>
    distinct()
  if (nrow(mukey_df) == 0) next
  
  # 4·3  SDA properties + acreage -----------------
  mukeys <- unique(mukey_df$mukey)
  
  tex <- get_SDA_property(property  = c("sandtotal_r","silttotal_r","claytotal_r"),
                          method    = "Weighted Average",
                          mukeys    = mukeys,
                          top_depth = 0, bottom_depth = 30)
  
  area <- get_mapunit_from_SDA(mukeys) |>
    select(mukey, muacres)
  
  if (nrow(tex) * nrow(area) == 0) next
  
  # 4·4  merge + county weighted means ------------
  soil_full <- tex |>
    left_join(area,     by = "mukey") |>
    left_join(mukey_df, by = "mukey") |>
    na.omit()
  if (nrow(soil_full) == 0) next
  
  final_county_texture[[st]] <-
    soil_full |>
    group_by(STATEFP, COUNTYFP, county_name) |>
    summarise(
      total_ac = sum(muacres),
      sand_pct = sum(sandtotal_r * muacres) / total_ac,
      silt_pct = sum(silttotal_r * muacres) / total_ac,
      clay_pct = sum(claytotal_r * muacres) / total_ac,
      .groups  = "drop"
    ) |>
    mutate(state = st)
  
  cat("✓")                                   # progress tick
}

# ── 5. Combine & export ─────────────────────────
final_county_texture <- bind_rows(final_county_texture)

print(head(final_county_texture))
# write.csv(final_county_texture,
#           "soil_texture_county_all_states.csv", row.names = FALSE)
