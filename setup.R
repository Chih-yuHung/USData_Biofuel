# R/setup.R -------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(soilDB)
  library(readxl)
  library(magick)
  library(here)
})

# ggplot theme
theme_set(theme_bw())

# project-wide paths ----------------------------------------------------
DATA_RAW  <- "data/raw"
DATA_PROC <- "data/processed"

# helper functions
# source("R/functions_climate.R",   local = TRUE)
# source("R/functions_soil.R",      local = TRUE)

# for all state's name
state_crosswalk <- read_csv("Inputs/state_crosswalk.csv") 

# Define the target crops
target_crops <- c("Rye(spring)", "Barley", "Wheat(spring)", "Corn(grain)", "Canola", 
                  "Wheat(durum)", "Corn(silage)", "Peas", "Rye(fall)", "Soybeans", 
                  "Wheat(winter)", "Sorghum", "Camelina", "Triticale", "Rye", "Wheat", "Corn")

# Define the mapping rules
crop_mapping <- function(crop, class) {
  case_when(
    crop == "BARLEY" ~ "Barley",
    crop == "CORN" & class == "GRAIN" ~ "Corn(grain)",
    crop == "CORN" & class == "SILAGE" ~ "Corn(silage)",
    crop == "CORN" & class == "ALL CLASSES" ~ "Corn",
    crop == "RYE" & class == "SPRING" ~ "Rye(spring)",
    crop == "RYE" & class == "FALL" ~ "Rye(fall)",
    crop == "RYE" & class == "ALL CLASSES" ~ "Rye",
    crop == "WHEAT" & class == "WINTER" ~ "Wheat(winter)",
    crop == "WHEAT" & class == "SPRING, (EXCL DURUM)" ~ "Wheat(spring)",
    crop == "WHEAT" & class == "SPRING, DURUM" ~ "Wheat(durum)",
    crop == "SOYBEANS" ~ "Soybeans",
    crop == "SORGHUM" ~ "Sorghum",
    crop == "PEAS" ~ "Peas",
    crop == "TRITICALE" ~ "Triticale",
    crop == "CANOLA" ~ "Canola",
    crop == "CAMELINA" ~ "Camelina",
    TRUE ~ NA_character_  # Remove irrelevant crops
  )
}



message(">>> setup.R loaded")



