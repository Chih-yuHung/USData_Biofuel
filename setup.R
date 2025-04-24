# R/setup.R -------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(soilDB)
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
state_crosswalk <- read_csv("state_crosswalk.csv") 

message(">>> setup.R loaded")
