# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Merge release dates into admission 
# adm_rct should be 
# -- Readme ####

# -- To do (delete if none) ####


# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions (delete if none) ####


# -- Read in Data ####

randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds")
release <- readRDS("data/processed/de_identified/1b_release_masked.Rds")

# move <- readRDS("data/processed/de_identified/2_move_cleaned.Rds")
move <- readRDS("data/processed/de_identified/1b_move_masked.Rds")

# ================================================================= ####
# Delete second release ####
duplicates <- release %>%
  count(research_id) %>%
  filter(n > 1) %>%
  pull(research_id)

release <- release %>% 
  mutate(duplicate = ifelse(research_id %in% duplicates, 1, 0)) %>%
  group_by(research_id) %>%
  mutate(drop = ifelse(duplicate == 1 & delete_date == max(delete_date), 1, 0)) %>%
  filter(drop == 0)


