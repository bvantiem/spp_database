# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Mask pcq data 
# -- Readme ####

# -- To do (delete if none) ####


# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_control_no_masking_function.R")
# -- Functions (delete if none) ####


# -- Read in Data ####
control_nos_inmate_ids <- readRDS("data/processed/processing_layer_1/control_nos_inmate_ids.Rds")
survey_population_unmasked <- readRDS("data/processed/processing_layer_2/survey_population_unmasked.Rds")

# ========================================================================= ####
# Mask IDs ####
i <- unique(control_nos_inmate_ids$control_number)
id.link <- mask_control_nos(i) # Generate masked Research IDs

survey_population_masked <- survey_population_unmasked %>%
  left_join(id.link, by = "control_number") %>%
  select(-any_of(c("control_number", "inmate_id", "id_num", "id_num_2"))) %>%
  relocate(research_id) # Moves research_id to the front

# ================================================================= ####
# Save pcq_masked #####
saveRDS(survey_population_masked, file = "data/processed/processing_layer_2/survey_population_masked.Rds")
