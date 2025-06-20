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
source("scripts_restricted_access/0_control_no_masking_function.R")
# -- Functions (delete if none) ####


# -- Read in Data ####
control_nos_inmate_ids <- readRDS("data_restricted_access/processed/identified/1a_control_nos_inmate_ids.Rds")
pcq_unmasked <- readRDS("data_restricted_access/processed/identified/2a_pcq.Rds")

# ========================================================================= ####
# Mask IDs ####
i <- unique(control_nos_inmate_ids$control_number)
id.link <- mask_control_nos(i) # Generate masked Research IDs

pcq_masked <- pcq_unmasked %>%
  left_join(id.link, by = "control_number") %>%
  select(-any_of(c("control_number", "inmate_id", "id_num", "id_num_2"))) %>%
  relocate(research_id) # Moves research_id to the front

# ================================================================= ####
# Save pcq_masked #####
saveRDS(pcq_masked, file = paste0("data/processed/de_identified/2b_pcq_masked.Rds"))
