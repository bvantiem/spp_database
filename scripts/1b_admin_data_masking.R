# ========================================================================= ####
# Notes to Script
# Notes to Script ####
# -- Objective ####
# Mask control numberes in administrative dataframes
# -- Readme ####
# -- To do ####
# ========================================================================= ####
# Set Up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_control_no_masking_function.R")

# -- Load data ####
control_nos_inmate_ids <- readRDS("data/processed/processing_layer_1/control_nos_inmate_ids.Rds")
for (name in c("basic", "move", "assess", "house", "program", "conduct", "work", "visit")) {
  assign(name, readRDS(paste0("data/processed/processing_layer_1/", name, ".Rds")))
}

# ========================================================================= ####
# Mask IDs ####
i <- unique(control_nos_inmate_ids$control_number)
id.link <- mask_control_nos(i) # Generate masked Research IDs

# Some control numbers are identical except for a leading zero
# make sure we add a leading zero to ensure all control numbers have 6 digits
id.link <- id.link %>%
  mutate(control_number = sprintf("%06d", as.integer(control_number)))

for (df_name in c("basic", "move", "assess", "house", "program", "conduct", "work", "visit")) {
  print(df_name)
  
  updated_df <- get(df_name) %>%
    mutate(control_number = sprintf("%06d", as.integer(control_number))) %>%  # Pad to 6 digits
    left_join(id.link, by = "control_number") %>%
    select(-any_of(c("state_id_num", "inmate_id"))) %>%
    relocate(research_id) # Moves research_id to the front
  
  new_name <- paste0(df_name, "_masked")
  assign(new_name, updated_df)
}
# ========================================================================= ####
# Save masked data frames ####
for (name in c("basic", "move", "assess", "house", "program", "conduct", "work", "visit")) {
  masked_df <- get(paste0(name, "_masked")) %>% as.data.frame()
  saveRDS(masked_df, file = paste0("data/processed/processing_layer_2/", name, "_masked.Rds"))
}

