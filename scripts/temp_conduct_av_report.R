# ======================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts_restricted_access/0_control_no_masking_function.R")

# -- Read in Data ####
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds")
admission <- readRDS("data/processed/de_identified/2b_admissions.Rds")
# New dataframe and our identified data 
conduct_2_raw <- read_excel("data_restricted_access/raw/1_admin_data/other/tbl_15_Misconducts_20250520.xlsx")

# Mask the new dataframe so that it's comparable with our cleaned data 
masked_control_nos <- mask_control_nos(conduct_2_raw$control_number)
masked_control_nos <- masked_control_nos %>% distinct()

# Masked dataframes 
conduct_2 <- left_join(conduct_2_raw, masked_control_nos,
                       by = "control_number",
                       relationship = "many-to-one")
conduct <- readRDS("data/processed/de_identified/2_conduct_rct_cleaned.Rds")

# Subset our conduct dataframe to the RCT sample only
admission <- admission %>% 
  filter(rct %in% c(0,1)) %>%
  distinct(research_id, rct, rct_treat_dt, rct_treat_wave, rct_stratum, rel_rct, rct_mnths_pretreat, rct_mnths_to_release)

conduct <- conduct %>%
  # Drop all misconducts before randomization
  filter(cndct_after_treat == 1,
         cndct_date <= as.Date(ymd(20250516)))


# -- Notes/Findings ####
# -- 1. only those in the treatment/control group seem to be in conduct_2
# -- 2. conduct_2 only includes post-treat misconducts
# -- -- conduct_2 time range: 2022-06-07 -> 2025-05-16
# -- -- conduct time range: 2000-07-06 -> 2025-06-20
# -- 3. Wave 7 is missing from conduct_2
# ======================================================================= ####
# Variables difference ####
names(conduct_2)[which(names(conduct_2) %in% names(conduct_raw))]
names(conduct_2)[which(names(conduct_2) %ni% names(conduct_raw))]

# ======================================================================= ####
# Difference in research IDs included ####
# 2 ids in our data missing from conduct_2
missing_from_conduct_2 <- conduct %>%
  filter(research_id %ni% conduct_2$research_id) %>%
  distinct(research_id)

# 18 ids in longer list not in our data 
missing_from_conduct <- conduct_2 %>%
  filter(!research_id %in% conduct$research_id) %>%
  distinct(research_id)

# -- of which 8 are not in the rct sample per our randassign data
missing_from_conduct %>%
  filter(research_id %in% randassign$research_id) %>%
  distinct()

# ======================================================================= ####
# Subset to overlapping IDs ####
conduct_overlap <- conduct %>% 
  filter(research_id %ni% missing_from_conduct_2$research_id) %>%
  filter(research_id %ni% missing_from_conduct$research_id)

conduct_2_overlap <- conduct_2 %>% 
  filter(research_id %ni% missing_from_conduct_2$research_id) %>%
  filter(research_id %ni% missing_from_conduct$research_id)

nrow(conduct_overlap)
nrow(conduct_2_overlap)

# ======================================================================= ####
# Conditional on overlapping IDs, which misconduct numbers are missing? ####
cndct_num_missing_from_conduct <- conduct_overlap %>%
  filter(misconduct_number_raw %ni% conduct_2_overlap$misconduct_number) %>%
  distinct(misconduct_number_raw)

cndct_num_missing_from_conduct_2 <- conduct_2_overlap %>%
  filter(misconduct_number %ni% conduct_overlap$misconduct_number_raw) %>%
  distinct(misconduct_number)
# ======================================================================= ####