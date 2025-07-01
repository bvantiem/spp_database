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
conduct_raw <- readRDS("data_restricted_access/processed/identified/1a_conduct.Rds")

# Mask the new dataframe so that it's comparable with our cleaned data 
masked_control_nos <- mask_control_nos(conduct_2_raw$control_number)
masked_control_nos <- masked_control_nos %>% distinct()


# Masked dataframes 
conduct_2 <- left_join(conduct_2_raw, masked_control_nos,
                       by = "control_number",
                       relationship = "many-to-one")
conduct <- readRDS("data/processed/de_identified/2_conduct_cleaned.Rds")

# Subset our conduct dataframe to the RCT sample only
admission <- admission %>% 
  filter(rct %in% c(0,1)) %>%
  distinct(research_id, rct, rct_treat_dt, rct_treat_wave, rct_stratum, rel_rct, rct_mnths_pretreat, rct_mnths_to_release)

conduct <- conduct %>%
  left_join(admission, 
            by = "research_id",
            relationship = "many-to-one") %>%
  filter(!is.na(adm_rct)) %>%
  # Drop all misconducts before the rct admission
  filter(cndct_date > adm_rct)

length(unique(conduct$research_id))
# ======================================================================= ####
# Variables in new dataframe that are also in our existing dataframe ####
names(conduct_2)[which(names(conduct_2) %in% names(conduct_raw))]

# Variables in new dataframe that are not in our existing dataframe ####
names(conduct_2)[which(names(conduct_2) %ni% names(conduct_raw))]

# Some places to start digging.. ####
# 1. Assess the variables we've got in conduct_2 ####
assess_variable(conduct_2$control_number)

# 2. Are all research_ids in conduct_2 also in conduct and vice versa? ####
ids_conduct <- unique(conduct$research_id)
ids_conduct_2 <- unique(conduct_2$research_id)
ids_conduct[which(ids_conduct %ni% ids_conduct_2)]
ids_conduct_2[which(ids_conduct_2 %ni% ids_conduct)]

# 3. Is there a pattern to which research_ids are missing? ####
# -- just treated/control?
# -- just chester?
# -- just a certain type of charge/guilty finding?
# -- just certain waves? (wave 7 missing?)
# -- date ranges?
# -- other ideas?
# -- Ideas on how to check this
# -- -- Use assess_variable
# -- -- Look at all IDs that are conduct but not in conduct_2 and vice versa 

# 4. For research_ids that overlap - do the dataframes include the same misconduct numbers? ####
# -- If not, why not? 
# -- -- Different date ranges (conduct_2 only starts from the admission date?)




# ======================================================================= ####