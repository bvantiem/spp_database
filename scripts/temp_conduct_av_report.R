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
# -- Notes/Findings ####
# -- 1. only those in the treatment/control group seem to be in conduct_2
# -- 2. conduct_2 only includes post-treat misconducts
# -- -- conduct_2 time range: 2022-06-07 -> 2025-05-16
# -- -- conduct time range: 2000-07-06 <- 2025-06-20
# -- 3. Wave 7 is missing from conduct_2
# ======================================================================= ####
# Variables in new dataframe that are also in our existing dataframe ####
names(conduct_2)[which(names(conduct_2) %in% names(conduct_raw))]

# Variables in new dataframe that are not in our existing dataframe ####
names(conduct_2)[which(names(conduct_2) %ni% names(conduct_raw))]

# ======================================================================= ####
# Some places to start digging.. ####
# 1. Assess the variables we've got in conduct_2 ####
assess_variable(conduct_2$control_number)
# -- 548 unique inmate_numbers/ inmate_id_nums
assess_variable(conduct_2$state_id_num)
assess_variable(conduct_2$inmate_number)
assess_variable(conduct_2$LSStartDate)
# -- refers to the date they started on LS 
# -- -- missing wave 7!
assess_variable(conduct_2$RandomAssignment)
assess_variable(conduct_2$misconduct_number)
assess_variable(conduct_2$institution)
# -- more than just CHS
assess_variable(conduct_2$misconduct_date)
assess_variable(conduct_2$MisconductDate)
# -- only includes post-treat misconducts 
assess_variable(conduct_2$category_charge1)
assess_variable(conduct_2$chrg_description)
assess_variable(conduct_2$place_hvl_code)
# -- specific location of misconduct
assess_variable(conduct_2$place_hvl_desc)
assess_variable(conduct_2$inmte_plea_gilty)
assess_variable(conduct_2$inmte_plea_ngilty)
assess_variable(conduct_2$inmte_plea_nplea)
assess_variable(conduct_2$vrdict_guilty)
assess_variable(conduct_2$vrdict_nguilty)
assess_variable(conduct_2$vrdict_dismis_wp)
assess_variable(conduct_2$vrdict_dismis_wop)
assess_variable(conduct_2$vrdict_reduced)
assess_variable(conduct_2$data_followup)
assess_variable(conduct_2$research_id)

# 2. Are all research_ids in conduct_2 also in conduct and vice versa? ####
ids_conduct <- unique(conduct$research_id)
ids_conduct_2 <- unique(conduct_2$research_id)
# In conduct but not in conduct_2
missing_from_conduct_2 <- conduct %>%
  filter(!research_id %in% conduct_2$research_id)
# -- 153 research_ids missing from conduct_2

# In conduct_2 but not in conduct
missing_from_conduct <- conduct_2 %>%
  filter(!research_id %in% conduct$research_id)
# -- 11 research_ids missing from conduct

# 3. Is there a pattern to which research_ids are missing? ####
# -- just treated/control?
# -- just chester?
# -- -- other facilties included
# -- just a certain type of charge/guilty finding?
# -- just certain waves? (wave 7 missing?)
# -- -- wave 7 missing
# -- date ranges?
# -- -- 2022-06-07 -> 2025-05-16 (conduct df is 2000-07-06 -> 2025-06-20)
# -- other ideas?
# -- Ideas on how to check this
# -- -- Use assess_variable
# -- -- -- conduct_2$misconduct_number has 322 unique misconduct numbers
# -- -- -- conduct$misconduct_number has 1115 unique misconduct numbers
# -- -- Look at all IDs that are conduct but not in conduct_2 and vice versa 

# 4. For research_ids that overlap - do the dataframes include the same misconduct numbers? ####
# -- If not, why not? 
# -- -- Different date ranges (conduct_2 only starts from the admission date?)
# 5. Subset conduct df for the date range of the conduct_2 df and waves 0-6 ####
temp <- conduct %>% filter(
  cndct_date >= as.Date("2022-06-07") & cndct_date <= as.Date("2025-05-16")) %>%
    filter(rct_treat_wave.x >= "0" & rct_treat_wave.x <= "6")
# -- same research_ids? ####
ids_temp <- unique(temp$research_id)
ids_conduct_2 <- unique(conduct_2$research_id)
# In temp but not in conduct_2
missing_from_conduct_2 <- temp %>%
  filter(!research_id %in% conduct_2$research_id)
# -- 47 research_ids missing from conduct_2

# In conduct_2 but not in temp
missing_from_temp <- conduct_2 %>%
  filter(!research_id %in% temp$research_id)
# -- 11 reserach_ids missing from temp

# -- same cndct_nums? ####
cndct_nums_temp <- unique(temp$cndct_num) 
cndct_nums_conduct_2 <- unique(conduct_2$control_number)

# In temp but not in conduct_2
cndct_num_missing_from_conduct_2 <- temp %>%
  filter(!cndct_num %in% conduct_2$misconduct_number)
# -- 105 misconduct numbers missing from conduct_2

# In conduct_2 but not in temp
cndct_num_missing_from_temp <- conduct_2 %>%
  filter(!misconduct_number %in% temp$cndct_num)
# -- 33 misconduct numbers missing from temp
# ======================================================================= ####