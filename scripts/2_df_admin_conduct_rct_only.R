# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Clean conduct data 
# -- Readme ####
# -- -- level of observation: research_id * cndct_num * cndct_chrg_desc
# -- To do ####
# -- -- Follow up with PADOC on cat_chrge1 & vrdict_guilty meaning
# Referencing the excel sheet cat_chrge1 looks to be levels of seriousness of offense
# but want to confirm with PADOC
# -- -- Once we have admit dates, add variables for the number and rate of incidences
# in most recent sentence
# -- Var Definitions: ####
# cndct_pretreat_all
# -- count of unique misconduct numbers by individual prior to rct_treat_dt
# cndct_pretreat_all_count_a
# -- count of unique misconduct numbers where the most serious charge was cat a prior to rct_treat_dt
# cndct_pretreat_guilty
# -- count of unique misconduct numbers by individual prior to rct_treat_dt on which they were found guilty on atleast one charge
# cndct_pretreat_guilty_count_a
# -- count of unique misconduct numbers by individual prior to rct_treat_dt on which the most serious charge on which they were found guilty was cat a
# cndct_posttreat_all
# -- count of unique misconduct numbers by individual from rct_treat_dt to rel_rct for people who have an rel_rct date,
# -- -- and from rct_treat_dt to date_datapull if there is no rel_rct date (still in custody)
# cndct_posttreat_all_count_a
# -- count of unique misconduct numbers by individual from rct_treat_dt to rel_rct for people who have a rel_rct date,
# -- -- and from rct_treat_dt to date_datapull if there is no rel_rct date, where the most serious charge was cat A.
# cndct_posttreat_guilty
# -- count of unique misconduct numbers by individual from rct_treat_dt to rel_rct for people who have a rel_rct date,
# -- --  and from rct_treat_dt to date_datapull if there is no rel_rct, on which they were found guilty on atleast one charge
# cndct_posttreat_guilty_count_a
# -- count of unique misconduct numbers by individual from rct_treat_dt to rel_rct for people who have a rel_rct date,
# -- -- and from rct_treaT_dt to date_datapull if there is no rel_rct date, where the most serious charge on which they were found
# -- -- guilty was cat A
# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions ####
# Data cleaning tools 
remove_leading_zeros <- function(x) {
  # Remove leading zeros
  cleaned_x <- sub("^0+", "", x)
  
  # If a value contained only zeros (now an empty string), replace with "0"
  cleaned_x[grepl("^0*$", x)] <- "0"
  
  return(cleaned_x)
}

# ensure that PA stays capitalized 
acronyms <- c("PA")
standardize_uppercase <- function(x) {
  x <- x %>%
    str_squish() %>%
    str_replace_all("\\s*-\\s*", " - ") %>%                         # Normalize dash spacing
    str_to_title()
  
  # Preserve acronyms (re-insert after title case)
  for (acro in acronyms) {
    pattern <- paste0("\\b", str_to_title(acro), "\\b")
    x <- str_replace_all(x, pattern, acro)
  }
  
  return(x)
}
# -- Read in Data ####
conduct <- readRDS("data/processed/de_identified/1b_conduct_masked.Rds")
admission <- readRDS("data/processed/de_identified/2b_admissions.Rds")
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds")
# ================================================================= ####
# Rename Raw Variables ####
# Append _raw to all columns except specified columns
conduct <- conduct |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(conduct), c("research_id","date_datapull", "control_number", "wave"))) %>%
  mutate(cndct_num = misconduct_number_raw,
         cndct_date = misconduct_date_raw,
         cndct_chrg_desc = chrg_description_raw,
         cndct_guilty = vrdict_guilty_raw,
         cndct_chrg_cat = category_charge1_raw) %>%
  mutate(pris_loc = institution_raw)
# Clean Variables ####


conduct <- conduct %>%
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) %>%
  # -- standardize wave by removing decimal
  mutate(wave = floor(as.numeric(wave))) %>%
  # DATES
  mutate(cndct_date = ymd(cndct_date)) %>%
  # MISCONDUCT
  mutate(cndct_chrg_desc = standardize_uppercase(cndct_chrg_desc)) %>%
  # -- some entries for cndct_guilty have 2 leading zeros, drop these for standardization
  mutate(cndct_guilty = sub("^00", "", cndct_guilty)) %>%
  # PRISON
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) 
# ================================================================= ####
# Add Notes to Variables ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(conduct$cndct_num) <- "Incident number, 0 NA values, NOT FULLY CLEANED, created using misconduct_number_raw (6/4/25)"
comment(conduct$cndct_date) <- "Date of misconduct, 0 NA values, fully cleaned, created using misconduct_date_raw (6/4/25)"
comment(conduct$cndct_chrg_desc) <- "Description of misconduct charge, 0 NA values, fully cleaned, created using chrg_description_raw (6/4/25)"
comment(conduct$cndct_guilty) <- "Binary of guilt?, 0 NA values, NOT FULLY CLEANED, created using vrdict_guilty_raw (6/4/25)"
comment(conduct$pris_loc) <- "Facility of misconduct, 695 NA values, NOT FULLY CLEANED, created using institution_raw (6/4/25)"
# -- Raw Variables ####
comment(conduct$misconduct_number_raw) <- "raw data, not fully cleaned variable available as cndct_num (6/4/25)"
comment(conduct$institution_raw) <- "raw data, not fully cleaned varaible available as pris_loc (6/4/25)"
comment(conduct$misconduct_date_raw) <- "raw data, cleaned variable available as cndct_date (6/4/25)"
comment(conduct$vrdict_guilty_raw) <-  "raw data, not fully cleaned variable available as condct_guilty (6/4/25)"
comment(conduct$category_charge1_raw) <- "raw data, no cleaned variable available (6/4/25)"
comment(conduct$chrg_description_raw) <- "raw data, cleaned variable available as cndct_chrg_desc (6/4/25)"
# ================================================================= ####
# Merge in treatment status and treatment date and subset to rct sample admission ####
admission <- admission %>% 
  filter(rct %in% c(0,1)) %>%
  distinct(research_id, rct, adm_rct, rct_treat_dt, rct_treat_wave, rct_stratum, rel_rct, rct_mnths_pretreat, rct_mnths_to_release)

conduct_rct <- conduct %>%
  left_join(admission, 
            by = "research_id",
            relationship = "many-to-one") %>%
  filter(!is.na(adm_rct)) %>%
  # Drop all misconducts before the rct admission
  filter(cndct_date > adm_rct)

rm(conduct)
# ================================================================= ####
# New Variables ####
# Define conduct charge types ####
drugs <- c("Possession Or Use Of Dangerous Or Contro",
           "Possession Or Use Of Dangerous Substance")
violent <- c("Fighting", 
             "Assault", 
             "Sexual Harassment", 
             "Aggravated Assault", 
             "Rape") # Not included, discuss: "Robbery","Extortion By Threat Of Violence"

conduct_rct <- conduct_rct %>%
  # filter(research_id==conduct_rct$research_id[1]) %>%
  # Conduct charge types 
  mutate(cndct_chrg_type = case_when(
    cndct_chrg_desc %in% drugs ~ "Drugs",
    cndct_chrg_desc %in% violent ~ "Violent",
    TRUE ~ "Other"
  )) %>%
  # Helper Columns ####
  # -- Incidents before and after treatment ####
  mutate(cndct_before_treat = if_else(cndct_date < rct_treat_dt, 1, 0)) %>%
  mutate(cndct_after_treat = case_when(
    !is.na(rel_rct) & cndct_date > rct_treat_dt & cndct_date < rel_rct ~ 1,
    is.na(rel_rct) & cndct_date > rct_treat_dt ~ 1, 
    TRUE ~ 0
  )) %>%
  # -- Most serious charge categories #####
  group_by(research_id, cndct_num) %>%
  mutate(cndct_most_serious = min(cndct_chrg_cat)) %>%
  ungroup() %>%
  group_by(research_id, cndct_num, cndct_guilty) %>%
  mutate(cndct_most_serious_guilty = min(cndct_chrg_cat)) %>%
  ungroup() %>%
  # -- Post-treatment months #####
  # -- These are months to release or to date datapull 
  # -- -- Note that these are called mnths_to_release because the data 
  # -- -- -- distinguishes between mnths_to_exit, mnths_to_release and mnths_since_release
  mutate(rct_mnths_posttreat = case_when(
    !is.na(rel_rct) ~ rct_mnths_to_release,
    is.na(rel_rct) ~ interval(rct_treat_dt, unique(conduct_rct$date_datapull))/months(1),
  )) %>%
  # Pre-treatment counts and rates #### 
  # -- Pre-treatment counts 
  # -- -- All
  group_by(research_id) %>%
  mutate(cndct_pretreat_all_count = n_distinct(cndct_num[cndct_before_treat == 1])) %>%
  mutate(cndct_pretreat_all_count_a = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_most_serious=="A"])) %>%
  mutate(cndct_pretreat_all_count_b = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_most_serious=="B"])) %>%
  mutate(cndct_pretreat_all_count_c = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_most_serious=="C"])) %>%
  # -- -- Guilty
  mutate(cndct_pretreat_guilty_count = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_guilty == 1])) %>%
  mutate(cndct_pretreat_guilty_count_a = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_guilty == 1 & cndct_most_serious_guilty=="A"])) %>%
  mutate(cndct_pretreat_guilty_count_b = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_guilty == 1 & cndct_most_serious_guilty=="B"])) %>%
  mutate(cndct_pretreat_guilty_count_c = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_guilty == 1 & cndct_most_serious_guilty=="C"])) %>%
  # -- -- Drug, Violent
  group_by(research_id) %>%
  mutate(cndct_pretreat_all_count_drug = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_chrg_type == "Drugs"])) %>%
  mutate(cndct_pretreat_all_count_violent = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_chrg_type == "Violent"])) %>%
  # -- -- Drug, Violent Guilty
  mutate(cndct_pretreat_guilty_count_drug = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_guilty == 1 & cndct_chrg_type == "Drugs"])) %>%
  mutate(cndct_pretreat_guilty_count_violent = n_distinct(cndct_num[cndct_before_treat == 1 & cndct_guilty == 1 & cndct_chrg_type == "Violent"])) %>%
  # -- Pre-treatment rates 
  # -- -- All
  mutate(cndct_pretreat_all_rate = cndct_pretreat_all_count/rct_mnths_pretreat) %>%
  mutate(cndct_pretreat_all_rate_a = cndct_pretreat_all_count_a/rct_mnths_pretreat) %>% 
  mutate(cndct_pretreat_all_rate_b = cndct_pretreat_all_count_b/rct_mnths_pretreat) %>% 
  mutate(cndct_pretreat_all_rate_c = cndct_pretreat_all_count_c/rct_mnths_pretreat) %>% 
  # -- -- Guilty
  mutate(cndct_pretreat_guilty_rate = cndct_pretreat_guilty_count/rct_mnths_pretreat) %>%
  mutate(cndct_pretreat_guilty_rate_a = cndct_pretreat_guilty_count_a/rct_mnths_pretreat) %>% 
  mutate(cndct_pretreat_guilty_rate_b = cndct_pretreat_guilty_count_b/rct_mnths_pretreat) %>% 
  mutate(cndct_pretreat_guilty_rate_c = cndct_pretreat_guilty_count_c/rct_mnths_pretreat) %>% 
  # -- -- Drug, Violent
  mutate(cndct_pretreat_all_rate_drug = cndct_pretreat_all_count_drug/rct_mnths_pretreat) %>%
  mutate(cndct_pretreat_all_rate_violent = cndct_pretreat_all_count_violent/rct_mnths_pretreat) %>%
  # -- -- Guilty
  mutate(cndct_pretreat_guilty_rate_drug = cndct_pretreat_guilty_count_drug/rct_mnths_pretreat) %>%
  mutate(cndct_pretreat_guilty_rate_violent = cndct_pretreat_guilty_count_violent/rct_mnths_pretreat) %>%
  # Post-treatment counts and rates #### 
  # -- Post-treatment counts 
  # -- -- All
  mutate(cndct_posttreat_all_count = n_distinct(cndct_num[cndct_after_treat == 1])) %>%
  mutate(cndct_posttreat_all_count_a = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_most_serious=="A"])) %>%
  mutate(cndct_posttreat_all_count_b = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_most_serious=="B"])) %>%
  mutate(cndct_posttreat_all_count_c = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_most_serious=="C"])) %>%
  # -- -- Guilty
  mutate(cndct_posttreat_guilty_count = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_guilty == 1])) %>%
  mutate(cndct_posttreat_guilty_count_a = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_guilty == 1 & cndct_most_serious_guilty=="A"])) %>%
  mutate(cndct_posttreat_guilty_count_b = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_guilty == 1 & cndct_most_serious_guilty=="B"])) %>%
  mutate(cndct_posttreat_guilty_count_c = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_guilty == 1 & cndct_most_serious_guilty=="C"])) %>%
  # -- -- Drug, Violent
  group_by(research_id) %>%
  mutate(cndct_posttreat_all_count_drug = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_chrg_type == "Drugs"])) %>%
  mutate(cndct_posttreat_all_count_violent = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_chrg_type == "Violent"])) %>%
  # -- -- Drug, Violent Guilty
  mutate(cndct_posttreat_guilty_count_drug = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_guilty == 1 & cndct_chrg_type == "Drugs"])) %>%
  mutate(cndct_posttreat_guilty_count_violent = n_distinct(cndct_num[cndct_after_treat == 1 & cndct_guilty == 1 & cndct_chrg_type == "Violent"])) %>%
  # -- Post-treatment rates 
  # -- -- All by category
  mutate(cndct_posttreat_all_rate = cndct_posttreat_all_count/rct_mnths_posttreat) %>%
  mutate(cndct_posttreat_all_rate_a = cndct_posttreat_all_count_a/rct_mnths_posttreat) %>% 
  mutate(cndct_posttreat_all_rate_b = cndct_posttreat_all_count_b/rct_mnths_posttreat) %>% 
  mutate(cndct_posttreat_all_rate_c = cndct_posttreat_all_count_c/rct_mnths_posttreat) %>% 
  # -- -- Guilty by category
  mutate(cndct_posttreat_guilty_rate = cndct_posttreat_guilty_count/rct_mnths_posttreat) %>%
  mutate(cndct_posttreat_guilty_rate_a = cndct_posttreat_guilty_count_a/rct_mnths_posttreat) %>% 
  mutate(cndct_posttreat_guilty_rate_b = cndct_posttreat_guilty_count_b/rct_mnths_posttreat) %>% 
  mutate(cndct_posttreat_guilty_rate_c = cndct_posttreat_guilty_count_c/rct_mnths_posttreat) %>% 
  # -- -- Drug, Violent
  mutate(cndct_posttreat_all_rate_drug = cndct_posttreat_all_count_drug/rct_mnths_posttreat) %>%
  mutate(cndct_posttreat_all_rate_violent = cndct_posttreat_all_count_violent/rct_mnths_posttreat) %>%
  # -- -- Guilty
  mutate(cndct_posttreat_guilty_rate_drug = cndct_posttreat_guilty_count_drug/rct_mnths_posttreat) %>%
  mutate(cndct_posttreat_guilty_rate_violent = cndct_posttreat_guilty_count_violent/rct_mnths_posttreat) %>%
  ungroup() 
# ================================================================= ####
# Reorganize Variables ####
conduct_rct <- reorder_vars(conduct_rct)
# ================================================================= ####
# Save Dataframe ####
saveRDS(conduct_rct, file = "data/processed/de_identified/2_conduct_rct_cleaned.Rds")
# ================================================================= ####
# Temporary Descriptive Stats Britte ####

summary(conduct_rct$cndct_pretreat_all_count[which(conduct_rct$rct==1)])
summary(conduct_rct$cndct_pretreat_all_count[which(conduct_rct$rct==0)])

summary(conduct_rct$cndct_posttreat_all_rate_a[which(conduct_rct$rct==1)])
summary(conduct_rct$cndct_posttreat_all_rate_a[which(conduct_rct$rct==0)])

summary(conduct_rct$cndct_posttreat_all_rate_b[which(conduct_rct$rct==1)])
summary(conduct_rct$cndct_posttreat_all_rate_b[which(conduct_rct$rct==0)])

summary(conduct_rct$cndct_posttreat_all_rate_c[which(conduct_rct$rct==1)])
summary(conduct_rct$cndct_posttreat_all_rate_c[which(conduct_rct$rct==0)])

# ================================================================= ####
# OLD CODE ####
# # -- Misconduct Count by Individual ####
# # count by individual the number of misconduct incidents prior to the date of the first wave
# # count based on misconduct number meaning there could be multiple charges but only counted as one
# # misconduct
# # -- define wave dates
# 
# rand_dates <- tibble(
#   wave = c(0,1,2,2.5,3,4,5,6,7),
#   rand_date = as.Date(c(rand1_date, rand1_date, rand2_date,
#                         rand2.5_date,
#                         rand3_date,
#                         rand4_date, rand5_date, rand6_date, rand7_date)))
# 
# # -- Get unique misconducts with date
# # -- -- count based on misconduct number meaning there could be multiple charges but only counted as one
# misconducts <- conduct_rct %>%
#   select(research_id, cndct_num, cndct_date) %>%
#   distinct() %>%
#   mutate(cndct_date = as.Date(cndct_date))
# 
# # -- Cross join each misconduct with each wave, and keep those that happened BEFORE or ON the rand date
# misconducts_expanded <- tidyr::crossing(misconducts, rand_dates) %>%
#   filter(cndct_date <= rand_date)
# 
# # -- Count unique misconducts per person per wave
# cumulative_counts <- misconducts_expanded %>%
#   group_by(research_id, wave) %>%
#   summarise(cum_cndct = n_distinct(cndct_num), .groups = "drop") %>%
#   mutate(varname = paste0("cndct_rand", wave, "_all")) %>%
#   select(research_id, varname, cum_cndct) %>%
#   pivot_wider(names_from = varname, values_from = cum_cndct)
# 
# # -- Join back into your original conduct data
# conduct_rct <- conduct_rct %>%
#   left_join(cumulative_counts, by = "research_id")
# 
# 
# conduct_rct <- conduct_rct %>%
#   mutate(
#     cndct_pretreat_all = case_when(
#       rct_treat_wave == 0 ~ cndct_rand0_all,
#       rct_treat_wave == 1 ~ cndct_rand1_all,
#       rct_treat_wave == 2 ~ cndct_rand2_all,
#       rct_treat_wave == 2.5 ~ cndct_rand2.5_all,
#       rct_treat_wave == 3 ~ cndct_rand3_all,
#       rct_treat_wave == 4 ~ cndct_rand4_all,
#       rct_treat_wave == 5 ~ cndct_rand5_all,
#       rct_treat_wave == 6 ~ cndct_rand6_all,
#       rct_treat_wave == 7 ~ cndct_rand7_all,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   ungroup()
# 
# # -- Guilty Misconduct Count by Individual ####
# # only the conducts in which people were found guilty
# # -- Filter misconducts to those that were found guilty ("001")
# guilty_misconducts <- conduct_rct %>%
#   filter(cndct_guilty == "1") %>%
#   select(research_id, cndct_num, cndct_date) %>%
#   distinct() %>%
#   mutate(cndct_date = as.Date(cndct_date))
# 
# # -- Cross join with wave dates, keep those on or before rand date
# guilty_expanded <- tidyr::crossing(guilty_misconducts, rand_dates) %>%
#   filter(cndct_date <= rand_date)
# 
# # -- Count unique guilty misconducts per person per wave
# guilty_counts <- guilty_expanded %>%
#   group_by(research_id, wave) %>%
#   summarise(n_guilty = n_distinct(cndct_num), .groups = "drop") %>%
#   pivot_wider(
#     names_from = wave,
#     values_from = n_guilty,
#     names_prefix = "cndct_rand",
#     names_sep = ""
#   )
# 
# # -- Rename columns to match your naming format (e.g., cndct_rand1_guilty)
# names(guilty_counts) <- names(guilty_counts) %>%
#   gsub("^cndct_rand(\\d+)$", "cndct_rand\\1_guilty", .)
# 
# # -- Join back into conduct
# conduct_rct <- conduct_rct %>%
#   select(-starts_with("cndct_rand")) %>%
#   left_join(cumulative_counts, by = "research_id") %>%
#   left_join(guilty_counts, by = "research_id")
# 
# conduct_rct <- conduct_rct %>%
#   rename(cndct_rand2.5_guilty = cndct_rand2.5)
# 
# conduct_rct <- conduct_rct %>%
#   mutate(
#     cndct_pretreat_guilty = case_when(
#       rct_treat_wave == 0 ~ cndct_rand0_guilty,
#       rct_treat_wave == 1 ~ cndct_rand1_guilty,
#       rct_treat_wave == 2 ~ cndct_rand2_guilty,
#       rct_treat_wave == 2.5 ~ cndct_rand2.5_guilty,
#       rct_treat_wave == 3 ~ cndct_rand3_guilty,
#       rct_treat_wave == 4 ~ cndct_rand4_guilty,
#       rct_treat_wave == 5 ~ cndct_rand5_guilty,
#       rct_treat_wave == 6 ~ cndct_rand6_guilty,
#       rct_treat_wave == 7 ~ cndct_rand7_guilty,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   ungroup()
# 
# 
# # -- Misconduct Monthly Rate ####
# # Calculate monthly misconduct rates before treatment using rct_treat_wave
# 
# # 1. Loop through waves 0–7 + 2.5 to create monthly misconduct rate variables
# # for (i in c(0, 1, 2, 2.5, 3, 4, 5, 6, 7)) {
# #   date_col <- sym(paste0("rand", i, "_date"))
# #   count_col <- sym(paste0("cndct_rand", i, "_all"))
# #   rate_col  <- paste0("cndct_mnthly_rand", i, "_all")
# #
# #   conduct_rct <- conduct_rct %>%
# #     mutate(
# #       !!rate_col := !!count_col / (as.numeric(!!date_col - adm_rct) / 30)
# #     )
# # }
# 
# for (i in c(0, 1, 2, 2.5, 3, 4, 5, 6, 7)) {
# 
#   date_col  <- paste0("rand", i, "_date")
#   count_col <- paste0("cndct_rand", i, "_all")
#   rate_col  <- paste0("cndct_mnthly_rand", i, "_all")
# 
#   # if the date column is missing altogether, make sure it exists as NA_Date_
#   if (!date_col %in% names(conduct_rct)) {
#     conduct_rct[[date_col]] <- NA_Date_
#   }
# 
#   conduct_rct <- conduct_rct %>%
#     mutate(
#       !!rate_col := ifelse(
#         is.na(.data[[date_col]]),
#         NA_real_,
#         .data[[count_col]] / (interval(adm_rct, .data[[date_col]]) / months(1))
#       )
#     )
# }
# 
# # 2. Create single pre-treatment misconduct rate column based on rct_treat_wave
# conduct_rct <- conduct_rct %>%
#   mutate(
#     cndct_mnthly_pretreat_all = case_when(
#       rct_treat_wave == 0   ~ cndct_mnthly_rand0_all,
#       rct_treat_wave == 1   ~ cndct_mnthly_rand1_all,
#       rct_treat_wave == 2   ~ cndct_mnthly_rand2_all,
#       rct_treat_wave == 2.5 ~ cndct_mnthly_rand2.5_all,
#       rct_treat_wave == 3   ~ cndct_mnthly_rand3_all,
#       rct_treat_wave == 4   ~ cndct_mnthly_rand4_all,
#       rct_treat_wave == 5   ~ cndct_mnthly_rand5_all,
#       rct_treat_wave == 6   ~ cndct_mnthly_rand6_all,
#       rct_treat_wave == 7   ~ cndct_mnthly_rand7_all,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   ungroup()
# 
# # -- Misconduct Monthly Guilty Rate ####
# # Calculate monthly guilty rates before treatment using rct_treat_wave
# 
# # 1. Loop through waves 0–7 + 2.5 to create monthly guilty rate variables
# # for (i in c(0,1,2,2.5,3,4,5,6,7)) {
# #   date_col <- sym(paste0("rand", i, "_date"))
# #   count_col <- sym(paste0("cndct_rand", i, "_guilty"))
# #   rate_col  <- paste0("cndct_mnthly_rand", i, "_guilty")
# #
# #   conduct_rct <- conduct_rct %>%
# #     mutate(
# #       !!rate_col := !!count_col / (as.numeric(!!date_col - adm_rct) / 30)
# #     )
# # }
# 
# for (i in c(0,1,2,2.5,3,4,5,6,7)) {
# 
#   date_col  <- paste0("rand", i, "_date")
#   count_col <- paste0("cndct_rand", i, "_guilty")
#   rate_col  <- paste0("cndct_mnthly_rand", i, "_guilty")
# 
#   if (!date_col %in% names(conduct_rct)) {
#     conduct_rct[[date_col]] <- NA_Date_
#   }
# 
#   conduct_rct <- conduct_rct %>%
#     mutate(
#       !!rate_col := ifelse(
#         is.na(.data[[date_col]]),
#         NA_real_,
#         .data[[count_col]] / (interval(adm_rct, .data[[date_col]]) / months(1))
#       )
#     )
# }
# 
# # 2. Create single pre-treatment guilty rate column based on rct_treat_wave
# conduct_rct <- conduct_rct %>%
#   mutate(
#     cndct_mnthly_pretreat_guilty = case_when(
#       rct_treat_wave == 0 ~ cndct_mnthly_rand0_guilty,
#       rct_treat_wave == 1 ~ cndct_mnthly_rand1_guilty,
#       rct_treat_wave == 2 ~ cndct_mnthly_rand2_guilty,
#       rct_treat_wave == 2.5 ~ cndct_mnthly_rand2.5_guilty,
#       rct_treat_wave == 3 ~ cndct_mnthly_rand3_guilty,
#       rct_treat_wave == 4 ~ cndct_mnthly_rand4_guilty,
#       rct_treat_wave == 5 ~ cndct_mnthly_rand5_guilty,
#       rct_treat_wave == 6 ~ cndct_mnthly_rand6_guilty,
#       rct_treat_wave == 7 ~ cndct_mnthly_rand7_guilty,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   ungroup()
# 
# 
# 
# # Keep only the pretreatment columns and cndct_guilty
# # Vector of column names you want to subset from
# vars_to_consider <- grep("all|guilty", names(conduct_rct), value = TRUE)
# 
# # Filter only those that contain '_pretreat_'
# vars_to_keep <- vars_to_consider[grepl("_pretreat_|cndct_guilty", vars_to_consider)]
# 
# # Drop all vars_to_consider except those in vars_to_keep
# conduct_rct <- conduct_rct |>
#   select(-all_of(setdiff(vars_to_consider, vars_to_keep)))
# 
# # Set all NA values to zero
# conduct_rct[is.na(conduct_rct)] <- 0
# 
# # -- Pretreatment Counts by Misconduct Category ####
# # -- -- NA for created pretreatment vars is due to having 0 pretreat misconducts (should these be handled differently?)
# # 1. create new column with the highest cndct_charge_cat per misconduct number ("cndct_charge_cat_most_serious")
# conduct_rct <- conduct_rct %>%
#   group_by(research_id, cndct_num) %>%
#   # -- build new column with the lowest alphabetical letter (aka highest charge cat)
#   mutate(cndct_charge_cat_most_serious = min(cndct_chrg_cat, na.rm = TRUE)) %>%
#   ungroup()
# # 2. create another column with the highest cndct_charge_cat found guilty of per misconduct number ("cndct_charge_cat_most_serious_guilty")
# conduct_rct <- conduct_rct %>%
#   group_by(research_id, cndct_num) %>%
#   mutate(
#     cndct_charge_cat_most_serious_guilty = if (any(cndct_guilty == "1")) {
#       min(cndct_chrg_cat[cndct_guilty == "1"], na.rm = TRUE)
#     } else {
#       NA
#     }
#   ) %>%
#   ungroup()
# 
# # 3. Filter to pre-treatment misconducts (using only most serious charge)
# pretreat_most_serious <- conduct_rct %>%
#   distinct(research_id, cndct_num, cndct_charge_cat_most_serious, cndct_date, rct_treat_dt, adm_rct) %>%
#   filter(cndct_date >= adm_rct & cndct_date < rct_treat_dt)
# 
# # 4. Count number of misconducts by category per person
# pretreat_cat_counts <- pretreat_most_serious %>%
#   group_by(research_id, cndct_charge_cat_most_serious, adm_rct, rct_treat_dt) %>%
#   summarise(count = n_distinct(cndct_num), .groups = "drop") %>%
#   # Ensure we keep people who have zero incidents
#   # -- Total number of rows should be # of IDs in pretreat_most_serious * 3 (for A, B, C)
#   complete(
#     research_id,
#     cndct_charge_cat_most_serious,
#     fill = list(count = 0)
#   )
# 
# # 5. Pivot wide
# pretreat_cat_counts_wide <- pretreat_cat_counts %>%
#   pivot_wider(
#     names_from = cndct_charge_cat_most_serious,
#     values_from = count,
#     names_glue = "cndct_pretreat_all_count_{tolower(cndct_charge_cat_most_serious)}",
#     values_fill = 0
#   )
# 
# # 6. Join back into conduct_rct
# conduct_rct <- conduct_rct %>%
#   left_join(pretreat_cat_counts_wide,
#             by = c("research_id", "adm_rct", "rct_treat_dt"))
# 
# # -- -- Create person-level pre-treatment duration (in months) for rates ####
# conduct_rct <- conduct_rct %>%
#   mutate(months_pre = as.numeric(difftime(rct_treat_dt, adm_rct, units = "days")) / 30.44)
# 
# # -- Pretreatment Counts by Guilty Misconduct Category ####
# # 1. Filter to pre-treatment GUILTY misconducts
# pretreat_guilty <- conduct_rct %>%
#   distinct(research_id, cndct_num, cndct_charge_cat_most_serious_guilty, cndct_date, rct_treat_dt, adm_rct) %>%
#   filter(!is.na(cndct_charge_cat_most_serious_guilty), !is.na(cndct_date), !is.na(rct_treat_dt)) %>%
#   filter(cndct_date >= adm_rct, cndct_date < rct_treat_dt)
# 
# # 2. Count number of guilty misconducts by most serious category per person
# guilty_cat_counts <- pretreat_guilty %>%
#   group_by(research_id, cndct_charge_cat_most_serious_guilty) %>%
#   summarise(count = n_distinct(cndct_num), .groups = "drop") %>%
#   # Ensure we keep people who have zero incidents
#   # -- Total number of rows should be # of IDs in pretreat_guilty * 3 (for A, B, C)
#   complete(
#     research_id,
#     cndct_charge_cat_most_serious_guilty,
#     fill = list(count = 0)
#   )
# 
# 
# # 3. Pivot wide to make each category a new column
# guilty_cat_counts_wide <- guilty_cat_counts %>%
#   pivot_wider(
#     names_from = cndct_charge_cat_most_serious_guilty,
#     values_from = count,
#     names_glue = "cndct_pretreat_guilty_count_{tolower(cndct_charge_cat_most_serious_guilty)}",
#     values_fill = 0
#   )
# 
# # 4. Join into conduct_rct
# conduct_rct <- conduct_rct %>%
#   left_join(guilty_cat_counts_wide, by = "research_id") %>%
#   # -- replace NA (for no guilty misconducts) with 0
#   mutate(across(
#     starts_with("cndct_pretreat_guilty_count_"),
#     ~ replace_na(., 0)
#   ))
# # -- Pretreatment Rates by Misconduct Cat for All and Guilty Misconducts ####
# # -- -- Loop through category codes to build rate columns
# for (cat in c("a", "b", "c")) {
#   all_count_col   <- sym(paste0("cndct_pretreat_all_count_", cat))
#   guilty_count_col <- sym(paste0("cndct_pretreat_guilty_count_", cat))
# 
#   all_rate_col    <- paste0("cndct_pretreat_all_rate_", cat)
#   guilty_rate_col <- paste0("cndct_pretreat_guilty_rate_", cat)
# 
#   conduct_rct <- conduct_rct %>%
#     mutate(
#       !!all_rate_col    := !!all_count_col / months_pre,
#       !!guilty_rate_col := !!guilty_count_col / months_pre
#     )
# }
# 
# # -- -- drop helper column
# # conduct_rct <- conduct_rct %>%
# #   select(-"months_pre")
# 
# # -- Posttreatment Counts by Misconduct Category ####
# # 1. Filter to post-treatment misconducts (using only most serious charge)
# posttreat_most_serious <- conduct_rct %>%
#   distinct(research_id, cndct_num, cndct_charge_cat_most_serious, cndct_date, rct_treat_dt, adm_rct, rel_rct) %>%
#   mutate(keep = case_when(
#     !is.na(rel_rct) & cndct_date > rct_treat_dt & cndct_date < rel_rct ~ 1,
#     is.na(rel_rct) & cndct_date > rct_treat_dt ~ 1,
#     TRUE ~ 0
#   )) %>%
#   filter(keep == 1) %>%
#   select(-keep) %>%
#   select(-rel_rct) # Drop it before merging later as it's already in conduct_rct
# 
# # 2. Count number of misconducts by category per person
# posttreat_cat_counts <- posttreat_most_serious %>%
#   group_by(research_id, cndct_charge_cat_most_serious, adm_rct, rct_treat_dt) %>%
#   summarise(count = n_distinct(cndct_num), .groups = "drop") %>%
#   # Ensure we keep people who have zero incidents
#   # -- Total number of rows should be # of IDs in posttreat_most_serious * 3 (for A, B, C)
#   complete(
#     research_id,
#     cndct_charge_cat_most_serious,
#     fill = list(count = 0)
#   )
# 
# 
# # 3. Pivot wide
# posttreat_cat_counts_wide <- posttreat_cat_counts %>%
#   pivot_wider(
#     names_from = cndct_charge_cat_most_serious,
#     values_from = count,
#     names_glue = "cndct_posttreat_all_count_{tolower(cndct_charge_cat_most_serious)}",
#     values_fill = 0
#   )
# 
# # 4. Join back into conduct_rct
# conduct_rct <- conduct_rct %>%
#   left_join(posttreat_cat_counts_wide,
#             by = c("research_id", "adm_rct", "rct_treat_dt"))
# 
# # -- -- Create person-level post-treatment duration (in months) for rates ####
# # -- Time from treatment to release for those who are released
# # -- Time from treatment to today (date code was run) for those who are not yet released
# conduct_rct <- conduct_rct %>%
#   mutate(months_post = case_when(
#     !is.na(rel_rct) ~ as.numeric(difftime(rel_rct, rct_treat_dt, units = "days")) / 30.44,
#     is.na(rel_rct) ~ as.numeric(difftime(today(), rct_treat_dt, units = "days")) / 30.44,
#     TRUE ~ NA))
# 
# # -- Posttreatment Counts by Guilty Misconduct Category ####
# # 1. Filter to pre-treatment GUILTY misconducts
# posttreat_guilty <- conduct_rct %>%
#   distinct(research_id, cndct_num, cndct_charge_cat_most_serious_guilty, cndct_date, rct_treat_dt, adm_rct) %>%
#   filter(cndct_date > rct_treat_dt)
# 
# # 2. Count number of guilty misconducts by most serious category per person
# guilty_cat_counts <- posttreat_guilty %>%
#   group_by(research_id, cndct_charge_cat_most_serious_guilty) %>%
#   summarise(count = n_distinct(cndct_num), .groups = "drop") %>%
#   # Ensure we keep people who have zero incidents
#   # -- Total number of rows should be # of IDs in posttreat_guilty * 3 (for A, B, C)
#   complete(
#     research_id,
#     cndct_charge_cat_most_serious_guilty,
#     fill = list(count = 0)
#   )
# 
# # 3. Pivot wide to make each category a new column
# guilty_cat_counts_wide <- guilty_cat_counts %>%
#   pivot_wider(
#     names_from = cndct_charge_cat_most_serious_guilty,
#     values_from = count,
#     names_glue = "cndct_posttreat_guilty_count_{tolower(cndct_charge_cat_most_serious_guilty)}",
#     values_fill = 0
#   )
# 
# # 4. Join into conduct_rct
# conduct_rct <- conduct_rct %>%
#   left_join(guilty_cat_counts_wide, by = "research_id") %>%
#   # -- replace NA (for no guilty misconducts) with 0
#   mutate(across(
#     starts_with("cndct_posttreat_guilty_count_"),
#     ~ replace_na(., 0)
#   ))
# # -- Posttreatment Rates by Misconduct Cat for All and Guilty Misconducts ####
# # -- -- Loop through category codes to build rate columns
# for (cat in c("a", "b", "c")) {
# 
#   all_count_col     <- paste0("cndct_posttreat_all_count_", cat)
#   guilty_count_col  <- paste0("cndct_posttreat_guilty_count_", cat)
#   all_rate_col      <- paste0("cndct_posttreat_all_rate_", cat)
#   guilty_rate_col   <- paste0("cndct_posttreat_guilty_rate_", cat)
# 
#   # force missing count columns to exist
#   if (!all_count_col %in% names(conduct_rct)) {
#     conduct_rct[[all_count_col]] <- NA
#   }
#   if (!guilty_count_col %in% names(conduct_rct)) {
#     conduct_rct[[guilty_count_col]] <- NA
#   }
# 
#   conduct_rct <- conduct_rct %>%
#     mutate(
#       !!all_rate_col    := !!sym(all_count_col) / months_post,
#       !!guilty_rate_col := !!sym(guilty_count_col) / months_post
#     )
# }
# 
# # -- -- drop helper column
# # conduct_rct <- conduct_rct %>%
# #   select(-"months_post")

# Temporary Descriptive Stats Gabby ####
# -- number of misconducts per unique control number
# -- -- NOTE: individuals are only in this dataset if they have committed atleast
#             one misconduct, does not include anyone with 0 misconducts
# misconducts_per_person <- conduct %>%
#   group_by(research_id) %>%
#   summarize(n_misconducts = n())
# summary(misconducts_per_person$n_misconducts)
# 
# # -- likelihood of each verdict result
# conduct %>%
#   count(cndct_guilty) %>%
#   mutate(percent = n / sum(n) * 100)
# 
# # -- most common misconduct charges
# conduct %>%
#   count(cndct_chrg_desc, sort = TRUE) %>%
#   slice_head(n = 10)  # top 10