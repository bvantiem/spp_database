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

# -- Variable Defs: 
# cndct_pre_wave1_guilty_count_a
# -- count of unique misconduct numbers by individual for individuals who were in prison at the study start from adm_study to wave1_date,
# -- -- where the most serious charge on which they were found guilty was cat A
# cndct_posttreat_guilty_count_a
# -- count of unique misconduct numbers by individual from rct_treat_dt to rel_rct for people who have a rel_rct date,
# -- -- and from rct_treaT_dt to date_datapull if there is no rel_rct date, where the most serious charge on which they were found
# -- -- guilty was cat A
# ================================================================= ####
# Code to align with merged administrative data files ####
conduct$date_datapull <- ymd(20250623)
conduct$wave <- 0
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
# New Variables ####
# -- Misconduct Count by Individual ####
# count by individual the number of misconduct incidents prior to the date of the first wave
# count based on misconduct number meaning there could be multiple charges but only counted as one
# misconduct
# -- define wave dates

rand_dates <- tibble(
  wave = 0:7,
  rand_date = as.Date(c(rand1_date, rand1_date, rand2_date, rand3_date, 
                        rand4_date, rand5_date, rand6_date, rand7_date)))

# -- Get unique misconducts with date
misconducts <- conduct %>%
  select(research_id, cndct_num, cndct_date) %>%
  distinct() %>%
  mutate(cndct_date = as.Date(cndct_date))

# -- Cross join each misconduct with each wave, and keep those that happened BEFORE or ON the rand date
misconducts_expanded <- tidyr::crossing(misconducts, rand_dates) %>%
  filter(cndct_date <= rand_date)

# -- Count unique misconducts per person per wave
cumulative_counts <- misconducts_expanded %>%
  group_by(research_id, wave) %>%
  summarise(cum_cndct = n_distinct(cndct_num), .groups = "drop") %>%
  mutate(varname = paste0("cndct_rand", wave, "_all")) %>%
  select(research_id, varname, cum_cndct) %>%
  pivot_wider(names_from = varname, values_from = cum_cndct)

# -- Join back into your original conduct data
conduct <- conduct %>%
  left_join(cumulative_counts, by = "research_id")

# -- Guilty Misconduct Count by Individual ####
# only the conducts in which people were found guilty
# -- Filter misconducts to those that were found guilty ("001")
guilty_misconducts <- conduct %>%
  filter(cndct_guilty == "1") %>%
  select(research_id, cndct_num, cndct_date) %>%
  distinct() %>%
  mutate(cndct_date = as.Date(cndct_date))

# -- Cross join with wave dates, keep those on or before rand date
guilty_expanded <- tidyr::crossing(guilty_misconducts, rand_dates) %>%
  filter(cndct_date <= rand_date)

# -- Count unique guilty misconducts per person per wave
guilty_counts <- guilty_expanded %>%
  group_by(research_id, wave) %>%
  summarise(n_guilty = n_distinct(cndct_num), .groups = "drop") %>%
  pivot_wider(
    names_from = wave,
    values_from = n_guilty,
    names_prefix = "cndct_rand",
    names_sep = ""
  )

# -- Rename columns to match your naming format (e.g., cndct_rand1_guilty)
names(guilty_counts) <- names(guilty_counts) %>%
  gsub("^cndct_rand(\\d+)$", "cndct_rand\\1_guilty", .)

# -- Join back into conduct
conduct <- conduct %>%
  select(-starts_with("cndct_rand")) %>%
  left_join(cumulative_counts, by = "research_id") %>%
  left_join(guilty_counts, by = "research_id")

# -- Misconduct Monthly Rate ####
admission <- admission %>% 
  filter(rct %in% c(0,1)) %>%
  distinct(research_id, adm_rct, rct_treat_dt, rct_treat_wave)

conduct <- conduct %>%
  left_join(admission %>% select(research_id, adm_rct, rct_treat_wave), by = "research_id")

add_pretreatment_rates <- function(conduct, admission) {
  
  for (i in 0:7) {
    date_col <- sym(paste0("rand", i, "_date"))
    count_col <- sym(paste0("cndct_rand", i, "_all"))
    rate_col  <- paste0("cndct_mnthly_rand", i, "_all")
    
    conduct <- conduct %>%
      mutate(
        !!rate_col := !!count_col / (as.numeric(!!date_col - adm_rct) / 30)
      )
  }
  
  conduct <- conduct %>%
    mutate(
      cndct_mnthly_pretreat_all = case_when(
        rct_treat_wave == 0 ~ cndct_mnthly_rand0_all,
        rct_treat_wave == 1 ~ cndct_mnthly_rand1_all,
        rct_treat_wave == 2 ~ cndct_mnthly_rand2_all,
        rct_treat_wave == 3 ~ cndct_mnthly_rand3_all,
        rct_treat_wave == 4 ~ cndct_mnthly_rand4_all,
        rct_treat_wave == 5 ~ cndct_mnthly_rand5_all,
        rct_treat_wave == 6 ~ cndct_mnthly_rand6_all,
        rct_treat_wave == 7 ~ cndct_mnthly_rand7_all,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup()
  
  return(conduct)
}
conduct <- add_pretreatment_rates(conduct,admission)

# -- Misconduct Monthly Guilty Rate ####
add_pretreatment_guilty_rates <- function(conduct, admission) {
  
  for (i in 0:7) {
    date_col <- sym(paste0("rand", i, "_date"))
    count_col <- sym(paste0("cndct_rand", i, "_guilty"))
    rate_col  <- paste0("cndct_mnthly_rand", i, "_guilty")
    
    conduct <- conduct %>%
      mutate(
        !!rate_col := !!count_col / (as.numeric(!!date_col - adm_rct) / 30)
      )
  }
  
  conduct <- conduct %>%
    mutate(
      cndct_mnthly_pretreat_all = case_when(
        rct_treat_wave == 0 ~ cndct_mnthly_rand0_guilty,
        rct_treat_wave == 1 ~ cndct_mnthly_rand1_guilty,
        rct_treat_wave == 2 ~ cndct_mnthly_rand2_guilty,
        rct_treat_wave == 3 ~ cndct_mnthly_rand3_guilty,
        rct_treat_wave == 4 ~ cndct_mnthly_rand4_guilty,
        rct_treat_wave == 5 ~ cndct_mnthly_rand5_guilty,
        rct_treat_wave == 6 ~ cndct_mnthly_rand6_guilty,
        rct_treat_wave == 7 ~ cndct_mnthly_rand7_guilty,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup()
  
  return(conduct)
}
conduct <- add_pretreatment_guilty_rates(conduct,admission)

# ================================================================= ####
# Temporary Descriptive Stats ####
# -- number of misconducts per unique control number
# -- -- NOTE: individuals are only in this dataset if they have committed atleast
#             one misconduct, does not include anyone with 0 misconducts
misconducts_per_person <- conduct %>%
  group_by(research_id) %>%
  summarize(n_misconducts = n())
summary(misconducts_per_person$n_misconducts)

# -- likelihood of each verdict result
conduct %>%
  count(cndct_guilty) %>%
  mutate(percent = n / sum(n) * 100)

# -- most common misconduct charges
conduct %>%
  count(cndct_chrg_desc, sort = TRUE) %>%
  slice_head(n = 10)  # top 10
# ================================================================= ####
# Reorganize Variables ####
conduct <- reorder_vars(conduct)
# ================================================================= ####
# Save Dataframe ####
saveRDS(conduct, file = "data/processed/de_identified/2_conduct_cleaned.Rds")
# ================================================================= ####