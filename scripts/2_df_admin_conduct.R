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
admission <- readRDS("data/processed/de_identified/2b_admissions_rct.Rds")
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
  mutate(cndct_date = ymd(as_date(cndct_date))) %>%
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
# count by individual the number of misconduct incidents prior to the date of the first wave
# -- define wave dates
wave_dates <- tibble(
  wave = 1:7,
  wave_date = as.Date(c("2022-05-01", "2022-11-15", "2023-05-20",
                        "2023-11-28", "2024-06-06", "2024-10-22", "2025-05-01")))

# -- Get unique misconducts with date
misconducts <- conduct %>%
  select(research_id, cndct_num, cndct_date) %>%
  distinct() %>%
  mutate(cndct_date = as.Date(cndct_date))

# -- Cross join each misconduct with each wave, and keep those that happened BEFORE or ON the wave date
misconducts_expanded <- tidyr::crossing(misconducts, wave_dates) %>%
  filter(cndct_date <= wave_date)

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


# only the conducts in which people were found guilty
# -- Filter misconducts to those that were found guilty ("001")
guilty_misconducts <- conduct %>%
  filter(cndct_guilty == "1") %>%
  select(research_id, cndct_num, cndct_date) %>%
  distinct() %>%
  mutate(cndct_date = as.Date(cndct_date))

# -- Cross join with wave dates, keep those on or before wave date
guilty_expanded <- tidyr::crossing(guilty_misconducts, wave_dates) %>%
  filter(cndct_date <= wave_date)

# -- Count unique guilty misconducts per person per wave
guilty_counts <- guilty_expanded %>%
  group_by(research_id, wave) %>%
  summarise(cum_guilty = n_distinct(cndct_num), .groups = "drop") %>%
  mutate(varname = paste0("cndct_rand", wave, "_guilty")) %>%
  select(research_id, varname, cum_guilty) %>%
  pivot_wider(names_from = varname, values_from = cum_guilty)

# -- Join back into conduct
conduct <- conduct %>%
  left_join(guilty_counts, by = "research_id")

# guilt rate by month
# Steps:
# 0. add column to house with admit dates (this is a constant within individual)
# 1. merge in admit date from house (left_join on unique combinations of research_id and admit_date)
# 2. calculate number of months between admit date and date of rand1, rand2 etc. (doesnt need to be whole number, can be decimal)
# 3. divide the number of guilty incidents by number of month
# cndct_guilty_rand1_mnthly, etc.
# cndct_all_ran1_mnthly, etc.
# ================================================================= ####
# Merge in RCT Data + Calculate Conduct Rates Pre and Post Treatment ####
# 1. Merge rct data from df-admissions into df-conduct
# 2. Use rct_adm (latest admission prior to treatment) rct_treat_dt (the date that the person got treated) to # -- -- calculate misconduct rate prior to being treated. 
# -- -- Example: If someone got treated in May 2022, and their latest admission was in Jan 2020. We want to
# -- -- (1) calculate the number of misconducts between Jan 2020 and May 2022, (2) divide number of of misconducts by number of months between Jan 2020 and May 2022. 



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