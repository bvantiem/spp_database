# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Clean conduct data 
# -- Readme ####
# -- To do ####
# -- -- Follow up with PADOC on cat_chrge1 & vrdict_guilty meaning
# Referencing the excel sheet cat_chrge1 looks to be levels of seriousness of offense
# but want to confirm with PADOC
# clarify PNG institution location - very common in this dataset haven't observed it in others
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
conduct <- readRDS("data/processed/1b_conduct_masked.Rds")

# ================================================================= ####
# Rename Raw Variables ####
# Append _raw to all columns except specified columns
conduct <- conduct |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(conduct), c("research_id","date_datapull", "control_number", "wave"))) %>%
  mutate(cndct_num = misconduct_number_raw,
         cndct_date = misconduct_date_raw,
         cndct_chrg_desc = chrg_description_raw,
         cndct_guilty = vrdict_guilty_raw) %>%
  mutate(pris_loc = institution_raw) %>%
  relocate(ends_with("_raw"), .after = last_col())
# Clean Variables ####
conduct <- conduct %>%
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) %>%
  # DATES
  mutate(cndct_date = ymd(as_date(cndct_date))) %>%
  # MISCONDUCT
  mutate(cndct_chrg_desc = standardize_uppercase(cndct_chrg_desc)) %>%
  # -- some entries for cndct_guilty have 2 leading zeros, drop these for standardization
  mutate(cndct_guilty = sub("^00", "", cndct_guilty)) %>%
  # PRISON
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(pris_loc, .after = cndct_guilty) %>%
  relocate(date_datapull, .after = pris_loc) %>%
  relocate(wave, .after = date_datapull)
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
# ================================================================= ####
# Temporary Descriptive Stats ####
# -- number of misconducts per unique control number
# -- -- NOTE: individuals are only in this dataset if they have committed atleast
#             one misconduct, does not include anyone with 0 misconducts
misconducts_per_person <- conduct %>%
  group_by(control_number) %>%
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
# Save Dataframe ####
saveRDS(conduct, file = "data/processed/2_conduct_cleaned.Rds")
# ================================================================= ####