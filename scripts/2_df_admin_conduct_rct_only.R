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
# 1. pre treatment counts and rates by misconduct category
# 5. take similar approach to existing code in calculating numbers and rates but this time by most serious category
# 6. columns names would be "cndct_pretreat_all", "cndct_pretreat_guilty", "cndct_pretreat_all_mnthly", "cndct_pretreat_guilty_mnthly",
# -- new col names: "cndct_pretreat_all_a", "cndct_pretreat_guilty_a", etc.
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
# Merge in treatment status and treatment date and subset to rct sample ####
admission <- admission %>% 
  filter(rct %in% c(0,1)) %>%
  distinct(research_id, adm_rct, rct_treat_dt, rct_treat_wave, rct_stratum, rct)

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
# -- Misconduct Count by Individual ####
# count by individual the number of misconduct incidents prior to the date of the first wave
# count based on misconduct number meaning there could be multiple charges but only counted as one
# misconduct
# -- define wave dates

rand_dates <- tibble(
  wave = c(0,1,2,2.5,3,4,5,6,7),
  rand_date = as.Date(c(rand1_date, rand1_date, rand2_date, 
                        rand2.5_date,
                        rand3_date, 
                        rand4_date, rand5_date, rand6_date, rand7_date)))

# -- Get unique misconducts with date
# -- -- count based on misconduct number meaning there could be multiple charges but only counted as one
misconducts <- conduct_rct %>%
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
conduct_rct <- conduct_rct %>%
  left_join(cumulative_counts, by = "research_id")


conduct_rct <- conduct_rct %>%
  mutate(
    cndct_pretreat_all = case_when(
      rct_treat_wave == 0 ~ cndct_rand0_all,
      rct_treat_wave == 1 ~ cndct_rand1_all,
      rct_treat_wave == 2 ~ cndct_rand2_all,
      rct_treat_wave == 2.5 ~ cndct_rand2.5_all,
      rct_treat_wave == 3 ~ cndct_rand3_all,
      rct_treat_wave == 4 ~ cndct_rand4_all,
      rct_treat_wave == 5 ~ cndct_rand5_all,
      rct_treat_wave == 6 ~ cndct_rand6_all,
      rct_treat_wave == 7 ~ cndct_rand7_all,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# -- Guilty Misconduct Count by Individual ####
# only the conducts in which people were found guilty
# -- Filter misconducts to those that were found guilty ("001")
guilty_misconducts <- conduct_rct %>%
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
conduct_rct <- conduct_rct %>%
  select(-starts_with("cndct_rand")) %>%
  left_join(cumulative_counts, by = "research_id") %>%
  left_join(guilty_counts, by = "research_id")

conduct_rct <- conduct_rct %>%
  rename(cndct_rand2.5_guilty = cndct_rand2.5)

conduct_rct <- conduct_rct %>%
  mutate(
    cndct_pretreat_guilty = case_when(
      rct_treat_wave == 0 ~ cndct_rand0_guilty,
      rct_treat_wave == 1 ~ cndct_rand1_guilty,
      rct_treat_wave == 2 ~ cndct_rand2_guilty,
      rct_treat_wave == 2.5 ~ cndct_rand2.5_guilty,
      rct_treat_wave == 3 ~ cndct_rand3_guilty,
      rct_treat_wave == 4 ~ cndct_rand4_guilty,
      rct_treat_wave == 5 ~ cndct_rand5_guilty,
      rct_treat_wave == 6 ~ cndct_rand6_guilty,
      rct_treat_wave == 7 ~ cndct_rand7_guilty,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


# -- Misconduct Monthly Rate ####
# Calculate monthly misconduct rates before treatment using rct_treat_wave

# 1. Loop through waves 0–7 + 2.5 to create monthly misconduct rate variables
for (i in c(0, 1, 2, 2.5, 3, 4, 5, 6, 7)) {
  date_col <- sym(paste0("rand", i, "_date"))
  count_col <- sym(paste0("cndct_rand", i, "_all"))
  rate_col  <- paste0("cndct_mnthly_rand", i, "_all")
  
  conduct_rct <- conduct_rct %>%
    mutate(
      !!rate_col := !!count_col / (as.numeric(!!date_col - adm_rct) / 30)
    )
}

# 2. Create single pre-treatment misconduct rate column based on rct_treat_wave
conduct_rct <- conduct_rct %>%
  mutate(
    cndct_mnthly_pretreat_all = case_when(
      rct_treat_wave == 0   ~ cndct_mnthly_rand0_all,
      rct_treat_wave == 1   ~ cndct_mnthly_rand1_all,
      rct_treat_wave == 2   ~ cndct_mnthly_rand2_all,
      rct_treat_wave == 2.5 ~ cndct_mnthly_rand2.5_all,
      rct_treat_wave == 3   ~ cndct_mnthly_rand3_all,
      rct_treat_wave == 4   ~ cndct_mnthly_rand4_all,
      rct_treat_wave == 5   ~ cndct_mnthly_rand5_all,
      rct_treat_wave == 6   ~ cndct_mnthly_rand6_all,
      rct_treat_wave == 7   ~ cndct_mnthly_rand7_all,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# -- Misconduct Monthly Guilty Rate ####
# Calculate monthly guilty rates before treatment using rct_treat_wave

# 1. Loop through waves 0–7 + 2.5 to create monthly guilty rate variables
for (i in c(0,1,2,2.5,3,4,5,6,7)) {
  date_col <- sym(paste0("rand", i, "_date"))
  count_col <- sym(paste0("cndct_rand", i, "_guilty"))
  rate_col  <- paste0("cndct_mnthly_rand", i, "_guilty")
    
  conduct_rct <- conduct_rct %>%
    mutate(
      !!rate_col := !!count_col / (as.numeric(!!date_col - adm_rct) / 30) 
    )
}
 
# 2. Create single pre-treatment guilty rate column based on rct_treat_wave 
conduct_rct <- conduct_rct %>%
  mutate(
    cndct_mnthly_pretreat_guilty = case_when(
      rct_treat_wave == 0 ~ cndct_mnthly_rand0_guilty,
      rct_treat_wave == 1 ~ cndct_mnthly_rand1_guilty,
      rct_treat_wave == 2 ~ cndct_mnthly_rand2_guilty,
      rct_treat_wave == 2.5 ~ cndct_mnthly_rand2.5_guilty,
      rct_treat_wave == 3 ~ cndct_mnthly_rand3_guilty,
      rct_treat_wave == 4 ~ cndct_mnthly_rand4_guilty,
      rct_treat_wave == 5 ~ cndct_mnthly_rand5_guilty,
      rct_treat_wave == 6 ~ cndct_mnthly_rand6_guilty,
      rct_treat_wave == 7 ~ cndct_mnthly_rand7_guilty,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()



# Keep only the pretreatment columns and cndct_guilty ####
# Vector of column names you want to subset from
vars_to_consider <- grep("all|guilty", names(conduct_rct), value = TRUE)

# Filter only those that contain '_pretreat_'
vars_to_keep <- vars_to_consider[grepl("_pretreat_|cndct_guilty", vars_to_consider)]

# Drop all vars_to_consider except those in vars_to_keep
conduct_rct <- conduct_rct |>
  select(-all_of(setdiff(vars_to_consider, vars_to_keep)))

# Set all NA values to zero
conduct_rct[is.na(conduct_rct)] <- 0

# -- Pretreatment Counts/Rates by Misconduct Category ####
# -- -- NA for created pretreatment vars is due to having 0 pretreat misconducts (should these be handled differently?)
# 1. create new column with the highest cndct_charge_cat per misconduct number ("cndct_charge_cat_most_serious")
conduct_rct <- conduct_rct %>%
  group_by(research_id, cndct_num) %>%
  # -- build new column with the lowest alphabetical letter (aka highest charge cat)
  mutate(cndct_charge_cat_most_serious = min(cndct_chrg_cat, na.rm = TRUE)) %>%
  ungroup()
# 2. create another column with the highest cndct_charge_cat found guilty of per misconduct number ("cndct_charge_cat_most_serious_guilty")
conduct_rct <- conduct_rct %>%
  group_by(research_id, cndct_num) %>%
  mutate(
    cndct_charge_cat_most_serious_guilty = if (any(cndct_guilty == "1")) {
      min(cndct_chrg_cat[cndct_guilty == "1"], na.rm = TRUE)
    } else {
      NA
    }
  ) %>%
  ungroup()

# 3. filter to pre-treatment charges
temp <- conduct_rct %>%
  filter(!is.na(cndct_chrg_cat), cndct_date >= adm_rct, cndct_date < rct_treat_dt)

# 4. count charges per misconduct event per category
pretreat_counts <- temp %>%
  group_by(research_id, cndct_num, cndct_chrg_cat) %>%
  summarise(n_charges = n(), .groups = "drop")

# 5. Aggregate up to research_id level to get total A, B, C charges pre-treatment
charges_by_person_cat <- pretreat_counts %>%
  group_by(research_id, cndct_chrg_cat, cndct_num) %>%
  summarise(cndct_pretreat_all_count = sum(n_charges), .groups = "drop")

# 6. get months pre-treatment per person
pretreat_window <- conduct_rct %>%
  distinct(research_id, adm_rct, rct_treat_dt, cndct_num) %>%
  mutate(months_pre = as.numeric(difftime(rct_treat_dt, adm_rct, units = "days")) / 30.44)

# 7. join and calculate rates
pretreat_rates <- charges_by_person_cat  %>%
  left_join(pretreat_window, by = "cndct_num") %>%
  mutate(cndct_pretreat_all_rate = cndct_pretreat_all_count / months_pre)

# 8. pivot wider so each category becomes its own column
pretreat_rates_wide <- pretreat_rates %>%
  select(cndct_num, cndct_chrg_cat, cndct_pretreat_all_count, cndct_pretreat_all_rate) %>%
  rename(count = cndct_pretreat_all_count, rate = cndct_pretreat_all_rate) %>%
  pivot_wider(
    names_from = cndct_chrg_cat,
    values_from = c(count, rate),
    # if there are no misconducts fill with a 0
    values_fill = 0,
    names_glue = "cndct_pretreat_all_{.value}_{tolower(cndct_chrg_cat)}"
  )

# 9. combine
conduct_rct <- conduct_rct %>%
  left_join(pretreat_rates_wide, by = "cndct_num")

# -- Pretreatment Counts/Rates by Guilty Misconduct Category ####
# 1. filter only pre-treatment guilty charges
temp <- conduct_rct %>%
  filter(
    !is.na(cndct_chrg_cat),
    cndct_date >= adm_rct, cndct_date < rct_treat_dt,
    cndct_guilty == "1"
  )

# 2. count guilty charges per misconduct event
pretreat_counts <- temp %>%
  group_by(research_id, cndct_num, cndct_chrg_cat) %>%
  summarise(n_charges = n(), .groups = "drop")

# 3. summarize by cndct_num and category
charges_by_person_cat_guilty <- pretreat_counts %>%
  group_by(cndct_num, cndct_chrg_cat) %>%
  summarise(cndct_pretreat_guilty_count = sum(n_charges), .groups = "drop")

# 4. join and calculate guilty rate
pretreat_rates <- charges_by_person_cat_guilty %>%
  left_join(pretreat_window, by = "cndct_num") %>%
  mutate(cndct_pretreat_guilty_rate = cndct_pretreat_guilty_count / months_pre)

# 5. pivot wider 
pretreat_rates_wide <- pretreat_rates %>%
  select(cndct_num, cndct_chrg_cat, cndct_pretreat_guilty_count, cndct_pretreat_guilty_rate) %>%
  rename(count = cndct_pretreat_guilty_count, rate = cndct_pretreat_guilty_rate) %>%
  pivot_wider(
    names_from = cndct_chrg_cat,
    values_from = c(count, rate),
    values_fill = 0,
    names_glue = "cndct_pretreat_guilty_{.value}_{tolower(cndct_chrg_cat)}"
  )

# 6. join back to df
conduct_rct <- conduct_rct %>%
  left_join(pretreat_rates_wide, by = "cndct_num")
# ================================================================= ####
# Temporary Descriptive Stats Britte ####

temp <- conduct_rct %>%
  distinct(research_id, rct, rct_stratum, cndct_pretreat_all,
           cndct_pretreat_guilty, cndct_mnthly_pretreat_all, cndct_mnthly_pretreat_guilty)

summary(temp$cndct_mnthly_pretreat_all[temp$rct==1])
summary(temp$cndct_mnthly_pretreat_all[temp$rct==0])

# ================================================================= ####
# Temporary Descriptive Stats Gabby ####
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