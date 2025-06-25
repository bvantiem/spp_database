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

# cndct_mnthly_pretreat_all
# cndct_mnthly_pretreat_guilty
# cndct_mnthly_rand1_all
# cndct_mnthly_rand1_guilty
# cndct_rand1_all
# cndct_rand1_guilty

admission <- admission %>% 
  filter(rct %in% c(0,1)) %>%
  distinct(research_id, adm_rct, rct_treat_dt, rct_treat_wave)

conduct <- conduct %>%
  left_join(admission %>% select(research_id, adm_rct, rct_treat_wave), by = "research_id") %>%
  group_by(research_id) %>%
  #Rand0
  mutate(no_months_before_rand_date = as.numeric(rand1_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand0_all = cndct_rand0_all/no_months_before_rand_date) %>%
  #Rand1
  mutate(no_months_before_rand_date = as.numeric(rand1_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand1_all = cndct_rand1_all/no_months_before_rand_date) %>%
  #Rand2
  mutate(no_months_before_rand_date = as.numeric(rand2_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand2_all = cndct_rand2_all/no_months_before_rand_date) %>%
  #Rand3
  mutate(no_months_before_rand_date = as.numeric(rand3_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand3_all = cndct_rand3_all/no_months_before_rand_date) %>%
  #Rand4
  mutate(no_months_before_rand_date = as.numeric(rand4_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand4_all = cndct_rand4_all/no_months_before_rand_date) %>%
  #Rand5
  mutate(no_months_before_rand_date = as.numeric(rand5_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand5_all = cndct_rand5_all/no_months_before_rand_date) %>%
  #Rand6
  mutate(no_months_before_rand_date = as.numeric(rand6_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand6_all = cndct_rand6_all/no_months_before_rand_date) %>%
  #Rand7
  mutate(no_months_before_rand_date = as.numeric(rand7_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand7_all = cndct_rand7_all/no_months_before_rand_date) %>%
  #Drop helper column 
  select(-no_months_before_rand_date) %>%
  # Calculate before treatment rates 
  mutate(cndct_mnthly_pretreat_all = case_when(
    rct_treat_wave == 0 ~ cndct_mnthly_rand0_all,
    rct_treat_wave == 1 ~ cndct_mnthly_rand1_all,
    rct_treat_wave == 2 ~ cndct_mnthly_rand2_all,
    rct_treat_wave == 3 ~ cndct_mnthly_rand3_all,
    rct_treat_wave == 4 ~ cndct_mnthly_rand4_all,
    rct_treat_wave == 5 ~ cndct_mnthly_rand5_all,
    rct_treat_wave == 6 ~ cndct_mnthly_rand6_all,
    rct_treat_wave == 7 ~ cndct_mnthly_rand7_all,
  )) %>% 
  ungroup() 

# -- Misconduct Monthly Guilty Rate ####
conduct <- conduct %>%
  group_by(research_id) %>%
  #Rand0
  mutate(no_months_before_rand_date = as.numeric(rand1_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand0_guilty = cndct_rand0_guilty/no_months_before_rand_date) %>%
  #Rand1
  mutate(no_months_before_rand_date = as.numeric(rand1_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand1_guilty = cndct_rand1_guilty/no_months_before_rand_date) %>%
  #Rand2
  mutate(no_months_before_rand_date = as.numeric(rand2_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand2_guilty = cndct_rand2_guilty/no_months_before_rand_date) %>%
  #Rand3
  mutate(no_months_before_rand_date = as.numeric(rand3_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand3_guilty = cndct_rand3_guilty/no_months_before_rand_date) %>%
  #Rand4
  mutate(no_months_before_rand_date = as.numeric(rand4_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand4_guilty = cndct_rand4_guilty/no_months_before_rand_date) %>%
  #Rand5
  mutate(no_months_before_rand_date = as.numeric(rand5_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand5_guilty = cndct_rand5_guilty/no_months_before_rand_date) %>%
  #Rand6
  mutate(no_months_before_rand_date = as.numeric(rand6_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand6_guilty = cndct_rand6_guilty/no_months_before_rand_date) %>%
  #Rand7
  mutate(no_months_before_rand_date = as.numeric(rand7_date-adm_rct)/30) %>%
  mutate(cndct_mnthly_rand7_guilty = cndct_rand7_guilty/no_months_before_rand_date) %>%
  #Drop helper column 
  select(-no_months_before_rand_date) %>%
  # Calculate before treatment rates 
  mutate(cndct_mnthly_pretreat_guilty = case_when(
    rct_treat_wave == 0 ~ cndct_mnthly_rand0_guilty,
    rct_treat_wave == 1 ~ cndct_mnthly_rand1_guilty,
    rct_treat_wave == 2 ~ cndct_mnthly_rand2_guilty,
    rct_treat_wave == 3 ~ cndct_mnthly_rand3_guilty,
    rct_treat_wave == 4 ~ cndct_mnthly_rand4_guilty,
    rct_treat_wave == 5 ~ cndct_mnthly_rand5_guilty,
    rct_treat_wave == 6 ~ cndct_mnthly_rand6_guilty,
    rct_treat_wave == 7 ~ cndct_mnthly_rand7_guilty,
  )) %>% 
  ungroup() 
# ================================================================= ####
# Merge in RCT Data + Calculate Conduct Rates Pre and Post Treatment ####
# 1. Merge rct data from df-admissions into df-conduct
# 2. Use rct_adm (latest admission prior to treatment) rct_treat_dt (the date that the person got treated) to calculate misconduct rate prior to being treated. 
# -- -- Example: If someone got treated in May 2022, and their latest admission was in Jan 2020. We want to
# -- -- (1) calculate the number of misconducts between Jan 2020 and May 2022, (2) divide number of of misconducts by number of months between Jan 2020 and May 2022. 
# 1. Merge rct data from df-admissions into df-conduct
conduct <- conduct %>%
  select(-adm_rct) %>%  # drop to prevent conflict
  left_join(admission %>% select(research_id, adm_rct, rct_treat_dt), by = "research_id") %>%
  mutate(
    adm_rct = as.Date(adm_rct),
    rct_treat_dt = as.Date(rct_treat_dt),
    cndct_date = as.Date(cndct_date)
  )

# 2. Flag each midsonduct as pre- or post-treatment
conduct <- conduct %>%
  mutate(
    misconduct_phase = case_when(
      !is.na(adm_rct) & !is.na(rct_treat_dt) & cndct_date >= adm_rct & cndct_date < rct_treat_dt ~ "pre",
      !is.na(rct_treat_dt) & cndct_date >= rct_treat_dt ~ "post",
      TRUE ~ NA_character_
    )
  )

# 3. Count pre/post misconducts per person
misconduct_counts <- conduct %>%
  filter(misconduct_phase %in% c("pre", "post")) %>%
  group_by(research_id, misconduct_phase) %>%
  summarise(misconducts = n_distinct(cndct_num), .groups = "drop")

# 4. Expand so every treated person has both pre/post rows
treated_ids <- conduct %>%
  filter(!is.na(rct_treat_dt)) %>%
  distinct(research_id)

phases <- tibble(misconduct_phase = c("pre", "post"))

all_treated_combos <- tidyr::crossing(treated_ids, phases)

misconduct_counts_full <- all_treated_combos %>%
  left_join(misconduct_counts, by = c("research_id", "misconduct_phase")) %>%
  mutate(misconducts = replace_na(misconducts, 0))

# 5. Pivot into wide format
misconduct_wide <- misconduct_counts_full %>%
  pivot_wider(
    names_from = misconduct_phase,
    values_from = misconducts
  )

# 6. Calculate time periods in months
treatment_windows <- admission %>%
  select(research_id, adm_rct, rct_treat_dt) %>%
  mutate(
    months_pre = as.numeric(difftime(rct_treat_dt, adm_rct, units = "days")) / 30.44
  )

# -- use date_datapull for post time period
latest_date <- max(as.Date(conduct$date_datapull), na.rm = TRUE)

treatment_windows <- treatment_windows %>%
  mutate(
    months_post = as.numeric(difftime(latest_date, rct_treat_dt, units = "days")) / 30.44
  )

# 7. Merge misconduct counts and compute rates
misconduct_wide <- misconduct_counts %>%
  pivot_wider(
    names_from = misconduct_phase,
    values_from = misconducts,
    values_fill = list(misconducts = 0) # fills any cases with no post misconducts with 0 instead of NA
  )

# Merge everything
rct_rates <- treatment_windows %>%
  left_join(misconduct_wide, by = "research_id") %>%
  mutate(
    rct_pre_rate = ifelse(is.na(months_pre) | months_pre <= 0, NA, pre / months_pre),
    rct_post_rate = ifelse(
      is.na(months_post) | months_post <= 0, NA,
      post / months_post
    )
  )

# 8. Join pre/post rates into df-conduct
conduct <- conduct %>%
  left_join(rct_rates %>% select(research_id, rct_pre_rate, rct_post_rate), by = "research_id")

conduct <- conduct %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
# -- Temporary Descriptive Stats ####
temp <- conduct %>%
  mutate(rate_diff = rct_post_rate - rct_pre_rate)

# Summary of change
temp %>%
  summarise(
    avg_change = mean(rate_diff, na.rm = TRUE),
    median_change = median(rate_diff, na.rm = TRUE),
    improved = sum(rate_diff < 0, na.rm = TRUE),
    worsened = sum(rate_diff > 0, na.rm = TRUE),
    no_change = sum(rate_diff == 0, na.rm = TRUE)
  )
# -- Temporary Debug Code ####
rct_rates %>%
  filter(is.na(rct_pre_rate) | is.na(rct_post_rate)) %>%
  mutate(
    reason = case_when(
      is.na(months_pre) | is.na(months_post) ~ "missing months",
      months_pre <= 0 | months_post <= 0 ~ "nonpositive time window",
      is.na(pre) | is.na(post) ~ "missing counts",
      TRUE ~ "other"
    )
  ) %>%
  count(reason)
# -- Misconduct for Treated df ####
conduct_treated <- conduct %>%
  filter(!is.na(rct_treat_dt)) %>%
  select(rct_treat_dt, rct_pre_rate, rct_post_rate, wave)
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