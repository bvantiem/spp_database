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
# -- Misconduct Count by Individual ####
# count by individual the number of misconduct incidents prior to the date of the first wave
# count based on misconduct number meaning there could be multiple charges but only counted as one
# misconduct
# -- define wave dates

wave_dates <- tibble(
  wave = 1:7,
  wave_date = as.Date(c(wave1_date, wave2_date, wave3_date, wave4_date,
                        wave5_date, wave6_date, wave7_date)))

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

# -- Guilty Misconduct Count by Individual ####
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

# -- Guilt Rate by Month ####
# guilt rate by month
# Steps:
# 1. merge in admit date from house (left_join on unique combinations of research_id and admit_date)
conduct <- conduct %>%
  left_join(admission %>% select(research_id, adm_rct), by = "research_id")
# 2. longform table of guilty counts
guilty_long <- conduct %>%
  select(research_id, starts_with("cndct_rand"), ends_with("_guilty"), adm_rct) %>%
  pivot_longer(
    cols = matches("^cndct_rand[1-7]_guilty$"),
    names_to = "wave_label",
    values_to = "guilty_count"
  ) %>%
  mutate(
    wave = as.integer(gsub("\\D+", "", wave_label))
  ) %>%
  left_join(wave_dates, by = "wave") %>%
  arrange(research_id, wave)
# 3. compute monthly rates
guilty_long <- guilty_long %>%
  group_by(research_id) %>%
  arrange(wave) %>%
  mutate(
    adm_rct = as.Date(adm_rct),
    
    # months since admit (used for wave 1 only)
    months_since_admit = as.numeric(difftime(wave_date, adm_rct, units = "days")) / 30.44,
    
    # difference in cumulative count between waves
    prior_count = lag(guilty_count),
    prior_date = lag(wave_date),
    new_guilty = guilty_count - coalesce(prior_count, 0),
    
    months_between = as.numeric(difftime(wave_date, prior_date, units = "days")) / 30.44,
    
    # Final guilty rate: use months_since_admit for wave 1, months_between otherwise
    guilty_rate = case_when(
      wave == 1 ~ ifelse(is.na(months_since_admit), NA, guilty_count / months_since_admit),
      months_between > 0 ~ new_guilty / months_between,
      TRUE ~ NA
    ),
    
    wave_label = paste0("cndct_rand", wave, "_guilty_mnthly")
  ) %>%
  ungroup()
# 4. pivot back and join
guilty_rate_wide <- guilty_long %>%
  group_by(research_id, wave_label) %>%
  summarise(guilty_rate = mean(guilty_rate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = wave_label, values_from = guilty_rate)

conduct <- conduct %>%
  left_join(guilty_rate_wide, by = "research_id")
# -- Misconduct Rate by Month ####
# rate for all misconducts, not just guilty
# -- Pivot cumulative misconduct counts into long format
all_long <- conduct %>%
  select(research_id, starts_with("cndct_rand"), ends_with("_all"), adm_rct) %>%
  pivot_longer(
    cols = matches("^cndct_rand[1-7]_all$"),
    names_to = "wave_label",
    values_to = "all_count"
  ) %>%
  mutate(
    wave = as.integer(gsub("\\D+", "", wave_label))  # extract wave number from label
  ) %>%
  left_join(wave_dates, by = "wave") %>%
  arrange(research_id, wave)

# -- Calculate monthly rate
all_long <- all_long %>%
  group_by(research_id) %>%
  arrange(wave) %>%
  mutate(
    adm_rct = as.Date(adm_rct),
    
    # For wave 1: time since admission
    months_since_admit = as.numeric(difftime(wave_date, adm_rct, units = "days")) / 30.44,
    
    # For wave 2–7: time between waves
    prior_count = lag(all_count),
    prior_date = lag(wave_date),
    new_all = all_count - coalesce(prior_count, 0),
    months_between = as.numeric(difftime(wave_date, prior_date, units = "days")) / 30.44,
    
    # Final rate logic
    all_rate = case_when(
      wave == 1 ~ ifelse(is.na(months_since_admit), NA, all_count / months_since_admit),
      months_between > 0 ~ new_all / months_between,
      TRUE ~ NA
    ),
    
    wave_label = paste0("cndct_rand", wave, "_all_mnthly")
  ) %>%
  ungroup()

# -- Pivot wide and join back into conduct
all_rate_wide <- all_long %>%
  group_by(research_id, wave_label) %>%
  summarise(all_rate = mean(all_rate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = wave_label, values_from = all_rate)

conduct <- conduct %>%
  left_join(all_rate_wide, by = "research_id")
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