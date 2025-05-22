# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Clean basic demographic data 
# -- Readme ####
# Level of observation: individual*date_datapull
# This may eventually need to become a dataset at the individual*survey participation level
# While most variables in this file are static - some change over the course of a person's incarceration
# Some individuals have multiple asca codes - meaning this reflects multiple admissions
# -- To do ####
# To do: ASCA classification is sometimes NULL while the offense code is the same as that of other variables, e.g "STRANGULATION: APPLYING PRESSURE TO THROAT OR NECK" is sometimes classified as violent and sometimes as NULL. Manually recategorize.

# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions ####
# Data cleaning tools 
assess_variable <- function(x) {
  # Check data type
  data_type <- class(x)

  # Determine range if numeric
  value_range <- if (is.numeric(x)|is.Date(x)) range(x, na.rm = TRUE) else NA

  # Count unique values
  unique_values <- unique(x)
  unique_count <- length(unique_values)

  # Get up to the first 100 unique values
  unique_sample <- head(unique_values, 100)

  # Table the number of characters in a variable
  nchar_table <- if(is.character(x)) table(nchar(x), useNA = "always") else NA

  # Count NA values
  na_count <- sum(is.na(x))

  # Count NaN values
  nan_count <- sum(is.nan(x))

  # Count empty values ("" for character variables)
  empty_count <- if (is.character(x)) sum(grepl("^\\s*$", x), na.rm = TRUE) else 0

  # Return results as a list
  result <- list(
    data_type = data_type,
    value_range = value_range,
    unique_count = unique_count,
    unique_sample = unique_sample,
    nchar_table = nchar_table,
    na_count = na_count,
    nan_count = nan_count,
    empty_count = empty_count
  )

  return(result)
}
remove_leading_zeros <- function(x) {
  # Remove leading zeros
  cleaned_x <- sub("^0+", "", x)

  # If a value contained only zeros (now an empty string), replace with "0"
  cleaned_x[grepl("^0*$", x)] <- "0"

  return(cleaned_x)
}

# -- Read in Data ####
basic <- readRDS("data/processed/processing_layer_2/basic_masked.Rds")

# ================================================================= ####
# Clean existing dataset
# -- Rename raw variables ####
# Append _raw to all columns except "research_id"
basic <- basic |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(basic), c("research_id","date_datapull", "control_number_pull")))

# Rename columns and put them in order
basic <- basic %>%
  mutate(sent_class = class_of_sent_raw,
         sent_status = sentence_status_raw,
         sent_min_cort_yrs = min_cort_sent_yrs_raw,
         sent_min_cort_mths = min_cort_sent_mths_raw,
         sent_min_cort_days = min_cort_sent_days_raw,
         sent_max_cort_yrs = max_cort_sent_yrs_raw,
         sent_max_cort_mths = max_cort_sent_mths_raw,
         sent_max_cort_days = max_cort_sent_days_raw,
         sent_min_expir_dt = min_expir_date_raw,
         sent_max_expir_dt = max_expir_date_raw,
         sent_max_expir_recmp_dt = RecmpMax_Dt_raw,
         sent_commitment_cnty = cnty_name_raw,
         sent_cust_lev = custody_raw,
         sent_off_asca = `ASCA Category - Ranked_raw`)%>%
  mutate(chg_off_code = offense_code_raw,
         chg_des = offense_raw) %>%
  mutate(inc_pris = location_permanent_raw) %>%
  mutate(dem_dob_dt = date_of_birth_raw,
         dem_race = race_code_raw,
         dem_marital = marital_status_code_raw,
         dem_edu_grade = grade_complete_raw,
         dem_mhcode = MHCode_raw,
         dem_stg_yes = STG_raw) %>%
  relocate(ends_with("_raw"), .after = last_col()) 

# -- Clean variables ####
cols_cort <- c("sent_min_cort_yrs",
               "sent_min_cort_mths",
               "sent_min_cort_days",
               "sent_max_cort_yrs",
               "sent_max_cort_mths",
               "sent_max_cort_days")

cols_expir <- c("sent_min_expir_dt",
                "sent_max_expir_dt")

basic <- basic %>%
  # Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) %>%
  # SENTENCING DATES
  mutate(across(all_of(cols_cort), ~ remove_leading_zeros(.) |> as.numeric())) %>%
  # -- Min and Max expiry dates are set to 0, 000, 00000000 etc. when individuals are sentenced to life.
  # -- But max dates are sometimes recomputed for life sentenced individuals
  mutate(across(all_of(c(cols_cort, cols_expir)), ~ ifelse(sent_class == "LF", NA, .))) %>%
  mutate(across(all_of(cols_expir), ymd)) %>%
  mutate(sent_max_expir_recmp_dt = ymd(as_date(sent_max_expir_recmp_dt))) %>%
  mutate(sent_class = case_when(
    sent_class == "CL" ~ "Commuted Life",
    sent_class == "DF" ~ "Definite",
    sent_class == "DT" ~ "Detention",
    sent_class == "IN" ~ "Indeterminate",
    sent_class == "LF" ~ "Life",
    TRUE ~ sent_class)) %>%
  mutate(sent_off_asca = case_when(
    sent_off_asca == "1-Violent" ~ "Violent",
    sent_off_asca == "VIOLENT" ~ "Violent",
    sent_off_asca == "2-Property" ~ "Property",
    sent_off_asca == "PROPERTY" ~ "Property",
    sent_off_asca == "3-Drugs" ~ "Drugs",
    sent_off_asca == "DRUGS" ~ "Drugs",
    sent_off_asca == "4-Public Order" ~ "Public Order",
    sent_off_asca == "PUBORDER" ~ "Public Order",
    sent_off_asca == "9-Not An Arrest" ~ "Not An Arrest",
    TRUE ~ sent_off_asca)) %>%
  # Remove trailing white space
  mutate(sent_commitment_cnty = gsub("\\s+$", "", sent_commitment_cnty)) %>%
  # DEMOGRAPHICS
  mutate(dem_dob_dt = ymd(dem_dob_dt)) %>%
  mutate(dem_race = case_when(
    dem_race == "A" ~ "Asian",
    dem_race == "B" ~ "Black",
    dem_race == "O" ~ "Other",
    dem_race == "W" ~ "White",
    dem_race == "I" ~ "American Indian",
    TRUE ~ dem_race)) %>%
    mutate(dem_marital = case_when(
      # NA values in marital_status_code_raw are listed as Unknown in marital_status_raw. 
      # Code ensures that both are recorded as "Unknown".
      marital_status_code_raw == "UNK" | marital_status_raw == "UNKNOWN" ~ "Unknown",
      marital_status_code_raw == "MAR" ~ "Married",
      marital_status_code_raw == "DIV" ~ "Divorced",
      marital_status_code_raw == "SIN" ~ "Single",
      marital_status_code_raw == "SEP" ~ "Separated",
      marital_status_code_raw == "WID" ~ "Widow",
      is.na(marital_status_code_raw) & !is.na(marital_status_raw) ~ marital_status_raw,
      TRUE ~ NA_character_
    )) %>%
  mutate(dem_edu_grade = remove_leading_zeros(dem_edu_grade) |> as.numeric()) %>%
  mutate(dem_mhcode = gsub("\\s+$", "", dem_mhcode)) %>%
  mutate(dem_stg_yes = as.numeric(dem_stg_yes))

# clean offense_raw to standardize capitalization of words.
# save these acronyms to ensure they stay capitalized
acronyms <- c("DUI", "IDSI")
standardize_offense <- function(x) {
  x <- x %>%
    str_squish() %>%
    str_replace_all("\\s*-\\s*", " - ") %>%                         # Normalize dash spacing
    str_replace_all("\\bMoveable\\b", "Movable") %>%               # Fix spelling
    str_replace_all("Mdse", "Merchandise") %>%                     # Expand abbreviation
    str_to_title()
  
  # Preserve acronyms (re-insert after title case)
  for (acro in acronyms) {
    pattern <- paste0("\\b", str_to_title(acro), "\\b")
    x <- str_replace_all(x, pattern, acro)
  }
  
  # Custom replacements
  x <- x %>%
    str_replace_all("^Murder\\s*-\\s*(1st|2nd|3rd) Degree$", "Murder (\\1 Degree)") %>%
    str_replace_all("^Person Not To Possess[, ]*Use[, ]*Etc\\.?[, ]*Firearms$", 
                    "Person Not To Possess, Use, Etc. Firearms")
  
  return(x)
}

# Apply to dataframe
basic <- basic %>%
  mutate(offense_raw = standardize_offense(offense_raw))
# -- Add Notes to Variables ####
  # to view notes added use str() or comment()
# -- -- Cleaned Variables ####
comment(basic$sent_class) <- "5 NA values, unknown cause for this otherwise cleaned variable"
comment(basic$sent_status) <- "no missing values, fully cleaned variable"
comment(basic$sent_min_cort_yrs) <- "318 missing values explained by those serving life, fully cleaned variable"
comment(basic$sent_min_cort_mths) <- "318 missing values explained by those serving life, fully cleaned variable"
comment(basic$sent_min_cort_days) <- "318 missing values explained by those serving life, fully cleaned variable"
comment(basic$sent_max_cort_yrs) <- "318 missing values explained by those serving life, fully cleaned variable"
comment(basic$sent_max_cort_mths) <- "318 missing values explained by those serving life, fully cleaned variable"
comment(basic$sent_max_cort_days) <- "318 missing values explained by those serving life, fully cleaned variable"
comment(basic$sent_min_expir_dt) <- "322 missing values" #what could be the reason for this?
comment(basic$sent_max_expir_dt) <- "359 missing values" #what could be the reason for this?
comment(basic$sent_max_expir_recmp_dt) <- "4577 missing values" #what could be the reason for this?
comment(basic$sent_commitment_cnty) <- "city of offense, fully cleaned variable"
comment(basic$sent_cust_lev) <- "161 missing values" #do we know why this could be?
comment(basic$sent_off_asca) <- "4 missing values, not fully cleaned variable"
comment(basic$chg_off_code) <- "9 missing values, not fully cleaned variable"
comment(basic$chg_des) <- "9 missing values, not fully cleaned need to fix capitalization"
comment(basic$inc_pris) <- "no missing values, fully cleaned"
comment(basic$dem_dob_dt) <- "no missing values, fully cleaned"
comment(basic$dem_race) <- "no missing values, fully cleaned variable"
comment(basic$dem_marital) <- "no missing values, fully cleaned variable"
comment(basic$dem_edu_grade) <- "3 NA values, fully cleaned variable"
comment(basic$dem_mhcode) <- "5 null values, fully cleaned variable"
comment(basic$dem_stg_yes) <- "2162 NA values, unknown cause for this"
# -- -- Raw Variables ####
comment(basic$location_permanent_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$delete_date_raw) <- "uncleaned variable, available only in raw form"
comment(basic$sentence_status_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$custody_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$temp_custody_raw)
comment(basic$class_of_sent_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$sentence_class_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$min_cort_sent_yrs_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$min_cort_sent_mths_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$min_cort_sent_days_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$max_cort_sent_yrs_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$max_cort_sent_mths_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$max_cort_sent_days_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$commit_cnty_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$cnty_name_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$offense_code_raw) <- "raw data, non raw variable avail still being cleaned"
comment(basic$offense_raw) <- "raw data, non raw variable avail still being cleaned"
comment(basic$`ASCA Category - Ranked_raw`) <- "raw data, cleaned non raw variable avail"
comment(basic$min_expir_date_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$max_expir_date_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$RecmpMax_Dt_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$date_of_birth_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$race_code_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$race_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$sex_type_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$sex_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$marital_status_code_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$marital_status_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$grade_complete_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$MHCode_raw) <-"raw data, cleaned non raw variable avail"
comment(basic$STG_raw) <- "raw data, cleaned non raw variable avail"
comment(basic$wave_raw) <- "no cleaned non raw variable currently"

# ================================================================= ####

# New Variables ####
basic <- basic %>%
  mutate(dem_edu_high_school = ifelse(dem_edu_grade>=12, 1,0)) %>%
  relocate(dem_edu_high_school, .after = dem_edu_grade) %>%
  mutate(
    dem_age_wave1 = decimal_date(ymd(20220501))-decimal_date(dem_dob_dt),
    dem_age_wave2 = decimal_date(ymd(20221115))-decimal_date(dem_dob_dt),
    dem_age_wave3 = decimal_date(ymd(20230520))-decimal_date(dem_dob_dt),
    dem_age_wave4 = decimal_date(ymd(20231128))-decimal_date(dem_dob_dt),
    dem_age_wave5 = decimal_date(ymd(20240606))-decimal_date(dem_dob_dt)) %>%
  relocate(starts_with("dem_age_wave"), .after = dem_dob_dt) %>%
  mutate(
    sent_days_to_min_wave1 = sent_min_expir_dt - ymd(20220501),
    sent_days_to_min_wave2 = sent_min_expir_dt - ymd(20221115),
    sent_days_to_min_wave3 = sent_min_expir_dt - ymd(20230520),
    sent_days_to_min_wave4 = sent_min_expir_dt - ymd(20231128),
    sent_days_to_min_wave5 = sent_min_expir_dt - ymd(20240606)) %>%
  relocate(starts_with("sent_days_to_min_wave"), .after = sent_max_expir_recmp_dt) %>%
  mutate(
    sent_min_in_days = (sent_min_cort_yrs*365)+(sent_min_cort_mths*31)+sent_min_cort_days,
    sent_max_in_days = (sent_max_cort_yrs*365)+(sent_max_cort_mths*31)+sent_max_cort_days)
# ================================================================= ####
# Save dataframe ####
saveRDS(basic, file = "data/processed/processing_layer_3/basic_cleaned.Rds")
        
# ================================================================= ####
# RELEVANT OLD CODE TO INTEGRATE LATER, INCL TIME SERVED ####

# Merge in age at treatment
basic$age_at_treatment <- with(basic, ifelse(treatment_wave==1, age_wave1,
                                             ifelse(treatment_wave==2, age_wave2,
                                                    ifelse(treatment_wave==3, age_wave3,
                                                           ifelse(treatment_wave==4, age_wave4,
                                                                  ifelse(treatment_wave==5, age_wave5,NA))))))



basic$days_to_min_at_treatment <-   with(basic, ifelse(treatment_wave==1, days_to_min_wave1,
                                                       ifelse(treatment_wave==2, days_to_min_wave2,
                                                              ifelse(treatment_wave==3, days_to_min_wave3,
                                                                     ifelse(treatment_wave==4, days_to_min_wave4,
                                                                            ifelse(treatment_wave==5, days_to_min_wave5,NA))))))
basic$min_sent_days <- with(basic, (min_cort_sent_yrs*365)+(min_cort_sent_mths*31)+(min_cort_sent_days))
basic$max_sent_days <- with(basic, (max_cort_sent_yrs*365)+(max_cort_sent_mths*31)+(max_cort_sent_days))

basic$life <- ifelse(basic$sentence_class %in% c("LIFE"),1,0)

# Calculate estimated admit dates & estimated time served
basic$est_admit_date <- basic$max_expir_date - 2*(days(basic$max_expir_date-basic$min_expir_date)) # estimate!

# Link with admit dates for lifers
basic <- left_join(basic, lifers[,c("admit_date", "research_id")])
basic[which(basic$research_id %in% lifers$research_id),"est_admit_date"] <- basic[which(basic$research_id %in% lifers$research_id),"admit_date"]
basic <- basic[,-which(names(basic)=="admit_date")]

i <- which(basic$sentence_class=="DEFINITE/FLAT")
basic$est_admit_date[i] <- basic$min_expir_date[i]-basic$min_sent_days[1]
i <- which(basic$sentence_class=="INDETERMINATE" & (basic$min_expir_date==basic$max_expir_date))
basic$est_admit_date[i] <- basic$max_expir_date[i]-basic$max_sent_days[i]
i <- which(basic$research_id=="rid_am3704")
basic$est_admit_date[i] <- ymd(20220430) # Manual override. Formula not 100% adequate

# DATA REQUEST: Information still missing for 20 individuals
i <- which(basic$sentence_class=="INDETERMINATE" & (basic$min_expir_date!="9999-01-01"
                                                    & basic$max_expir_date=="9999-01-01"))
basic$est_admit_date[i] <- NA
i <- which(basic$sentence_class=="DETENTION" & (basic$min_expir_date=="9999-01-01"
                                                & basic$max_expir_date=="9999-01-01"))
basic$est_admit_date[i] <- NA
i <- which(is.na(basic$sentence_class) & (basic$min_expir_date=="9999-01-01"
                                          & basic$max_expir_date=="9999-01-01"))
basic$est_admit_date[i] <- NA

basic$est_days_served_on_20220501 <- ymd(20220501)-basic$est_admit_date # Update with every wave
basic$est_days_served_on_20220501 <- as.numeric(gsub(" days", "", basic$est_days_served_on_20220501))

# Unresolved Issues
# delete date not in unambiguous format. Most are 9999999. All dates that are not 9999999 miss one number (e.g. 2022167)
