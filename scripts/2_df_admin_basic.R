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
# To do: Look into 55 NAs for research_id
# To do: Explore sent_min_expir_dt, sent_max_expir_dt, sent_off_asca, sent_max_expir_recmp_dt variables
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
standardize_uppercase <- function(x) {
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
# -- Read in Data ####
basic <- readRDS("data/processed/processing_layer_2/basic_masked.Rds")

# -- Prison Lookup Table ####
prison_lookup <- tribble(
  ~pris_loc,     ~pris_loc_full,
  "ALB",          "Albion",
  "BEN",          "Benner Township",
  "CAM",          "Cambridge Springs",
  "CHS",          "Chester",
  "COA",          "Coal Township",
  "DAL",         "Dallas",
  "FRA",          "Frackville",
  "FYT",          "Fayette",
  "FRS",         "Forest",
  "GRN",          "Greene",
  "HOU",          "Houtzdale",
  "HUN",          "Huntingdon",
  "LAU",          "Laurel Highlands",
  "MAH",          "Mahanoy",
  "MER",          "Mercer",
  "MUN",          "Muncy",
  "PHX",          "Phoenix",
  "PIT",          "Pittsburgh",
  "QUE",          "Quehanna Boot Camp",
  "RET",          "Retreat",
  "ROC",          "Rockview",
  "SMI",          "Smithfield",
  "SMR",          "Somerset",
  "WAM",          "Waymart"
) 

# ================================================================= ####
# Clean existing dataset
# Rename raw variables ####
# Append _raw to all columns except "research_id"
basic <- basic |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(basic), c("research_id","date_datapull", "control_number_pull", "wave")))

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
         sent_off_asca = `ASCA Category - Ranked_raw`)%>%
  mutate(chg_off_code = offense_code_raw,
         chg_des = offense_raw) %>%
  mutate(pris_loc = location_permanent_raw,
        pris_custody_lvl = custody_raw) %>%
  mutate(dem_dob_dt = date_of_birth_raw,
         dem_race = race_code_raw,
         dem_sex = sex_raw,
         dem_marital = marital_status_code_raw,
         dem_edu_grade = grade_complete_raw,
         dem_mhcode = MHCode_raw,
         dem_stg_yes = STG_raw) %>%
  relocate(ends_with("_raw"), .after = last_col()) 

# Clean variables ####
cols_cort <- c("sent_min_cort_yrs",
               "sent_min_cort_mths",
               "sent_min_cort_days",
               "sent_max_cort_yrs",
               "sent_max_cort_mths",
               "sent_max_cort_days")

cols_expir <- c("sent_min_expir_dt",
                "sent_max_expir_dt")

# save these acronyms to ensure they stay capitalized
acronyms <- c("DUI", 
              "IDSI")


# columns that need to be reformatted for the standardization of uppercase letters
cols_to_standardize <- c("offense_raw",
                         "sent_status", 
                         "sent_commitment_cnty", 
                         "chg_des")

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
  # -- Some responses were coded as NULL change this to NA
  mutate(sent_off_asca = na_if(sent_off_asca, "NULL")) %>%
  # -- Remove trailing white space
  mutate(sent_commitment_cnty = gsub("\\s+$", "", sent_commitment_cnty)) %>%
  # -- Use standardize function to fix uppercase discrepencies
  mutate(across(all_of(cols_to_standardize), standardize_uppercase)) %>%
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
      # -- NA values in marital_status_code_raw are listed as Unknown in marital_status_raw. 
      # -- Code ensures that both are recorded as "Unknown".
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
  mutate(dem_stg_yes = as.numeric(dem_stg_yes)) %>%
  # -- Change responses which are coded as NULL instead of NA
  mutate(dem_mhcode = na_if(dem_mhcode, "NULL")) %>%
  # PRISON
  # -- Change custody level into numeric result (ex L1 to 1)
  mutate(pris_custody_lvl = as.numeric(str_replace(pris_custody_lvl, "^L", ""))) %>%
  # -- Change the name from facility abrv to full name (ex CHS to Chester)
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(pris_loc, .after = pris_custody_lvl) %>%
  relocate(ends_with("_raw"), .after = last_col())

# ================================================================= ####
# Define new dataframes ####
# -- Static demographics ####
# one row per individuals using the earliest datapull
# -- -- Static Variables List: ####
# -- dem_edu_grade
# -- dem_dob_dt
# -- date_of_birth_raw
# -- -- Mostly Static Variables List: ####
# -- dem_race       2 ids (out of 2401) have changing race categorization
# -- race_code_raw  2 ids (out of 2401) have changing race categorization
# -- race_raw       2 ids (out of 2401) have changing race categorization
# -- dem_marital                1 id (out of 2401) has changing marital status
# -- marital_status_code_raw,   1 id (out of 2401) has changing marital status
# -- marital_status_raw         1 id (out of 2401) has changing marital status
# -- -- df Formation ####
basic_static_demographics <- basic %>%
  group_by(research_id) %>%
  # -- take variable info from only the earliest wave participated
  slice(1) %>% 
  ungroup() %>%
  # -- drop all variables but these, only static demographic variables
  select(control_number_pull, 
         research_id, 
         dem_race,
         dem_marital,
         dem_edu_grade,
         dem_dob_dt,
         date_datapull,
         race_code_raw,
         race_raw,
         marital_status_code_raw,
         marital_status_raw,
         date_of_birth_raw
  )
# -- Individual Sentence/ Charge ####
# -- -- one row per sentence (may be mutliple rows for one id)
basic_by_sentence <- basic %>%
  # -- group by these variables which when constant represent one charge
  group_by(dem_dob_dt, 
  # -- if any of these change than it will be reflected as a second row aka new charge
           sent_min_cort_yrs, 
           sent_min_cort_mths, 
           sent_min_cort_days) %>%
  slice(1) %>%     # One row per unique sentence
  ungroup() %>%
  # -- include only the following variables in new dataframe
  select(research_id,
         date_datapull,
         control_number_pull,
         sent_class,
         sent_min_cort_days,
         sent_min_cort_mths,
         sent_min_cort_yrs,
         sent_max_cort_days,
         sent_max_cort_mths,
         sent_max_cort_yrs,
         sent_commitment_cnty,
         sent_off_asca,
         chg_off_code,
         chg_des,
         dem_race,
         dem_marital,
         dem_edu_grade,
         dem_dob_dt,
         sentence_class_raw,
         min_cort_sent_days_raw,
         min_cort_sent_mths_raw,
         min_cort_sent_yrs_raw,
         max_cort_sent_days_raw,
         max_cort_sent_mths_raw,
         max_cort_sent_yrs_raw,
         commit_cnty_raw,
         `ASCA Category - Ranked_raw`,
         offense_code_raw,
         offense_raw,
         race_code_raw,
         race_raw,
         marital_status_code_raw,
         marital_status_raw,
         grade_complete_raw,
         date_of_birth_raw)

# ================================================================= ####
# Add Notes to Variables ####
  # to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(basic$sent_class) <- "Description of sentence type, 5 NA values, unknown cause otherwise cleaned variable, created using class_of_sent_raw"
comment(basic$sent_status) <- "Sentence status, no missing values, fully cleaned variable, created using sentence_status_raw"
comment(basic$sent_min_cort_yrs) <- "Min number of years sentenced, 318 (6.6%) missing values explained by those serving life, fully cleaned variable, created using min_cort_sent_yrs_raw" # what percent of the pop is 318?
comment(basic$sent_min_cort_mths) <- "Min number of months in sentence, 318 (6.6%) missing values explained by those serving life, fully cleaned variable, created using min_cort_sent_mnths_raw"
comment(basic$sent_min_cort_days) <- "Min number of days in sentence, 318 (6.6%) missing values explained by those serving life, fully cleaned variable, created using min_cort_sent_days_raw"
comment(basic$sent_max_cort_yrs) <- "Max number of years sentenced, 318 (6.6%) missing values explained by those serving life, fully cleaned variable, created using max_cort_sent_yrs_raw"
comment(basic$sent_max_cort_mths) <- "Max number of months in sentence, 318 (6.6%) missing values explained by those serving life, fully cleaned variable, created using max_cort_sent_mnths_raw"
comment(basic$sent_max_cort_days) <- "Max number of days in sentece, 318 (6.6%) missing values explained by those serving life, fully cleaned variable, created using max_cort_sent_days_raw"
comment(basic$sent_min_expir_dt) <- "Earliest date of end of sentence, 322 (6.7%) missing values, explore this further, created using min_expir_date_raw" 
comment(basic$sent_max_expir_dt) <- "Latest date of end of sentence, 359 (7.5%) missing values, explore this further, created using max_expir_date_raw" 
comment(basic$sent_max_expir_recmp_dt) <- "Latest date of recomputed sentence, 4577 (95.4%) missing values, created using RecmpMax_Dt_raw" 
comment(basic$sent_commitment_cnty) <- "County of commitment, fully cleaned variable, created using commit_cnty_raw"
comment(basic$sent_off_asca) <- "Type of offense, 4 missing values and 91 NULL (1.9%), fully cleaned variable,  created using ASCA Category - Ranked_raw" # why are there 4 NA and 91 NULL? Should these be combined 
comment(basic$chg_off_code) <- "Offense code,9 (0.2%) missing values, fully cleaned variable, created using offense_code_raw"
comment(basic$chg_des) <- "Offense description, 9 (0.2%) missing values, not fully cleaned need to fix capitalization, created using offense_raw"
comment(basic$pris_custody_lvl) <- "Individual custody level, 161 (3.4%) missing values for unknown reason, explore further, created using custody_raw"
comment(basic$pris_loc) <- "Facility name, no missing values, fully cleaned, created using location_permanent_raw"
comment(basic$dem_dob_dt) <- "Date of birth, no missing values, fully cleaned, created using date_of_birth_raw"
comment(basic$dem_race) <- "Race, no missing values, fully cleaned variable, created using race_code_raw"
comment(basic$dem_marital) <- "Marital status, no missing values, fully cleaned variable, created using marital_status_code_raw"
comment(basic$dem_edu_grade) <- "Highest level of education completed, 3 (0.06%) missing values, fully cleaned variable, created using grade_complete_raw"
comment(basic$dem_mhcode) <- "Classification of mental health, 5 (0.1%) missing values, unknown cause fully cleaned variable, created using MHCode_raw" 
comment(basic$dem_stg_yes) <- "Known gang affiliation = 1, 2162 (45.1%) missing values appears to be missing data, those without a stg are recorded as 0, created using STG_raw"
# -- Raw Variables ####
### add name of cleaned variable verison

comment(basic$location_permanent_raw) <- "raw data, cleaned non raw variable avail as pris_loc"
comment(basic$delete_date_raw) <- "raw data, uncleaned variable, available only in raw form"
comment(basic$sentence_status_raw) <- "raw data, cleaned non raw variable avail as sent_status"
comment(basic$custody_raw) <- "raw data, cleaned non raw variable avail as pris_custody_lvl"
comment(basic$temp_custody_raw) <- "raw data, uncleaned variable, available only in raw form"
comment(basic$class_of_sent_raw) <- "raw data, cleaned non raw variable avail as sent_class"
comment(basic$sentence_class_raw) <- "raw data, cleaned non raw variable avail as sent_class"
comment(basic$min_cort_sent_yrs_raw) <- "raw data, cleaned non raw variable avail as sent_min_cort_yrs"
comment(basic$min_cort_sent_mths_raw) <- "raw data, cleaned non raw variable avail as sent_min_cort_mnths"
comment(basic$min_cort_sent_days_raw) <- "raw data, cleaned non raw variable avail as sent_min_cort_days"
comment(basic$max_cort_sent_yrs_raw) <- "raw data, cleaned non raw variable avail as sent_max_cort_yrs"
comment(basic$max_cort_sent_mths_raw) <- "raw data, cleaned non raw variable avail as sent_max_cort_mnths"
comment(basic$max_cort_sent_days_raw) <- "raw data, cleaned non raw variable avail as sent_max_cort_days"
comment(basic$commit_cnty_raw) <- "raw data, cleaned non raw variable avail as sent_commitment_cnty"
comment(basic$cnty_name_raw) <- "raw data, cleaned non raw variable avail as sent_comitment_cnty"
comment(basic$offense_code_raw) <- "raw data, cleaned non raw variable avail as chg_off_code"
comment(basic$offense_raw) <- "raw data, cleaned non raw variable avail as chg_des"
comment(basic$`ASCA Category - Ranked_raw`) <- "raw data, cleaned non raw variable avail as sent_off_asca"
comment(basic$min_expir_date_raw) <- "raw data, cleaned non raw variable avail as sent_min_expir_dt"
comment(basic$max_expir_date_raw) <- "raw data, cleaned non raw variable avail as sent_max_expir_dt"
comment(basic$RecmpMax_Dt_raw) <- "raw data, cleaned non raw variable avail as sent_max_expir_recmp_dt"
comment(basic$date_of_birth_raw) <- "raw data, cleaned non raw variable avail as dem_dob_dt"
comment(basic$race_code_raw) <- "raw data, cleaned non raw variable avail as dem_race"
comment(basic$race_raw) <- "raw data, cleaned non raw variable avail as dem_race"
comment(basic$sex_type_raw) <- "raw data, cleaned non raw variable avail as dem_sex"
comment(basic$sex_raw) <- "raw data, cleaned non raw variable avail as dem_sex"
comment(basic$marital_status_code_raw) <- "raw data, cleaned non raw variable avail as dem_marital"
comment(basic$marital_status_raw) <- "raw data, cleaned non raw variable avail as dem_marital"
comment(basic$grade_complete_raw) <- "raw data, cleaned non raw variable avail as dem_edu_grade"
comment(basic$MHCode_raw) <-"raw data, cleaned non raw variable avail as dem_mhcode"
comment(basic$STG_raw) <- "raw data, cleaned non raw variable avail as dem_stg_yes"

# ================================================================= ####

# New Variables ####
basic <- basic %>%
  mutate(dem_edu_high_school = ifelse(dem_edu_grade>=12, 1,0)) %>%
  relocate(dem_edu_high_school, .after = dem_edu_grade) %>%
  mutate(
    dem_age_wave1 = decimal_date(ymd(wave1_date))-decimal_date(dem_dob_dt),
    dem_age_wave2 = decimal_date(ymd(wave2_date))-decimal_date(dem_dob_dt),
    dem_age_wave3 = decimal_date(ymd(wave3_date))-decimal_date(dem_dob_dt),
    dem_age_wave4 = decimal_date(ymd(wave4_date))-decimal_date(dem_dob_dt),
    dem_age_wave5 = decimal_date(ymd(wave5_date))-decimal_date(dem_dob_dt),
    dem_age_wave6 = decimal_date(ymd(wave6_date))-decimal_date(dem_dob_dt)) %>%
  relocate(starts_with("dem_age_wave"), .after = dem_dob_dt) %>%
  mutate(
    sent_days_to_min_wave1 = sent_min_expir_dt - ymd(wave1_date),
    sent_days_to_min_wave2 = sent_min_expir_dt - ymd(wave2_date),
    sent_days_to_min_wave3 = sent_min_expir_dt - ymd(wave3_date),
    sent_days_to_min_wave4 = sent_min_expir_dt - ymd(wave4_date),
    sent_days_to_min_wave5 = sent_min_expir_dt - ymd(wave5_date),
    sent_days_to_min_wave6 = sent_min_expir_dt - ymd(wave6_date)) %>%
  relocate(starts_with("sent_days_to_min_wave"), .after = sent_max_expir_recmp_dt) %>%
  mutate(
    sent_min_in_days = (sent_min_cort_yrs*365)+(sent_min_cort_mths*12)+sent_min_cort_days,
    sent_max_in_days = (sent_max_cort_yrs*365)+(sent_max_cort_mths*12)+sent_max_cort_days) %>%
  relocate(starts_with("sent_min_in_days"), .after = sent_max_cort_days) %>%
  relocate(starts_with("sent_max_in_days"), .after = sent_min_in_days)
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
