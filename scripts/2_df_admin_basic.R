# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Clean basic dataframe 
# Create two new dataframes - one at the individual level with (mostly) static demographics, and one at the individual*sentence level 
# -- Readme ####
# Basic contains demographic and sentencing information 
# Level of observation: individual*date_datapull*conviction*charge on which people were convicted
# Britte: I think that each datapull contains sentencing information for only the latest conviction - confirm 
# Multiple charges may be associated with different sentence expiry dates
# -- but if you subtract the sent_in_days you end up with the same date - e.g. see rid_001394
# -- these are likely sentences served concurrently 
# -- To do ####
# To do: ASCA classification is sometimes NULL while the offense code is the same as that of other variables, e.g "STRANGULATION: APPLYING PRESSURE TO THROAT OR NECK" is sometimes classified as violent and sometimes as NULL. Manually recategorize?
# See todo do section at the end of the script 
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
standardize_uppercase <- function(x) {
  x <- x %>%
    str_squish() %>%
    str_replace_all("\\s*-\\s*", " - ") %>%                        # Normalize dash spacing
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
basic <- readRDS("data/processed/de_identified/1b_basic_masked.Rds")
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds")
# ================================================================= ####
# Code to align with merged administrative data files ####
basic$date_datapull <- ymd(20250623)
basic$wave <- 0
# ================================================================= ####
# Rename raw variables ####
# Append _raw to all columns except specified columns
basic <- basic |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(basic), c("research_id","date_datapull", "control_number", "wave")))

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
         sent_off_asca = Ranking_raw)%>%
  mutate(chg_off_code = offense_code_raw,
         chg_des = offense_raw) %>%
  mutate(pris_loc = location_permanent_raw,
        pris_custody_lvl = custody_raw) %>%
  mutate(dem_dob_dt = date_of_birth_raw,
         dem_race = race_code_raw,
         dem_sex = sex_raw,
         dem_marital = marital_status_code_raw,
         dem_edu_grade = grade_complete_raw,
         dem_mhcode = mh_code_raw,
         dem_stg_yes = stg_raw) %>%
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


# columns that need to be reformatted for the standardization of uppercase letters
cols_to_standardize <- c("offense_raw",
                         "sent_status", 
                         "sent_commitment_cnty", 
                         "chg_des",
                         "dem_sex")

# save these acronyms to ensure they stay capitalized
acronyms <- c("DUI", 
              "IDSI")

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
  # SENTENCING INFO
  # -- mutate abrv into long form sent_class
  mutate(sent_class = case_when(
    sent_class == "CL" ~ "Commuted Life",
    sent_class == "DF" ~ "Definite",
    sent_class == "DT" ~ "Detention",
    sent_class == "IN" ~ "Indeterminate",
    sent_class == "LF" ~ "Life",
    TRUE ~ sent_class)) %>%
  # -- standardize ASCA codes
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
  # -- Use standardize function to fix uppercase discrepancies
  mutate(across(all_of(cols_to_standardize), standardize_uppercase)) %>%
  # DEMOGRAPHICS
  mutate(dem_dob_dt = ymd(dem_dob_dt)) %>%
  # -- mutate race abrv into long form race 
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
  # -- mutate other marital status into long form title
      marital_status_code_raw == "MAR" ~ "Married",
      marital_status_code_raw == "DIV" ~ "Divorced",
      marital_status_code_raw == "SIN" ~ "Single",
      marital_status_code_raw == "SEP" ~ "Separated",
      marital_status_code_raw == "WID" ~ "Widow",
      is.na(marital_status_code_raw) & !is.na(marital_status_raw) ~ marital_status_raw,
      TRUE ~ NA_character_
    )) %>%
  # -- some entries from dem_edu_grade have leading zeros, remove these
  mutate(dem_edu_grade = remove_leading_zeros(dem_edu_grade) |> as.numeric()) %>%
  # -- remove trailing whitespaces from dem_mhcode
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
  relocate(ends_with("_raw"), .after = last_col()) %>%
  relocate(date_datapull, .after = dem_stg_yes) %>%
  relocate(wave, .after = date_datapull)

# Create Dummies for Categorical Variables ####
basic <- make_dummies(basic, dem_marital)
basic <- make_dummies(basic, dem_sex)
basic <- make_dummies(basic, sent_class)
basic <- make_dummies(basic, sent_off_asca)
basic <- make_dummies(basic, dem_race)
# ================================================================= ####
# Add Notes to Variables ####
  # to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(basic$sent_class) <- "Description of sentence type, 5/5490 NA values, unknown cause otherwise cleaned variable, created using class_of_sent_raw (6/11/25)"
comment(basic$sent_status) <- "Active sentence status, no missing values, fully cleaned variable, created using sentence_status_raw (6/11/25)"
comment(basic$sent_min_cort_yrs) <- "Min number of years sentenced, 358/5490 missing values explained by those serving life, fully cleaned variable, created using min_cort_sent_yrs_raw (6/11/25)"
comment(basic$sent_min_cort_mths) <- "Min number of months in sentence, 358/5490 missing values explained by those serving life, fully cleaned variable, created using min_cort_sent_mnths_raw (6/11/25)"
comment(basic$sent_min_cort_days) <- "Min number of days in sentence, 358/5490 missing values explained by those serving life, fully cleaned variable, created using min_cort_sent_days_raw (6/11/25)"
comment(basic$sent_max_cort_yrs) <- "Max number of years sentenced, 358/5490 missing values explained by those serving life, fully cleaned variable, created using max_cort_sent_yrs_raw (6/11/25)"
comment(basic$sent_max_cort_mths) <- "Max number of months in sentence, 358/5490 missing values explained by those serving life, fully cleaned variable, created using max_cort_sent_mnths_raw (6/11/25)"
comment(basic$sent_max_cort_days) <- "Max number of days in sentece, 358/5490 missing values explained by those serving life, fully cleaned variable, created using max_cort_sent_days_raw (6/11/25)"
comment(basic$sent_min_expir_dt) <- "Earliest date of end of sentence, 365/5490 missing values, explore this further, created using min_expir_date_raw (6/11/25)" 
comment(basic$sent_max_expir_dt) <- "Latest date of end of sentence, 409/5490 missing values, explore this further, created using max_expir_date_raw (6/11/25)" 
comment(basic$sent_max_expir_recmp_dt) <- "Latest date of recomputed sentence, 5270/5490 missing values, created using RecmpMax_Dt_raw (6/11/25)" 
comment(basic$sent_commitment_cnty) <- "County of commitment, fully cleaned variable, created using commit_cnty_raw (6/11/25)"
comment(basic$sent_off_asca) <- "Type of offense, 98/5490 NA values, fully cleaned variable,  created using ASCA Category - Ranked_raw (6/11/25)"
comment(basic$chg_off_code) <- "Offense code, 12/5490 NA values, fully cleaned variable, created using offense_code_raw (6/11/25)"
comment(basic$chg_des) <- "Offense description, 12/5490 NA values, fully cleaned variable, created using offense_raw (6/11/25)"
comment(basic$pris_custody_lvl) <- "Individual custody level, 297/5490 missing values for unknown reason, explore further, created using custody_raw (6/11/25)"
comment(basic$pris_loc) <- "Facility name, no missing values, fully cleaned, created using location_permanent_raw (6/11/25)"
comment(basic$dem_dob_dt) <- "Date of birth, no missing values, fully cleaned, created using date_of_birth_raw (6/11/25)"
comment(basic$dem_race) <- "Race, no missing values, fully cleaned variable, created using race_code_raw (6/11/25)"
comment(basic$dem_marital) <- "Marital status, no missing values, fully cleaned variable, created using marital_status_code_raw (6/11/25)"
comment(basic$dem_edu_grade) <- "Highest level of education completed, 6/5490 missing values, fully cleaned variable, created using grade_complete_raw (6/11/25)"
comment(basic$dem_mhcode) <- "Classification of mental health, 41/5490 missing values, unknown cause fully cleaned variable, created using MHCode_raw (6/11/25)" 
comment(basic$dem_stg_yes) <- "Known gang affiliation = 1, 2855/5490 missing values appears to be missing data, those without a stg are recorded as 0, created using STG_raw (6/11/25)"
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
comment(basic$Ranking_raw) <- "raw data, cleaned non raw variable avail as sent_off_asca"
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
comment(basic$mh_code_raw) <-"raw data, cleaned non raw variable avail as dem_mhcode"
comment(basic$stg_raw) <- "raw data, cleaned non raw variable avail as dem_stg_yes"

# ================================================================= ####
# Merge in treatment status ####
basic <- basic %>%
  left_join(randassign,
            by = "research_id", 
            relationship = "many-to-one")
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
    dem_age_wave6 = decimal_date(ymd(wave6_date))-decimal_date(dem_dob_dt),
    dem_age_wave7 = decimal_date(ymd(wave7_date))-decimal_date(dem_dob_dt)) %>%
  relocate(starts_with("dem_age_wave"), .after = dem_dob_dt) %>%
  mutate(dem_age_at_treatment = case_when(
    rct_treat_wave == 0 ~ decimal_date(ymd(rand1_date))-decimal_date(dem_dob_dt),
    rct_treat_wave == 1 ~ decimal_date(ymd(rand2_date))-decimal_date(dem_dob_dt),
    rct_treat_wave == 2.5 ~ decimal_date(ymd(rand2.5_date))-decimal_date(dem_dob_dt),
    rct_treat_wave == 3 ~ decimal_date(ymd(rand3_date))-decimal_date(dem_dob_dt),
    rct_treat_wave == 4 ~ decimal_date(ymd(rand4_date))-decimal_date(dem_dob_dt),
    rct_treat_wave == 5 ~ decimal_date(ymd(rand5_date))-decimal_date(dem_dob_dt),
    rct_treat_wave == 6 ~ decimal_date(ymd(rand6_date))-decimal_date(dem_dob_dt),
    rct_treat_wave == 7 ~ decimal_date(ymd(rand7_date))-decimal_date(dem_dob_dt),
    TRUE ~ NA)) %>%
  mutate(
    sent_days_to_min_wave1 = sent_min_expir_dt - ymd(wave1_date),
    sent_days_to_min_wave2 = sent_min_expir_dt - ymd(wave2_date),
    sent_days_to_min_wave3 = sent_min_expir_dt - ymd(wave3_date),
    sent_days_to_min_wave4 = sent_min_expir_dt - ymd(wave4_date),
    sent_days_to_min_wave5 = sent_min_expir_dt - ymd(wave5_date),
    sent_days_to_min_wave6 = sent_min_expir_dt - ymd(wave6_date)) %>%
  relocate(starts_with("sent_days_to_min_wave"), .after = sent_max_expir_recmp_dt) %>%
  mutate(
    # -- Assuming 30 day months for simplicity
    sent_min_in_days = (sent_min_cort_yrs*365)+(sent_min_cort_mths*30)+sent_min_cort_days,
    sent_max_in_days = (sent_max_cort_yrs*365)+(sent_max_cort_mths*30)+sent_max_cort_days) %>%
  relocate(starts_with("sent_min_in_days"), .after = sent_max_cort_days) %>%
  relocate(starts_with("sent_max_in_days"), .after = sent_min_in_days)

# New Variables Specific to RCT Sample ####
basic <- basic %>%
  mutate(dem_edu_high_school = ifelse(dem_edu_grade>=12, 1,0)) %>%
  mutate(
    dem_age_treat1 = decimal_date(ymd(rand1_date))-decimal_date(dem_dob_dt),
    dem_age_treat2 = decimal_date(ymd(rand2_date))-decimal_date(dem_dob_dt),
    dem_age_treat2.5 = decimal_date(ymd(rand2.5_date))-decimal_date(dem_dob_dt),
    dem_age_treat3 = decimal_date(ymd(rand3_date))-decimal_date(dem_dob_dt),
    dem_age_treat4 = decimal_date(ymd(rand4_date))-decimal_date(dem_dob_dt),
    dem_age_treat5 = decimal_date(ymd(rand5_date))-decimal_date(dem_dob_dt),
    dem_age_treat6 = decimal_date(ymd(rand6_date))-decimal_date(dem_dob_dt),
    dem_age_treat7 = decimal_date(ymd(rand7_date))-decimal_date(dem_dob_dt)) %>%
  mutate(dem_age_at_treatment = case_when(
    rct_treat_wave == 1 ~ dem_age_treat1,
    rct_treat_wave == 2 ~ dem_age_treat2,
    rct_treat_wave == 2.5 ~ dem_age_treat2.5,
    rct_treat_wave == 3 ~ dem_age_treat3,
    rct_treat_wave == 4 ~ dem_age_treat4,
    rct_treat_wave == 5 ~ dem_age_treat5,
    rct_treat_wave == 6 ~ dem_age_treat6,
    rct_treat_wave == 7 ~ dem_age_treat7
  )) %>%
  select(-starts_with("dem_age_treat"))

# ================================================================= ####
# Crude De-duplication - Improve this ####
basic <- basic |>
  arrange(research_id, desc(sent_min_in_days)) %>%
  # Keep only the first time we pulled data for this person 
  # -- For the majority of people, this will be the conviction for the admission
  # -- that is linked to their RCT participation 
  # -- plus the most serious charge (per desc(sent_min_in_days)) 
  # -- -- Note there will be more variability for the PCQ sample 
  distinct(research_id, .keep_all = TRUE) 

# Should now have a dataframe at the individual level 
stopifnot(length(unique(basic$research_id))==nrow(basic))
# ================================================================= ####
# Temporary Descriptive Stats ####

# -- summary stats for min/max time sentenced
basic %>%
  select(sent_min_cort_yrs, sent_min_cort_mths, sent_min_cort_days,
         sent_max_cort_yrs, sent_max_cort_mths, sent_max_cort_days) %>%
  summary()
# -- Frequencies for Categorical Variabless
# sentence class
basic %>% count(sent_class, sort = TRUE)

# Demographics
basic %>% count(dem_race, sort = TRUE)
basic %>% count(dem_sex, sort = TRUE)
basic %>% count(dem_marital, sort = TRUE)
basic %>% count(dem_edu_grade, sort = TRUE)

# Mental health / STG
basic %>% count(dem_mhcode, sort = TRUE)
basic %>% count(dem_stg_yes, sort = TRUE)

# Custody level
basic %>% count(pris_custody_lvl, sort = TRUE)
# -- Analysis of county of commitment by race
basic %>% count(sent_commitment_cnty, sort = TRUE) 
basic %>%
  group_by(sent_commitment_cnty, dem_race) %>%
  tally() %>%
  arrange(desc(n))
# ================================================================= ####
# Reorganize Variables ####
basic <- reorder_vars(basic)
# ================================================================= ####
# Save dataframes ####
saveRDS(basic, file = "data/processed/de_identified/2_basic_cleaned.Rds")
# ================================================================= ####
# # RELEVANT OLD CODE TO INTEGRATE LATER, INCL TIME SERVED ####
# 
# # Merge in age at treatment
# basic$age_at_treatment <- with(basic, ifelse(treatment_wave==1, age_wave1,
#                                              ifelse(treatment_wave==2, age_wave2,
#                                                     ifelse(treatment_wave==3, age_wave3,
#                                                            ifelse(treatment_wave==4, age_wave4,
#                                                                   ifelse(treatment_wave==5, age_wave5,NA))))))
# 
# 
# 
# basic$days_to_min_at_treatment <-   with(basic, ifelse(treatment_wave==1, days_to_min_wave1,
#                                                        ifelse(treatment_wave==2, days_to_min_wave2,
#                                                               ifelse(treatment_wave==3, days_to_min_wave3,
#                                                                      ifelse(treatment_wave==4, days_to_min_wave4,
#                                                                             ifelse(treatment_wave==5, days_to_min_wave5,NA))))))
# 
# 
# # Calculate estimated admit dates & estimated time served
# basic$est_admit_date <- basic$max_expir_date - 2*(days(basic$max_expir_date-basic$min_expir_date)) # estimate!
# 
# # Link with admit dates for lifers
# basic <- left_join(basic, lifers[,c("admit_date", "research_id")])
# basic[which(basic$research_id %in% lifers$research_id),"est_admit_date"] <- basic[which(basic$research_id %in% lifers$research_id),"admit_date"]
# basic <- basic[,-which(names(basic)=="admit_date")]
# 
# i <- which(basic$sentence_class=="DEFINITE/FLAT")
# basic$est_admit_date[i] <- basic$min_expir_date[i]-basic$min_sent_days[1]
# i <- which(basic$sentence_class=="INDETERMINATE" & (basic$min_expir_date==basic$max_expir_date))
# basic$est_admit_date[i] <- basic$max_expir_date[i]-basic$max_sent_days[i]
# i <- which(basic$research_id=="rid_am3704")
# basic$est_admit_date[i] <- ymd(20220430) # Manual override. Formula not 100% adequate
# 
# # DATA REQUEST: Information still missing for 20 individuals
# i <- which(basic$sentence_class=="INDETERMINATE" & (basic$min_expir_date!="9999-01-01"
#                                                     & basic$max_expir_date=="9999-01-01"))
# basic$est_admit_date[i] <- NA
# i <- which(basic$sentence_class=="DETENTION" & (basic$min_expir_date=="9999-01-01"
#                                                 & basic$max_expir_date=="9999-01-01"))
# basic$est_admit_date[i] <- NA
# i <- which(is.na(basic$sentence_class) & (basic$min_expir_date=="9999-01-01"
#                                           & basic$max_expir_date=="9999-01-01"))
# basic$est_admit_date[i] <- NA
# 
# basic$est_days_served_on_20220501 <- ymd(20220501)-basic$est_admit_date # Update with every wave
# basic$est_days_served_on_20220501 <- as.numeric(gsub(" days", "", basic$est_days_served_on_20220501))
# 
# 

# Discarded code ####
# # -- Basic by static demographics ####
# # one row per individuals using the earliest datapull
# # -- -- Static Variables List:
# # -- dem_edu_grade
# # -- dem_dob_dt
# # -- date_of_birth_raw
# # -- -- Mostly Static Variables List: 
# # -- dem_race       2 ids (out of 2401) have changing race categorization
# # -- race_code_raw  2 ids (out of 2401) have changing race categorization
# # -- race_raw       2 ids (out of 2401) have changing race categorization
# # -- dem_marital                1 id (out of 2401) has changing marital status
# # -- marital_status_code_raw,   1 id (out of 2401) has changing marital status
# # -- marital_status_raw         1 id (out of 2401) has changing marital status
# # -- -- df Formation 
# basic_static_demographics <- basic %>%
#   group_by(research_id) %>%
#   # -- take variable info from only the earliest wave participated
#   slice(1) %>% 
#   ungroup() %>%
#   # -- drop all variables but these, only static demographic variables
#   select(research_id, 
#          dem_race,
#          dem_marital,
#          dem_edu_grade,
#          dem_dob_dt,
#          date_datapull,
#          race_code_raw,
#          race_raw,
#          marital_status_code_raw,
#          marital_status_raw,
#          date_of_birth_raw
#   )
# # -- Basic by Individual Sentence/ Charge ####
# # -- -- one row per sentence (may be mutliple rows for one id)
# basic_by_sentence <- basic %>%
#   # -- group by these variables which when constant represent one charge
#   group_by(dem_dob_dt, 
#            # -- if any of these change than it will be reflected as a second row aka new charge
#            sent_min_cort_yrs, 
#            sent_min_cort_mths, 
#            sent_min_cort_days) %>%
#   slice(1) %>%     # One row per unique sentence
#   ungroup() %>%
#   # -- include only the following variables in new dataframe
#   select(research_id,
#          date_datapull,
#          sent_class,
#          sent_min_cort_days,
#          sent_min_cort_mths,
#          sent_min_cort_yrs,
#          sent_max_cort_days,
#          sent_max_cort_mths,
#          sent_max_cort_yrs,
#          sent_commitment_cnty,
#          sent_off_asca,
#          chg_off_code,
#          chg_des,
#          dem_race,
#          dem_marital,
#          dem_edu_grade,
#          dem_dob_dt,
#          sentence_class_raw,
#          min_cort_sent_days_raw,
#          min_cort_sent_mths_raw,
#          min_cort_sent_yrs_raw,
#          max_cort_sent_days_raw,
#          max_cort_sent_mths_raw,
#          max_cort_sent_yrs_raw,
#          commit_cnty_raw,
#          `ASCA Category - Ranked_raw`,
#          offense_code_raw,
#          offense_raw,
#          race_code_raw,
#          race_raw,
#          marital_status_code_raw,
#          marital_status_raw,
#          grade_complete_raw,
#          date_of_birth_raw)