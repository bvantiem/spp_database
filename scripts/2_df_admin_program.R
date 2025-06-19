# =================================================================== ####
# Notes to Script: ####
# -- Objective ####
# -- Readme ####
# -- -- level of observation: research_id*prg_name*prg_start_date
# -- To do ####
# =================================================================== ####
# Set up ####
# -- Prepare Environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
# -- Functions ####
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
  
  return(result)}
# -- Read in Data ####
program <- readRDS("data/processed/de_identified/1b_program_masked.Rds")

# =================================================================== ####
# Rename Raw Variables ####
# Append _raw to all columns except specified columns
program <- program |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(program), c("research_id","date_datapull", "control_number", "wave")))

program <- program |>
  mutate(prg_name = Prgm_Nm_raw,
         prg_status = CompletionDesc_raw,
         prg_eval = Eval_Des_raw,
         prg_start_date = Inm_StrDt_raw,
         prg_end_date = Inm_EndDt_raw) |>
  mutate(pris_loc = Fac_Cd_raw) 
# Clean Variables ####
program <- program |> 
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) |>
  # -- Some responses were coded as NULL change this to NA
  mutate(across(where(is.character), ~ na_if(., "NULL"))) |>
  # DATES
  # -- move time for program to new column and drop
  mutate(prg_start_time = format(as.POSIXct(prg_start_date), format = "%H:%M:%S"),
         prg_start_date = as.Date(prg_start_date)) %>%
  select(-prg_start_time) %>%
  mutate(prg_end_time = format(as.POSIXct(prg_end_date), format = "%H:%M:%S"),
         prg_end_date = as.Date(prg_end_date)) %>%
  select(-prg_end_time) %>%
  # PROGRAM
  # -- standardize program names
  mutate(prg_name = case_when(
    prg_name == "Violence Prevention - High 12"        ~ "Violence Prevention High Intensity",
    prg_name == "Violence Prevention - Moderate 29"     ~ "Violence Prevention Moderate Intensity",
    prg_name == "Long Term Offender"                    ~ "Long Term Offenders",
    prg_name == "Sex Offender Program - Evaluation"     ~ "Sex Offender Program Evaluation",
    TRUE                                               ~ prg_name
  )) %>%
  # -- standardize prg_status 
  mutate(prg_status = case_when(
    prg_status == "Discharge prior to completion" ~ "Discharged Prior to Completion",
    prg_status == "Discharged prior to completion" ~ "Discharged Prior to Completion",
    prg_status == "Fail" ~ "Fail",
    prg_status == "Success" ~ "Success"
  )) %>%
  # -- create higher level program categorization
  mutate(prg_cat = case_when(
    grepl("\\bVP\\b|violence prevention", prg_name, ignore.case = TRUE) ~ "Violence Prevention",   # PRIORITY 1
    grepl("\\bTC\\b|therapeutic community", prg_name, ignore.case = TRUE) ~ "Therapeutic Community",
    grepl("\\bSDTP\\b|\\bRSO\\b|sex offender", prg_name, ignore.case = TRUE) ~ "Sex Offender Program", 
    grepl("substance|drug|alcohol|12 step|narcotics", prg_name, ignore.case = TRUE) ~ "Substance Abuse Program",
    grepl("\\bTPV\\b|\\bPV\\b|parole violator", prg_name, ignore.case = TRUE) ~ "Parole Violator Program",
    grepl("outpatient", prg_name, ignore.case = TRUE) ~ "OutPatient",
    grepl("parent|dads", prg_name, ignore.case = TRUE) ~ "Parenting Program",
    grepl("batterers", prg_name, ignore.case = TRUE) ~ "Batterers Program",
    grepl("thinking for a change", prg_name, ignore.case = TRUE) ~ "Thinking for a Change",
    grepl("counseling|seeking safety|relapse prevention|special needs|self -help|therapy", prg_name, ignore.case = TRUE) ~ "Mental Health or Counseling",
    grepl("back on track|parole|re-entry", prg_name, ignore.case = TRUE) ~ "Re-Entry/ Transitional Programs",
    TRUE ~ "Other"
  )) %>%
  relocate(prg_cat, .after = prg_name) %>%
  mutate(prg_end_date = case_when(
    research_id == "rid_519531" & prg_name == "Business Education"       ~ NA_Date_,
    research_id == "rid_061378" & prg_name == "Opioid Use Disorder Therapeutic Community"  ~ NA_Date_,
    research_id == "rid_551550" & prg_name == "Alcohol and Other Drug Education"        ~ NA_Date_,
    TRUE                                                        ~ prg_end_date
  )) %>%
  # DUMMY VARIABLES
  # -- create dummy variables for higher level program categories
  mutate(
    prg_cat_vp = ifelse(prg_cat == "Violence Prevention", 1, 0),
    prg_cat_tc = ifelse(prg_cat == "Therapeutic Community", 1, 0),
    prg_cat_so = ifelse(prg_cat == "Sex Offender Program", 1, 0),
    prg_cat_sub = ifelse(prg_cat == "Substance Abuse Program", 1, 0),
    prg_cat_pv = ifelse(prg_cat == "Parole Violator Program", 1, 0),
    prg_cat_op = ifelse(prg_cat == "OutPatient", 1, 0),
    prg_cat_prnt = ifelse(prg_cat == "Parenting Program", 1, 0),
    prg_cat_batt = ifelse(prg_cat == "Batterers Program", 1, 0),
    prg_cat_tforc = ifelse(prg_cat == "Thinking for a Change", 1, 0),
    prg_cat_mnt = ifelse(prg_cat == "Mental Health or Counseling", 1, 0),
    prg_cat_re = ifelse(prg_cat == "Re-Entry/ Transitional Programs", 1,0),
    prg_cat_oth = ifelse(prg_cat == "Other", 1, 0)
  )  %>%
# PRISON
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) 

program <- make_dummies(program, prg_status)
# =================================================================== ####
# Temporary Notes to Show Britte ####
# -- end date issues
program %>% filter(prg_end_date == "2100-01-01")
program %>% filter(prg_end_date == "2030-03-25")

# -- some prg_end_dates are in the future building in this stop so that we can correct these
# for future waves
# stopifnot(program$date_datapull + 365 < program$prg_end_date)
# =================================================================== ####
# Add Notes to Variables ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(program$pris_loc) <- "Facility location of programming, 0 NA values, fully cleaned, created using Fac_Cd_raw (6/6/25)"
comment(program$prg_name) <- "Name of program, no NA values, fully cleaned, created using Prgm_Nm_raw (6/6/25)"
comment(program$prg_status) <- "Status in the program, 939/9329 NA values, created using CompletionDesc_raw and ComplOrFail_SW_raw (6/6/25)"
comment(program$prg_eval) <- "Evaluation recieved for participant, 2348/9329 NA values, created using Eval_Des_raw (6/6/25)"
comment(program$prg_start_date) <- "Program start date, no NA values, fully cleaned, created using Inm_StrDt_raw (6/6/25)"
comment(program$prg_end_date) <- "Program end date, 377/9329 NA values, NOT FULLY CLEANED, created using Inm_EndDt_raw (6/6/25)"
# -- Raw Variables ####
comment(program$Prgm_Nm_raw) <- "raw data, non raw available as prg_name (6/6/25)"
comment(program$Fac_Cd_raw) <- "raw data, non raw available as pris_loc (6/6/25)"
comment(program$CompletionDesc_raw) <- "raw data, non raw available as prg_status (6/6/25)"
comment(program$Inm_StrDt_raw) <- "raw data, non raw available as prg_start_date (6/6/25)"
comment(program$Inm_EndDt_raw) <- "raw data, non raw available as prg_end_date (6/6/25)"
comment(program$Eval_Des_raw) <- "raw data, non raw available as prg_eval (6/6/25)"
# =================================================================== ####
# New Variables ####
# length of program participation
program <- program %>%
  mutate(
    prg_start_date = as.Date(prg_start_date),
    prg_end_date = as.Date(prg_end_date),
    prg_length_days = as.numeric(prg_end_date - prg_start_date)
  ) %>%
  relocate(prg_length_days, .after = prg_end_date)
# =================================================================== ####
# Temporary Descriptive Stats ####
# -- length of program participation
summary(program$prg_length_days)

# -- number of programs per person
programs_per_person <- program %>%
  group_by(research_id) %>%
  summarize(n_programs = n())

summary(programs_per_person$n_programs)

# -- frequency of each program category
program %>%
  summarize(across(starts_with("prg_cat_"), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "category", values_to = "count") %>%
  arrange(desc(count))

# -- program participation by facility (all individuals have been housed at 
#    Chester -> majority of programs were done there)
program %>%
  count(pris_loc, sort = TRUE)
# =================================================================== ####
# Reorganize Variables ####
program <- reorder_vars(program)
# ================================================================= ####
# Save dataframe ####
saveRDS(program, file = "data/processed/de_identified/2_program_cleaned.Rds")
# =================================================================== ####