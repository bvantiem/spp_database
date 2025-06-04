# =================================================================== ####
# Notes to Script: ####
# -- Objective ####
# -- Readme ####
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
program <- readRDS("data/processed/processing_layer_2/program_masked.Rds")

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
  mutate(pris_loc = Fac_Cd_raw,) |>
  relocate(ends_with("_raw"), .after = last_col()) 
# Clean Variables ####
program <- program |> 
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) |>
  # -- Some responses were coded as NULL change this to NA
  mutate(across(where(is.character), ~ na_if(., "NULL"))) |>
  # DATES
  # -- move time for program end to new column
  mutate(prg_start_time = format(as.POSIXct(prg_start_date), format = "%H:%M:%S"),
         prg_start_date = as.Date(prg_start_date)) %>%
  mutate(prg_end_time = format(as.POSIXct(prg_end_date), format = "%H:%M:%S"),
         prg_end_date = as.Date(prg_end_date)) %>%
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
  # DUMMY VARIABLES
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
  relocate(prg_cat_vp, .after = prg_cat) %>%
  relocate(prg_cat_tc, .after = prg_cat_vp) %>%
  relocate(prg_cat_so, .after = prg_cat_tc) %>%
  relocate(prg_cat_sub, .after = prg_cat_so) %>%
  relocate(prg_cat_pv, .after = prg_cat_sub) %>%
  relocate(prg_cat_op, .after = prg_cat_pv) %>%
  relocate(prg_cat_prnt, .after = prg_cat_op) %>%
  relocate(prg_cat_batt, .after = prg_cat_prnt) %>%
  relocate(prg_cat_tforc, .after = prg_cat_batt) %>%
  relocate(prg_cat_mnt, .after = prg_cat_tforc) %>%
  relocate(prg_cat_re, .after = prg_cat_mnt) %>%
  relocate(prg_cat_oth, .after = prg_cat_re) %>%
# PRISON
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(pris_loc, .after = prg_end_date) %>%
  relocate(date_datapull, .after = pris_loc) %>%
  relocate(wave, .after = date_datapull)


# =================================================================== ####
# Add Notes to Variables ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(program$pris_loc) <- "Facility location of programming, 645/9329 NA values unknown why, created using Fac_Cd_raw"
comment(program$prg_name) <- "Name of program, no NA values, fully cleaned, created using Prgm_Nm_raw"
comment(program$prg_status) <- "Status in the program, 939/9329 NA values, created using CompletionDesc_raw and ComplOrFail_SW_raw"
comment(program$prg_eval) <- "Evaluation recieved for participant, 2348/9329 NA values, created using Eval_Des_raw"
comment(program$prg_start_date) <- "Program start date, no NA values, fully cleaned, created using Inm_StrDt_raw"
comment(program$prg_end_date) <- "Program end date, 377/9329 NA values, created using Inm_EndDt_raw"
# -- Raw Variables ####
comment(program$Prgm_Nm_raw) <- "raw data, non raw available as prg_name"
comment(program$Fac_Cd_raw) <- "raw data, non raw available as pris_loc"
comment(program$CompletionDesc_raw) <- "raw data, non raw available as prg_status"
comment(program$Inm_StrDt_raw) <- "raw data, non raw available as prg_start_date"
comment(program$Inm_EndDt_raw) <- "raw data, non raw available as prg_end_date"
comment(program$Eval_Des_raw) <- "raw data, non raw available as prg_eval"
# =================================================================== ####
# New Variables ####
# length of program participation
program <- program %>%
  mutate(
    prg_start = as.Date(prg_start),
    prg_end = as.Date(prg_end),
    prg_length_days = as.numeric(prg_end - prg_start)
  ) %>%
  relocate(prg_length_days, .after = prg_end)
# =================================================================== ####
# Save dataframe ####
# =================================================================== ####