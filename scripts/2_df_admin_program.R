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
         prg_start = Inm_StrDt_raw,
         prg_end = Inm_EndDt_raw) |>
  mutate(pris_loc = Fac_Cd_raw,) |>
  relocate(ends_with("_raw"), .after = last_col()) 
# Clean Variables ####
program <- program |> 
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) |>
  # -- Some responses were coded as NULL change this to NA
  mutate(across(where(is.character), ~ na_if(., "NULL"))) |>
  # DATES
  # --look into time variables
  # mutate(prg_end = as.Date(prg_end) ) |>
  # PROGRAM
  # -- standardize program names
  mutate(prg_name = case_when(
    prg_name == "Violence Prevention - High 12"        ~ "Violence Prevention High Intensity 629",
    prg_name == "Violence Prevention - Moderate 29"     ~ "Violence Prevention Moderate Intensity 1518",
    prg_name == "Long Term Offender"                    ~ "Long Term Offenders",
    prg_name == "Sex Offender Program - Evaluation"     ~ "Sex Offender Program Evaluation",
    TRUE                                               ~ prg_name
  )) %>%
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(date_datapull, .after = prg_end) %>%
# -- create higher level program categorization
  mutate(prg_cat = case_when(
    grepl("\\bVP\\b|violence prevention", prg_name, ignore.case = TRUE) ~ "Violence Prevention",   # PRIORITY 1
    grepl("\\bSDTP\\b|sex offender", prg_name, ignore.case = TRUE) ~ "Sex Offender Program", 
    grepl("substance|drug|alcohol", prg_name, ignore.case = TRUE) ~ "Substance Abuse Program",
    TRUE ~ "Other"
  )) %>%
  relocate(prg_cat, .after = prg_name)


# =================================================================== ####
# Add Notes to Variables ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(program$pris_loc) <- "Facility location of programming, 9329 NA values unknown why, created using Fac_Cd_raw"
comment(program$prg_name) <- "Name of program, no NA values, fully cleaned, created using Prgm_Nm_raw"
comment(program$prg_status) <- "Status in the program, 939 NA values, created using CompletionDesc_raw"
comment(program$prg_eval) <- "Evaluation recieved for participant, 2348 NA values, created using Eval_Des_raw"
comment(program$prg_start) <- "Program start date, no NA values, fully cleaned, created using Inm_StrDt_raw"
comment(program$prg_end) <- "Program end date, 377 NA values, created using Inm_EndDt_raw"
# -- Raw Variables ####
comment(program$Prgm_Nm_raw) <- "raw data, non raw available as prg_name"
comment(program$Fac_Cd_raw) <- "raw data, non raw available as pris_loc"
comment(program$CompletionDesc_raw) <- "raw data, non raw available as prg_status"
comment(program$Inm_StrDt_raw) <- "raw data, non raw available as prg_start"
comment(program$Inm_EndDt_raw) <- "raw data, non raw available as prg_end"
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