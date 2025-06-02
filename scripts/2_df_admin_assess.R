# =================================================================== ####
# Notes to Script: ####
# -- Objective ####
# clean assessment data
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
assess <- readRDS("data/processed/processing_layer_2/assess_masked.Rds")
# =================================================================== ####
# Rename raw variables ####
assess <- assess |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(assess), c("research_id","date_datapull", "control_number", "wave")))|>
  mutate(test_desc = Test_Desc_raw,
         test_score = Test_Score_raw,
         test_date = Test_Dt_raw) |>
  relocate(ends_with("_raw"), .after = last_col())

# Clean variables ####
assess <- assess |>  
  mutate(test_time = format(as.POSIXct(test_date), format = "%H:%M:%S"),
  test_date = as.Date(test_date) ) |>
  relocate(test_time, .after = test_date)

# Fully NA rows ####
NA_rows <- assess %>%
  filter(if_all(
    .cols = -c(research_id, date_datapull, wave, control_number),
    .fns = ~ is.na(.)
  ))
# =================================================================== ####
# Add Notes to Variable ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(assess$test_desc) -> "Test taken, 59 fully NA rows, created using Test_Desc_raw"
comment(assess$test_score) -> "Score on test, 59 fully NA rows, created using Test_Score_raw"
comment(assess$test_date) -> "Date test taken, 59 fully NA rows, created using Test_Dt_raw"
comment(assess$test_time) -> "Time test taken, 59 fully NA rows, created using Test_Dt_raw"
# -- Raw Variables ####
comment(assess$Test_Desc_raw) -> "raw data, cleaned variable available as test_desc"
comment(assess$Test_Score) -> "raw data, cleaned variable available as test_score"
comment(assess$Test_Dt) -> "raw data, cleaned variable available as test_date & test_time"
# =================================================================== ####
# New Variables ####
# =================================================================== ####
# Save Dataframe ####
# =================================================================== ####