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
  mutate(test_name = Test_Desc_raw,
         test_score = Test_Score_raw,
         test_date = Test_Dt_raw) |>
  relocate(ends_with("_raw"), .after = last_col())

# Clean variables ####
assess <- assess |>  
  mutate(test_time = format(as.POSIXct(test_date), format = "%H:%M:%S"),
  test_date = as.Date(test_date) ) |>
  select(-test_time) |> 
  mutate(test_name = case_when(
    test_name == "CSS-M" ~ "Correctional Supervision Scale - Modified",
    test_name == "ST99" ~ "Static 1999",
    test_name == "LSI-R" ~ "Level of Service Inventory - Revised",
    test_name == "TCU" ~ "Texas Christian University Drug Screen",
    test_name == "HIQ" ~ "Hostile Interpretations Questionnaire",
    test_name == "RST" ~ "Risk Screen Tool",
    TRUE ~ test_name 
  )) |>
  relocate(date_datapull, .after = test_date) |>
  relocate(wave, .after = date_datapull) |>
  
  # Create dummy variables
  # -- test_name
  mutate(
    test_name_cssm = if_else(test_name == "Criminal Sentiments Scale - Modified", 1, 0),
    test_name_st99 = if_else(test_name == "Static 99", 1, 0),
    test_name_lsir = if_else(test_name == "Level of Service Inventory - Revised", 1, 0),
    test_name_tcu  = if_else(test_name == "Texas Christian University Drug Screen", 1, 0),
    test_name_hiq  = if_else(test_name == "Hostile Interpretations Questionnaire", 1, 0),
    test_name_rst  = if_else(test_name == "Risk Screen Tool", 1, 0)) |>
  relocate(test_name_cssm, .after = test_name) |>
  relocate(test_name_st99, .after = test_name_cssm) |>
  relocate(test_name_lsir, .after = test_name_st99) |>
  relocate(test_name_tcu, .after = test_name_lsir) |>
  relocate(test_name_hiq, .after = test_name_tcu) |>
  relocate(test_name_rst, .after = test_name_hiq) |>
  # -- create test_Score variables for each test_name
  # -- -- if they took the test their score will appear if not then an NA will 
  mutate(
    test_score_cssm = if_else(test_name_cssm == 1, test_score, NA_real_),
    test_score_st99 = if_else(test_name_st99 == 1, test_score, NA_real_),
    test_score_lsir = if_else(test_name_lsir == 1, test_score, NA_real_),
    test_score_tcu  = if_else(test_name_tcu == 1,  test_score, NA_real_),
    test_score_hiq  = if_else(test_name_hiq == 1,  test_score, NA_real_),
    test_score_rst  = if_else(test_name_rst == 1,  test_score, NA_real_)
  )|>
  relocate(test_score_cssm, .after = test_name_rst) |>
  relocate(test_score_st99, .after = test_score_cssm) |>
  relocate(test_score_lsir, .after = test_score_st99) |>
  relocate(test_score_tcu, .after = test_score_lsir) |>
  relocate(test_score_hiq, .after = test_score_tcu) |>
  relocate(test_score_rst, .after = test_score_hiq)

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
# Temporary Descriptive Statistics ####
# -- How many tests do people take?
assess %>%
  count(research_id) %>%
  summarise(avg_tests_per_person = mean(n))
# The average person takes approx 13.15 tests
# -- some people take a test multiple times, how many unique tests do people take?
assess %>%
  distinct(research_id, test_name) %>%   # Keep only unique test-person combos
  count(research_id) %>%                 # Count unique tests per person
  summarise(avg_unique_tests = mean(n))  # Compute average
# The average number of unique tests someone takes is approx 3.69
# -- Distribution of test scores for each different test
assess %>%
  group_by(test_name) %>%
  summarise(
    avg_score = mean(test_score, na.rm = TRUE),
    min_score = min(test_score, na.rm = TRUE),
    max_score = max(test_score, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(avg_score))
# =================================================================== ####
# Save Dataframe ####
# =================================================================== ####