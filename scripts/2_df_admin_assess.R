# =================================================================== ####
# Notes to Script: ####
# -- Objective ####
# clean assessment data
# create dummy variables for each test/score
# -- Readme ####
# level of observation: research_id*test_name*test_date
# -- To do ####
# collect scoring for pass/fail of each test to create pass/fail variable
# =================================================================== ####
# Set up ####
# -- Prepare Environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
# -- Functions ####

# -- Read in Data ####
assess <- readRDS("data/processed/de_identified/1b_assess_masked.Rds")
# =================================================================== ####
# Rename raw variables ####
assess <- assess |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(assess), c("research_id","date_datapull", "control_number", "wave")))|>
  mutate(test_name = Test_Desc_raw,
         test_score = Test_Score_raw,
         test_date = Test_Dt_raw) |>
  # -- relocate variables appended with raw to the end of the dataframe
  relocate(ends_with("_raw"), .after = last_col())

# Clean variables ####
assess <- assess |>  
  # -- pull time out of test_date variable to separate into new column
  mutate(test_time = format(as.POSIXct(test_date), format = "%H:%M:%S"),
  test_date = as.Date(test_date) ) |>
  # -- drop new test_time variable, not a useful variable in cleaned dataset
  select(-test_time) |> 
  # -- change abrv test name into full title -> full title checked with PADOC data dictionary
  mutate(test_name = case_when(
    test_name == "CSS-M" ~ "Correctional Supervision Scale - Modified",
    test_name == "ST99" ~ "Static 1999",
    test_name == "LSI-R" ~ "Level of Service Inventory - Revised",
    test_name == "TCU" ~ "Texas Christian University Drug Screen",
    test_name == "HIQ" ~ "Hostile Interpretations Questionnaire",
    test_name == "RST" ~ "Risk Screen Tool",
    TRUE ~ test_name 
  )) |>
  # Create dummy variables
# if i use the make_dummies here it would make the dummy variable test_name_static_99... 
# do we want that? 
  # -- test_name dummy
  mutate(
    test_name_cssm = if_else(test_name == "Criminal Sentiments Scale - Modified", 1, 0),
    test_name_st99 = if_else(test_name == "Static 99", 1, 0),
    test_name_lsir = if_else(test_name == "Level of Service Inventory - Revised", 1, 0),
    test_name_tcu  = if_else(test_name == "Texas Christian University Drug Screen", 1, 0),
    test_name_hiq  = if_else(test_name == "Hostile Interpretations Questionnaire", 1, 0),
    test_name_rst  = if_else(test_name == "Risk Screen Tool", 1, 0)) |>
  # create test_Score variables for each test_name
  # -- if they took the test their score list under test_score will appear if not then an NA will 
  mutate(
    test_score_cssm = if_else(test_name_cssm == 1, test_score, NA_real_),
    test_score_st99 = if_else(test_name_st99 == 1, test_score, NA_real_),
    test_score_lsir = if_else(test_name_lsir == 1, test_score, NA_real_),
    test_score_tcu  = if_else(test_name_tcu == 1,  test_score, NA_real_),
    test_score_hiq  = if_else(test_name_hiq == 1,  test_score, NA_real_),
    test_score_rst  = if_else(test_name_rst == 1,  test_score, NA_real_)
  )

# Fully NA rows ####
NA_rows <- assess %>%
  filter(if_all(
    .cols = -c(research_id, date_datapull, wave),
    .fns = ~ is.na(.)
  ))
# =================================================================== ####
# Add Notes to Variable ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(assess$test_name) -> "Test taken, 59 fully NA rows, created using Test_Desc_raw (6/11/25)"
comment(assess$test_score) -> "Score recieved on test, 59 fully NA rows, created using Test_Score_raw (6/11/25)"
comment(assess$test_date) -> "Date test taken, 59 fully NA rows, created using Test_Dt_raw (6/11/25)"
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
# Reorganize Variables ####
assess <- reorder_vars(assess)
assess <- reorder_vars(assess)
# =================================================================== ####
# Save Dataframe ####
saveRDS(assess, file = "data/processed/de_identified/2_assess_cleaned.Rds")
# =================================================================== ####