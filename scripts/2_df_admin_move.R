# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Clean move data 
# -- Readme ####
# -- -- level of observation: research_id*mve_desc*mve_date
# -- To do ####
# -- -- in mve_desc figure out what RTN, DTT, ATT refer to: minor issue bcs rare codes
# only 33 instances total
# -- -- request Move Sequence Number from PADOC
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
move <- readRDS("data/processed/1b_move_masked.Rds")
# ================================================================= ####
# Rename Raw Variables ####
move <- move |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(move), c("research_id","date_datapull", "control_number", "wave")))

move <- move %>%
  mutate(mve_date = mov_move_date_raw,
         mve_desc = mov_move_code_raw) %>%
  relocate(ends_with("_raw"), .after = last_col()) 

# Clean Variables ####
move <- move |> 
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) |>
  # -- Some responses were coded as NULL change this to NA
  mutate(across(where(is.character), ~ na_if(., "NULL"))) |>
  mutate(mve_desc = case_when(
  # -- full description of the move description codes
  # -- RTN (return?), DTT, ATT do not exist in movedescription_raw (33 entries for these)
    mve_desc == "AB" ~ "Add - Bail",
    mve_desc == "RTN" ~ "RTN - No Description Available",
    mve_desc == "ASH" ~ "Add - State Hospital",
    mve_desc == "ACT" ~ "Add - County Transfer",
    mve_desc == "APD" ~ "Add - Parole Detainee",
    mve_desc == "DTT" ~ "DTT - No Description Available",
    mve_desc == "AOPV" ~ "Add - Out-of-State Probation/Parole Violator",
    mve_desc == "DIT" ~ "Delete - In Transit",
    mve_desc == "ATT" ~ "ATT - No Description Available",
    mve_desc == "AIT" ~ "Add - In Transit",
    mve_desc == "AE" ~ "Add - Escape",
    mve_desc == "PLC" ~ "Change - Permanent Location Change",
    mve_desc == "ADET" ~ "Add - Detetioner",
    mve_desc == "AA" ~ "Add - Administrative",
    mve_desc == "DA" ~ "Delete - Administrative",
    mve_desc == "TFM" ~ "Change - From Medical Facility",
    mve_desc == "TTM" ~ "Change - To Medical Facility",
    mve_desc == "AOTH" ~ "Add - Other - Use Sparingly",
    mve_desc == "RTT" ~ "Change - Return Temporary Transfer",
    mve_desc == "STT" ~ "Change - Send Temporary Transfer",
    mve_desc == "APV" ~ "Add - Parole Violator",
    mve_desc == "D" ~ "Delete - Discharge/Delete",
    mve_desc == "AC" ~ "Add - Court Commitment",
    mve_desc == "XPT" ~ "Change - Transfer Point",
    mve_desc == "TRN" ~ "Change - To Other Institution Or CCC",
    mve_desc == "SC" ~ "Change - Status Change")) %>%
  # DATES
  # -- put in ymd formate
  mutate(mve_date = ymd(as_date(mve_date))) 
# ================================================================= ####
# Add Notes to Variables ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(move$mve_date) <- "Date of move, 0 NA values, fully cleaned, created using mov_move_date_raw (6/4/25)"
comment(move$mve_desc) <- "Description of move, 0 NA values, not fully cleaned, created using mov_move_code_raw and MoveDescription_raw (6/4/25)"
# -- Raw Variables ####
comment(move$mov_move_date_raw) <- "raw data, available in cleaned form as mve_date"
comment(move$mov_move_code_raw) <- "raw data, available in cleaned form as mve_desc"
comment(move$MoveDescription_raw) <- "raw data, available in cleaned form as mve_desc"
comment(move$mov_rec_del_flag_raw) <- "raw data, uniform `N` across all rows, cleaned verison not available"
# ================================================================= ####
# New Variables ####
# ================================================================= ####
# Temporary Descriptive Stats ####
# -- total number of admin moves per person
moves_per_person <- move %>%
  group_by(research_id) %>%
  summarize(n_moves = n())
summary(moves_per_person$n_moves)

# -- most common move types
move %>%
  count(mve_desc, sort = TRUE) %>%
  slice_head(n = 10)
# ================================================================= ####
# Reorganize Variables ####
move <- reorder_vars(move)
# ================================================================= ####
# Save Dataframe ####
saveRDS(move, file = "data/processed/2_move_cleaned.Rds")
# ================================================================= ####