# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Clean conduct data 
# -- Readme ####
# -- To do ####
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

# ensure that PA stays capitalized 
acronyms <- c("PA")
standardize_uppercase <- function(x) {
  x <- x %>%
    str_squish() %>%
    str_replace_all("\\s*-\\s*", " - ") %>%                         # Normalize dash spacing
    str_to_title()
  
  # Preserve acronyms (re-insert after title case)
  for (acro in acronyms) {
    pattern <- paste0("\\b", str_to_title(acro), "\\b")
    x <- str_replace_all(x, pattern, acro)
  }
  
  return(x)
}
# -- Read in Data ####
conduct <- readRDS("data/processed/processing_layer_2/conduct_masked.Rds")

# ================================================================= ####
# Rename Raw Variables ####
# Append _raw to all columns except specified columns
conduct <- conduct |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(conduct), c("research_id","date_datapull", "control_number", "wave"))) %>%
  mutate(cndct_num = misconduct_number_raw,
         cndct_date = misconduct_date_raw,
         cndct_chrg_desc = chrg_description_raw,
         cndct_guilty = vrdict_guilty_raw) %>%
  mutate(pris_loc = institution_raw) %>%
  relocate(ends_with("_raw"), .after = last_col())
# Clean Variables ####
conduct <- conduct %>%
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) %>%
  # DATES
  mutate(cndct_date = ymd(as_date(cndct_date))) %>%
  # MISCONDUCT
  mutate(cndct_chrg_desc = standardize_uppercase(cndct_chrg_desc)) %>%
  # -- some have 2 leading zeros, drop these for standardization
  mutate(cndct_guilty = sub("^00", "", cndct_guilty)) %>%
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(pris_loc, .after = cndct_guilty) %>%
  relocate(date_datapull, .after = pris_loc) %>%
  relocate(wave, .after = date_datapull)
# ================================================================= ####
# Add Notes to Variables ####
# -- Cleaned Variables ####
# -- Raw Variables ####
# ================================================================= ####
# New Variables ####
# ================================================================= ####
# Temporary Descriptive Stats ####
# ================================================================= ####
# Save Dataframe ####
# ================================================================= ####