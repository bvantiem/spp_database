# Set up ---------------------------------- ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# Notes ####
# Data cleaning tools ####
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

# Load Data ####
work <- readRDS("data/processed/processing_layer_1/work.Rds")


# Rename variables ####
# Append _raw to all columns except "research_id"
work <- work |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(work), c("research_id", "date_datapull", "control_number_pull", "wave")))

# Rename columns and put them in order
work <- work %>%
  mutate(pris_work_type = Job_Nm_raw,
         pris_work_dly_hrs = InmDly_Hrs_raw,
         pris_work_start_dt = WrkAsgnmtStrt_Dt_raw,
         pris_work_end_dt = WrkAsgnmtEnd_Dt_raw,
         loc_facility = Fac_Cd_raw) %>%
  relocate(ends_with("_raw"), .after = last_col())

# Clean variables ####
cols_date <- c("WrkAsgnmtStrt_Dt_raw", "WrkAsgnmtEnd_Dt_raw")

work <- work %>%
  # Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) %>%
  # Dates
  mutate(pris_work_start_dt = ymd(as_date(pris_work_start_dt))) %>%
  mutate(pris_work_end_dt = ymd(as_date(pris_work_end_dt)))

range(work$pris_work_end_dt, na.rm=T)
