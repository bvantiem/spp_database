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
house <- readRDS("data/processed/processing_layer_2/house_masked.Rds")
# -- Prison Lookup Table ####
# =================================================================== ####
# Rename Raw Variables ####
# Append _raw to all columns except specified columns
house <- house |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(house), c("research_id","date_datapull", "control_number", "wave")))

house <- house |>
  mutate(date_in = date_in_raw,
         date_out = date_out_raw) |>
  mutate(house_bld = building_raw,
         house_unit = section_raw,
         house_cell = cell_raw,
         house_bed_num = bed_number_raw,
         house_bed_type = bed_type_raw,
         house_sec_lvl = security_level_raw,
         house_unit_type = housing_status_raw,
         house_bed_stat = bed_status_raw) |>
  mutate(dem_hndcap = handicap_stat_raw) |>
  mutate(pris_loc = facility_raw,) %>%
  relocate(ends_with("_raw"), .after = last_col())

# Clean Variables ####
house <- house %>%
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) %>%
  # -- Some responses were coded as NULL change this to NA
  mutate(across(where(is.character), ~ na_if(., "NULL"))) %>%
  # DATE
  mutate(
    # -- need a leading zero for month
    date_in = str_pad(as.character(date_in), width = 8, side = "left", pad = "0"),
    date_out = str_pad(as.character(date_out), width = 8, side = "left", pad = "0"),
    date_in = mdy(date_in),
    date_out = mdy(date_out)
  ) %>%
  # DEMOGRAPHICS
  mutate(
    dem_hndcap = str_trim(toupper(dem_hndcap)),  # Clean up spacing/casing
    dem_hndcap = case_when(
      dem_hndcap == "Y" ~ 1,
      dem_hndcap == "N" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # HOUSING CHARACTERISTICS
  # some have a leading zero, drop this for standardization
  mutate(house_bed_num = sub("^0+", "", house_bed_num)) %>%
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(pris_loc, .after = house_bed_stat)

# Fully NA Rows ####
NA_rows <- house %>%
      filter(if_all(
             .cols = -c(research_id, date_datapull, wave, control_number),
             .fns = ~ is.na(.)
         ))
# =================================================================== ####
# Add Notes to Variable ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(house$date_in) <- "Date in ... facility? unit? system? 5413 missing values, created using date_in_raw"
comment(house$date_out) <- "Date out ... facility? unit? system? 10852 missing values, created using date_out_raw"
comment(house$house_bld) <- "Description of housing building, 113 NA values unknown why... look into this!, created using building_raw variable"
comment(house$house_unit) <- "Housing unit, 113 NA values... same as house_bld missing, created using section_raw"
comment(house$house_cell) <- "Cell number individual lives in, 113 NA values, created using cell_raw"
comment(house$house_bed_num) 
comment(house$house_bed_type)
comment(house$house_sec_lvl) <- "Security level of housing unit, 113 NA values, created using security_level_raw"
comment(house$house_unit_type) <- "Description of type of housing unit, 113 NA values, created using housing_status_raw"
comment(house$house_bed_stat) 
comment(house$pris_loc) <- "Facility description, 13464 NA values, created using facility_raw"
comment(house$dem_hndcap) <- "Binary variable for handicap status 1=yes 0=no, 139 NA values, created using handicap_stat_raw"
# -- Raw Variables ####
comment(house$date_in_raw) <- "raw data, non raw available as date_in"
comment(house$date_out_raw) <- "raw data, non raw available as date_out"
comment(house$facility_raw) <- "raw data, cleaned non raw available as pris_loc"
comment(house$building_raw) <- "raw data, non raw available as house_bld"
comment(house$section_raw) <- "raw data, non raw available as house_unit"
comment(house$cell_raw) <- "raw data, non raw available as house_cell"
comment(house$bed_number_raw) <- "raw data, non raw available as house_bed_num"
comment(house$bed_type_raw) <- "raw data, non raw available as house_bed_type"
comment(house$security_level_raw) <- "raw data, non raw available as house_sec_lvl"
comment(house$handicap_stat_raw) <- "raw data, non raw available as dem_hndcap"
comment(house$housing_status_raw) <- "raw data, non raw available as house_unit_type"
comment(house$bed_status_raw) <- "raw data, non raw available as house_bed_stat"
# =================================================================== ####
# New Variables ####
# =================================================================== ####
# Save Dataframe ####
# =================================================================== ####