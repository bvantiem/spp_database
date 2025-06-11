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

remove_leading_zeros <- function(x) {
  # Remove leading zeros
  cleaned_x <- sub("^0+", "", x)
  
  # If a value contained only zeros (now an empty string), replace with "0"
  cleaned_x[grepl("^0*$", x)] <- "0"
  
  return(cleaned_x)
}
# -- Read in Data ####
house <- readRDS("data/processed/processing_layer_2/house_masked.Rds")
# =================================================================== ####
# Rename Raw Variables ####
# Append _raw to all columns except specified columns
house <- house |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(house), c("research_id","date_datapull", "control_number", "wave")))

house <- house |>
  mutate(loc_date_in = date_in_raw,
         loc_date_out = date_out_raw) |>
  mutate(loc_bld = building_raw,
         loc_unit = section_raw,
         loc_cell = cell_raw,
         loc_bed_num = bed_number_raw,
         loc_bed_type = bed_type_raw,
         loc_sec_lvl = security_level_raw,
         loc_unit_type = housing_status_raw,
         loc_bed_stat = bed_status_raw) |>
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
  mutate(loc_bed_num = remove_leading_zeros(loc_bed_num)) %>%
  # -- not useful date out saved as 0, 00000000, or NA 
  mutate(loc_date_out = ifelse(loc_date_out=="00000000", NA, loc_date_out)) %>%
  mutate(loc_date_out = ifelse(loc_date_out=="0", NA, loc_date_out)) %>%
  # -- Sometimes dates are saved like 9012016 instead of 09012016
  mutate(loc_date_in = ifelse(nchar(loc_date_in)==7, paste0("0", loc_date_in), loc_date_in)) %>%
  mutate(loc_date_out = ifelse(nchar(loc_date_out)==7, paste0("0", loc_date_out), loc_date_out)) %>%
  # -- Dates are saved in different formats
  mutate(loc_date_in = parse_date_time(loc_date_in, orders = c("mdy", "ymd"))) %>%
  mutate(loc_date_out = parse_date_time(loc_date_out, orders = c("mdy", "ymd"))) %>%
  mutate(loc_date_in = ymd(as_date(loc_date_in))) %>%
  mutate(loc_date_out = ymd(as_date(loc_date_out))) %>%
  distinct() %>%
  # DEMOGRAPHICS
  mutate(
    dem_hndcap = str_trim(toupper(dem_hndcap)),
    dem_hndcap = case_when(
      dem_hndcap == "Y" ~ 1,
      dem_hndcap == "N" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # HOUSING CHARACTERISTICS
  # some have a leading zero, drop this for standardization
  mutate(loc_bed_num = sub("^0+", "", loc_bed_num)) %>%
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(pris_loc, .after = dem_hndcap) %>%
  relocate(date_datapull, .after = pris_loc) %>%
  relocate(wave, .after = date_datapull)

# Fully NA Rows ####
# -- 139 rows are fully NA except for research_id, control_number, wave, research_id
NA_rows <- house %>%
      filter(if_all(
             .cols = -c(research_id, date_datapull, wave, control_number),
             .fns = ~ is.na(.)
         ))
# =================================================================== ####
# Add Notes to Variable ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(house$loc_date_in) <- "Date in housing bed/unit/facility, 139/162819 NA values in fully NA rows, created using date_in_raw (6/11/25)"
comment(house$loc_date_out) <- "Date out housing bed/unit/facility, 12805/162819 NA values, created using date_out_raw (6/11/25)"
comment(house$loc_bld) <- "Description of housing building, 139 NA values in fully NA rows, created using building_raw variable (6/11/25)"
comment(house$loc_unit) <- "Housing unit, 139 NA values in fully NA rows, created using section_raw (6/11/25)"
comment(house$loc_cell) <- "Cell number individual lives in, 139 NA values in fully NA rows, created using cell_raw (6/11/25)"
comment(house$loc_bed_num) <- "Bed number, 139 NA values in fully NA rows, created using bed_number_raw (6/11/25)"
comment(house$loc_bed_type) <- "Bed type, all entries are 1 unsure meaning, 139 NA values in fully NA rows, created using bed_type_raw (6/11/25)"
comment(house$loc_sec_lvl) <- "Security level of housing unit, 139 NA values in fully NA rows, created using security_level_raw (6/11/25)"
comment(house$loc_unit_type) <- "Description of type of housing unit, 139 NA values in fully NA rows, created using housing_status_raw (6/11/25)"
comment(house$loc_bed_stat) <- "Bed status, most entries are 9 some are Z, 139 NA values in fully NA rows, created using housing_status_raw (6/11/25)"
comment(house$pris_loc) <- "Facility location, 139 NA values in fully NA rows, created using facility_raw (6/11/25)"
comment(house$dem_hndcap) <- "Binary variable for handicap status 1=yes 0=no, 139 NA values in fully NA rows, created using handicap_stat_raw (6/11/25)"
# -- Raw Variables ####
comment(house$date_in_raw) <- "raw data, non raw available as date_in"
comment(house$date_out_raw) <- "raw data, non raw available as date_out"
comment(house$facility_raw) <- "raw data, cleaned non raw available as pris_loc"
comment(house$building_raw) <- "raw data, non raw available as loc_bld"
comment(house$section_raw) <- "raw data, non raw available as loc_unit"
comment(house$cell_raw) <- "raw data, non raw available as loc_cell"
comment(house$bed_number_raw) <- "raw data, non raw available as loc_bed_num"
comment(house$bed_type_raw) <- "raw data, non raw available as loc_bed_type"
comment(house$security_level_raw) <- "raw data, non raw available as loc_sec_lvl"
comment(house$handicap_stat_raw) <- "raw data, non raw available as dem_hndcap"
comment(house$housing_status_raw) <- "raw data, non raw available as loc_unit_type"
comment(house$bed_status_raw) <- "raw data, non raw available as loc_bed_stat"
# =================================================================== ####
# Britte's Old Deduplication Code (to be reviewed) ####
# Deduplicate house
house <- house |>
  # Create a group key based on all columns EXCEPT the three exceptions
  group_by(across(-c(wave, date_datapull, control_number_pull))) |>
  
  # Keep only the row with the earliest date_datapull in each group
  slice_min(order_by = date_datapull, with_ties = FALSE) |>
  
  ungroup()

# The last assignment within datapulls has a NA date
# We want to drop it
house <- house %>%
  # -- Identify the last date within a datapull
  group_by(date_datapull) %>%
  mutate(last_date_within_pull = case_when(
    date_in == max(date_in, na.rm = TRUE) ~ 1,
    TRUE ~ 0)) %>%
  ungroup() %>%
  group_by(research_id) %>%
  # -- We want to drop this row except when this is the last datapull an individual was a part of.
  mutate(drop = case_when(
    last_date_within_pull == 1 & is.na(date_out) & date_datapull != max(date_datapull) ~ 1,
    TRUE ~ 0)) %>%
  ungroup() %>%
  filter(drop == 0) %>%
  select(-last_date_within_pull, -drop)

# -- Within data pulls, for the last cell assignment, we observe two rows in the data, with the cell number saved as "2016" and as "061", or as "1014" and "014".
# -- Delete the second observation
house <- house %>%
  group_by(research_id) %>%
  mutate(drop = case_when(
    date_datapull == max(date_datapull) & nchar(cell)!=4  ~ 1,
    TRUE ~ 0)) %>%
  filter(drop == 0) %>%
  select(-drop)

# -- We sometimes observe one date_in with multiple date_outs for the same individual. When this happens, we often see someone move in and out of a cell on the same day. We delete those instances.
house <- house %>%
  group_by(research_id, date_in) %>%
  mutate(n_date_ins = n()) %>%
  ungroup() %>%
  filter(!(n_date_ins > 1 & date_in == date_out)) |>
  select(-n_date_ins)

# Identify consecutive moves and create a row for each new sentence
# For now - assume every gap of more than a day is a new sentence (TO BE REFINED)
house <- house %>%
  # -- Sort data so we can detect chronological sequences within each individual
  arrange(research_id, date_in) %>%
  group_by(research_id) %>%
  # -- Identify if current entry continues directly from previous (same day or 1-day gap)
  mutate(consecutive_within_stay = date_in == lag(date_out)|date_in == lag(date_out)+1,
         consecutive_within_stay_and_prison = (consecutive_within_stay == TRUE) & (facility == lag(facility))) %>%
  # -- Create sentence number: increment when not consecutive (or on first row)
  # -- is.na(consecutive_within_stay) handles the first row per research_id
  # -- !consecutive_within_stay marks where a new sentence starts.
  mutate(sentence_no = cumsum(is.na(consecutive_within_stay) | !consecutive_within_stay)) %>%
  ungroup()
# =================================================================== ####
# New Variables ####
# =================================================================== ####
# Save Dataframe ####
saveRDS(house, file = "data/processed/2_house_cleaned.Rds")
# =================================================================== ####