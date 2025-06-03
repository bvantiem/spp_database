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
visit <- readRDS("data/processed/processing_layer_2/visit_masked.Rds")

# =================================================================== ####
# Rename Raw Variables ####
# Append _raw to all columns except specified columns
visit <- visit |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(visit), c("research_id","date_datapull", "control_number", "wave")))

visit <- visit %>%
  mutate(vst_desc = Rltnshp_Cd_raw,
         vst_id = Vstr_Num_raw,
         vst_type = Vstr_TpCd_raw,
         vst_start_date = VstEvnt_DtTm_raw,
         vst_end_date = VstEvnt_TmOut_raw) %>%
  mutate(pris_loc = Fac_Cd_raw) %>%
  relocate(ends_with("_raw"), .after = last_col()) 
# Clean Variables ####
visit <- visit |> 
  # -- Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) |>
  # -- Some responses were coded as NULL change this to NA
  mutate(across(where(is.character), ~ na_if(., "NULL"))) |>
  mutate(vst_desc = case_when(
    vst_desc == "ATT" ~ "Attorney/Magistrate/Paralegal",
    vst_desc == "AUN" ~ "Aunt",
    vst_desc == "BRO" ~ "Brother",
    vst_desc == "DAU" ~ "Daughter",
    vst_desc == "FAT" ~ "Father",
    vst_desc == "FRN" ~ "Friend",
    vst_desc == "GDA" ~ "GrandDaughter",
    vst_desc == "GFA" ~ "GrandFather",
    vst_desc == "GMO" ~ "GrandMother",
    vst_desc == "HUS" ~ "GrandSon",
    vst_desc == "MOT" ~ "Mother",
    vst_desc == "OTR" ~ "Other Relationship",
    vst_desc == "SAU" ~ "Step-Aunt",
    vst_desc == "SBR" ~ "Step-Brother",
    vst_desc == "SDA" ~ "Step-Daughter",
    vst_desc == "SFA" ~ "Step-Father",
    vst_desc == "SIS" ~ "Sister",
    vst_desc == "SMO" ~ "Step-Mother",
    vst_desc == "SON" ~ "Son",
    vst_desc == "SPA" ~ "Spiritual/Religious Advisor",
    vst_desc == "SSI" ~ "Step-Sister",
    vst_desc == "SSO" ~ "Step-Son",
    vst_desc == "SUN" ~ "Step-Uncle",
    vst_desc == "UNC" ~ "Uncle",
    vst_desc == "WIF" ~ "Wife"
  )) %>%
  mutate(vst_type = case_when(
    vst_type == "A" ~ "Attorney or Associate",
    vst_type == "R" ~ "Regular",
    vst_type == "S" ~ "Religious Advisor")) %>%
# PRISON
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(pris_loc, .after = date_datapull) %>%
# DATES
# -- move time for program end to new column
  mutate(vst_start_time = format(as.POSIXct(vst_start_date), format = "%H:%M:%S"),
       vst_start_date = as.Date(vst_start_date)) %>%
  mutate(vst_end_time = format(as.POSIXct(vst_end_date), format = "%H:%M:%S"),
         vst_end_date = as.Date(vst_end_date)) %>%
  relocate(vst_start_time, .after = vst_start_date) %>%
  relocate(vst_end_time, .after = vst_end_date) %>%
  relocate(date_datapull, .after = vst_end_time)
  
# =================================================================== ####
# Add Notes to Variables ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(visit$pris_loc) <- "Facility location, 20120 NA values unknown reason for missing, created using Fac_Cd_raw"
comment(visit$vst_desc) <- "Visitor description, 1534 NA values unknown reason for missing, created using Rltnshp_Cd_raw, Rltnshp_Des_raw"
comment(visit$vst_type) 
comment(visit$vst_id)
comment(visit$vst_start_date) <- "Start date for visit, 0 NA values, fully cleaned, created using VstEvnt_DtTm_raw"
comment(visit$vst_start_time)
comment(visit$vst_end_date)
comment(visit$vst_end_time)
# -- Raw Variables ####
comment(visit$Fac_Cd_raw)
comment(visit$Vstr_Num_raw)
comment(visit$Rltnshp_Cd_raw)
comment(visit$Vstr_TpCd_raw)
comment(visit$Rltnshp_Des_raw)
comment(visit$Vstr_TpDes_raw)
comment(visit$VstEvnt_DtTm_raw)
comment(visit$VstEvnt_TmOut_raw)
# =================================================================== ####
# New Variables ####
# =================================================================== ####
# Save Dataframe ####
# =================================================================== ####