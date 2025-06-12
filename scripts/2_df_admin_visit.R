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

# -- Read in Data ####
visit <- readRDS("data/processed/1b_visit_masked.Rds")

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
  # -- recode abrv visitor descriptions into full form descriptions
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
    vst_desc == "GSO" ~ "GrandSon",
    vst_desc == "HUS" ~ "Husband",
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
  # -- recode abrv for type of visitor into full form description
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
  relocate(date_datapull, .after = vst_end_time) %>%
  relocate(wave, .after = date_datapull)
  
# =================================================================== ####
# Add Notes to Variables ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(visit$pris_loc) <- "Facility location, 0 NA values, fully cleaned, created using Fac_Cd_raw (6/6/25)"
comment(visit$vst_desc) <- "Visitor description, 0 NA values, fully cleaned created using Rltnshp_Cd_raw, Rltnshp_Des_raw (6/6/25)"
comment(visit$vst_type) <- "Type of visitor, 0 NA values, fully cleaned created using Vstr_TpCd_raw and Vstr_TpDes_raw (6/6/25)"
comment(visit$vst_id) <- "ID number assigned to visitor, 0 NA values, fully cleaned created using Vstr_Num_raw (6/6/25)"
comment(visit$vst_start_date) <- "Start date for visit, 0 NA values, fully cleaned, created using VstEvnt_DtTm_raw (6/6/25)"
comment(visit$vst_start_time) <- "Start time of visit, 0 NA values, fully cleaned, created using VstEvnt_DtTm_raw (6/6/25)"
comment(visit$vst_end_date) <- "End date for visit, 96 NA values, unknown reason for this, created using VstEvnt_TmOut_raw (6/6/25)"
comment(visit$vst_end_time) <- "End time of visit, 96 NA values, unknown reason for this, created using VstEvnt_TmOut_raw (6/6/25)"
# -- Raw Variables ####
comment(visit$Fac_Cd_raw) <- "raw variable, non raw verison available as pris_loc (6/6/25)"
comment(visit$Vstr_Num_raw) <- "raw variable, cleaned version available as vst_id (6/6/25)"
comment(visit$Rltnshp_Cd_raw) <- "raw variable, cleaned verison available as vst_desc (6/6/25)"
comment(visit$Vstr_TpCd_raw) <- "raw variable, cleaned verison available as vst_type (6/6/25)"
comment(visit$Rltnshp_Des_raw) <- "raw variable, cleaned verison available as vst_desc (6/6/25)"
comment(visit$Vstr_TpDes_raw) <- "raw variable, cleaned verison available as vst_type (6/6/25)"
comment(visit$VstEvnt_DtTm_raw) <- "raw variable, cleaned verison available as vst_start_date and vst_start_time (6/6/25)"
comment(visit$VstEvnt_TmOut_raw) <- "raw variable, cleaned verison available as vst_end_date and vst_end_time (6/6/25)"
# =================================================================== ####
# New Variables ####
# =================================================================== ####
# Temporary Descriptive Statistics ####
# -- How many visits do people get on average? 
visit %>%
  count(research_id) %>%                     # Count visits per person
  summarise(avg_visits = mean(n))            # Calculate the average
# -- How many unique visitors do people get on average?
visit %>%
  distinct(research_id, vst_id) %>%   # Drop duplicates of same visitor visiting same person
  count(research_id) %>%                  # Count unique visitors per person
  summarise(avg_unique_visitors = mean(n))
# -- most common visit types
visit %>%
  count(vst_type, sort = TRUE)

visit %>%
  count(vst_desc, sort = TRUE)
# =================================================================== ####
# Save Dataframe ####
saveRDS(visit, file = "data/processed/2_visit_cleaned.Rds")
# =================================================================== ####