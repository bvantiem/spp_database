# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Clean work data 
# -- Readme ####
# level of observation: research_id*job_cat_desc*job_start_date
# We've done initial work to clean and standardize these fields. 
# This df contains work assignments, courses, and status'. The df is missing 
# start and end dates. We know these exist as we received them in earlier
# datapulls. We need to request start and end dates as well as talk to PADOC
# about what is contained in this df before continuing with data cleaning.
# -- To do ####
# -- -- look into inaccurate dates for job_start/end_date
# 1. Talk to PADOC about what is contained in this df.
# 2. Finish cleaning script after 1 and 2 are done
# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions ####

standardize_job_field <- function(x) {
  # Step 1: Lowercase everything
  x_clean <- tolower(x)
  
  # Step 2: Capitalize first letter of each word
  x_clean <- gsub("(^|[[:space:]])([a-z])", "\\1\\U\\2", x_clean, perl = TRUE)
  
  # Step 3: Replace exact matches (like 'Ci') back to uppercase
  x_clean <- gsub("\\bCi\\b", "CI", x_clean)
  x_clean <- gsub("\\bTv\\b", "TV", x_clean)
  
  return(x_clean)
}

remove_leading_zeros <- function(x) {
  # Remove leading zeros
  cleaned_x <- sub("^0+", "", x)
  
  # If a value contained only zeros (now an empty string), replace with "0"
  cleaned_x[grepl("^0*$", x)] <- "0"
  
  return(cleaned_x)
}
# -- Read in Data ####
work <- readRDS("data/processed/1b_work_masked.Rds")


# ================================================================= ####
# Rename raw variables ####
# Append _raw to all columns except specified columns
work <- work |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(work), c("research_id","date_datapull", "control_number", "wave")))

# Rename columns and put them in order
work <- work |>
  mutate(job_lvl = WrkAsgnmt_Tp_raw,
         job_cat_desc = Catgry_Desc_raw,
         job_hrs_daily = InmDly_Hrs_raw,
         job_sch = InmSchd_Cd_raw,
         job_field = Job_Cd_raw,
         job_start_date = WrkAsgnmtStrt_Dt_raw,
         job_end_date = WrkAsgnmtEnd_Dt_raw) |>
  mutate(pris_loc = Fac_Cd_raw) |>
  relocate(ends_with("_raw"), .after = last_col()) 

# Clean variables ####
work <- work %>%
  # Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) %>%
  # JOB RELATED VARIABLES
  # -- pull time out of test_date variable to separate into new column
  mutate(
    job_start_time = format(job_start_date, "%H:%M:%S")
  ) %>% 
  mutate(job_start_date = as.Date(job_start_date)) %>%
  mutate(
    job_end_time = format(job_end_date, "%H:%M:%S")
  ) %>%
  mutate(job_end_date = as.Date(job_end_date)) %>%
  # -- drop time as they are almost fully NA or 00:00:00
  select(-job_start_time, -job_end_time) %>%
  # -- mutate abrv description of job level into full form description
  mutate(job_lvl = case_when(
    job_lvl == "PRI" ~ "Primary",
    job_lvl == "SEC" ~ "Secondary",
    job_lvl == "TER" ~ "Tertiary",
    TRUE ~ job_lvl)) %>%
  # -- convert NULL responses into NA, reformat into numeric
  mutate(
    job_hrs_daily = na_if(job_hrs_daily, "NULL"),
    job_hrs_daily = as.numeric(job_hrs_daily)
  ) %>%
  # -- mutate abrv job_field description into full form description
  # -- -- induced using table of job_code_raw and job_nm_raw 
  mutate(job_field = case_when(
    job_field == "MSC" ~ "MISCELLANEOUS",
    job_field == "ART" ~ "ARTIST PROGRAM",
    job_field == "BUS" ~ "BUSINESS OFFICE",
    job_field == "CAF" ~ "CI - FAYETTE CI-ADMINISTRATION",
    job_field == "CBO" ~ "CI - COMMISSARY BAGGING OPERATIONS",
    job_field == "CCG" ~ "CI GRATERFORD - CARTON",
    job_field == "CCM" ~ "CI - MAHANOY COMMISSARY",
    job_field == "CGA" ~ "CI GREENE - GARMENT",
    job_field == "CIE" ~ "CI - Commissary for Inmate Employment",
    job_field == "CNS" ~ "COUNSELORS",
    job_field == "COP" ~ "CI - MAHANOY - PRECISION METAL SHOP",
    job_field == "CSH" ~ "CI HUNTINGDON - SOAP  - DETERGENT",
    job_field == "CTR" ~ "CENTRALIZED SERVICES",
    job_field == "CWP" ~ "COMMUNITY WORK PROGRAMS",
    job_field == "DAT" ~ "DRUG AND ALCOHOL TREATMENT",
    job_field == "DRM" ~ "DRUG AND ALCOHOL MOU", 
    job_field == "DRT" ~ "SUBSTANCE USE DISORDER",    
    job_field == "EDU" ~ "EDUCATION SERVICES",
    job_field == "FIR" ~ "FIRE - SAFETY",
    job_field == "FOO" ~ "FOOD SERVICES",
    job_field == "FOR" ~ "FORESTRY CAMP",
    job_field == "FPR" ~ "CI Forest Plow Restoration and Paint Shop",
    job_field == "FWP" ~ "CI FOREST WHEEL REFINISHING AND PAINT SHOP",
    job_field == "GAR" ~ "CI - GARMENT",
    job_field == "GRS" ~ "CI - GARMENTS",   
    job_field == "INA" ~ "INMATE ACTIVITIES",
    job_field == "INE" ~ "INMATE EMPLOYMENT",
    job_field == "LAN" ~ "LAUNDRY",
    job_field == "LAU" ~ "CI - LIBRARY OPERATIONS",
    job_field == "LIB" ~ "LIBRARY SERVICES",
    job_field == "MAC" ~ "MAINTENANCE AND CONSTRUCTION",
    job_field == "MAT" ~ "CI - MATTRESS",
    job_field == "MED" ~ "MEDICAL SERVICE STAFF",
    job_field == "MIP" ~ "MISCELLANEOUS INMATE PAYROLL",
    job_field == "MNT" ~ "MAINTENANCE  -  CONSTRUCTION",
    job_field == "MSC" ~ "MISCELLANEOUS",
    job_field == "OTH" ~ "OTHER MEDICAL SERVICES",
    job_field == "PGA" ~ "CI PHOENIX GARMENTS",
    job_field == "PLO" ~ "CI PHOENIX LAUNDRY OPERATIONS",
    job_field == "PMS" ~ "CI MAHANOY - PRECISION METAL SHOP",
    job_field == "PRI" ~ "CI - PRINTING",
    job_field == "PSH" ~ "CI PHOENIX SHOES",
    job_field == "PSY" ~ "PSYCHOLOGICAL SERVICES",
    job_field == "PUN" ~ "CI PHOENIX UNDERWEAR",  
    job_field == "REL" ~ "RELIGIOUS SERVICES",
    job_field == "SEC" ~ "SECURITY",
    job_field == "SHO" ~ "CI - SHOES",
    job_field == "SHP" ~ "CI - SHIPPING AND RECEIVING",
    job_field == "STC" ~ "STAFF CLOTHING",
    job_field == "TEX" ~ "CI - TECTILES",
    job_field == "TF" ~ "CI - TRANSPORTATION AND FREIGHT",
    job_field == "TGS" ~ "CI - TAGS AND SIGNS",
    job_field == "TVR" ~ "CI - TV Repair Shop",
    job_field == "UND" ~ "CI - UNDERWEAR",
    job_field == "UTL" ~ "UTILITIES",
    job_field == "WF" ~ "CI - WOOD FURNITURE",
    TRUE ~ job_field )) %>%  
  mutate(job_field = standardize_job_field(job_field)) %>%
  # create higher level categories for job_field
  mutate(job_field_cat = case_when(
    job_field %in% c("ARTIST PROGRAM") ~ "Arts Program",
    job_field %in% c("CI - FAYETTE CI-ADMINISTRATION", "CENTRALIZED SERVICES", "BUSINESS OFFICE") ~ "Administration",
    job_field %in% c("CI - COMMISSARY BAGGING OPERATIONS", "CI - Commissary for Inmate Employment", "CI - MAHANOY COMMISSARY") ~ "Commissary Services",
    job_field %in% c("CI GRATERFORD - CARTON", "CI GREENE - GARMENT", "CI - GARMENT", "CI - GARMENTS", "CI PHOENIX GARMENTS", 
                    "LAUNDRY", "CI PHOENIX LAUNDRY OPERATIONS", "CI - SHOES", "CI PHOENIX SHOES", "CI - UNDERWEAR", 
                    "CI PHOENIX UNDERWEAR", "STAFF CLOTHING") ~ "Garments/Laundry Services",
    job_field %in% c("CI - MAHANOY - PRECISION METAL SHOP", "CI MAHANOY - PRECISION METAL SHOP", "CI Forest Plow Restoration and Paint Shop", 
                    "CI FOREST WHEEL REFINISHING AND PAINT SHOP", "CI - WOOD FURNITURE") ~ "Metal/Wood/Paint Shops",
    job_field %in% c("CI HUNTINGDON - SOAP  - DETERGENT", "CI - MATTRESS") ~ "Soft Goods Manufacturing",
    job_field %in% c("CI - LIBRARY OPERATIONS", "LIBRARY SERVICES") ~ "Library Services",
    job_field %in% c("INMATE EMPLOYMENT") ~ "Inmate Employment",
    job_field %in% c("COUNSELORS", "PSYCHOLOGICAL SERVICES") ~ "Behavioral Health",
    job_field %in% c("DRUG AND ALCOHOL TREATMENT", "DRUG AND ALCOHOL MOU", "SUBSTANCE USE DISORDER") ~ "Substance Use Treatment",
    job_field %in% c("RELIGIOUS SERVICES") ~ "Religious Services",
    job_field %in% c("FOOD SERVICES") ~ "Food Services",
    job_field %in% c("EDUCATION SERVICES") ~ "Education",
    job_field %in% c("FIRE - SAFETY", "FORESTRY CAMP") ~ "Fire Safety/Forestry",
    job_field %in% c("MAINTENANCE AND CONSTRUCTION", "MAINTENANCE  -  CONSTRUCTION") ~ "Maintenance/Repair",
    job_field %in% c("MEDICAL SERVICE STAFF", "OTHER MEDICAL SERVICES") ~ "Medical Services",
    job_field %in% c("SECURITY") ~ "Security",
    job_field %in% c("CI - SHIPPING AND RECEIVING") ~ "Shipping and Receiving",
    job_field %in% c("CI - TECTILES") ~ "Textiles",
    job_field %in% c("CI - TRANSPORTATION AND FREIGHT") ~ "Transportation",
    job_field %in% c("COMMUNITY WORK PROGRAMS") ~ "Community Work Programs",
    TRUE ~ "Other"
  )) %>%
  relocate(job_field_cat, .after = job_field) %>%
  # -- fix misspelling/standardization of spacing and capitalization
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bOutpatientRecoveryUnit", "Outpatient Recovery Unit")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bEDUCTIOAN FULL TIME STUDENT\\b", "Education Full Time Student")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bPEER ASSIST\\b", "Peer Assist")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bD BLOCK INMATE AIDE\\b", "D Block Inmate Aide")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bTEACHER AIDE\\b", "Teacher Aide")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bFOOD SERVICE MIDNIGHT\\b", "Food Service Midnight")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bAISLES\\b", "Aisles")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bHOLD\\b", "Hold")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bCUSTODIAL MAINTENANCE\\b", "Custodial Maintenance")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bB BLOCK HEAVY DUTY CLEANER\\b", "B Block Heavy Duty Cleaner")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bEDUCATION PART-TIME\\b", "Education Part-Time")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bED-CUSTODIAL MAINTENANCE\\b", "ED-Custodial Maintenance")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bED-WAREHOUSE\\b", "ED-Warehouse")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bED-COMPUTER REPAIR\\b", "ED-Computer Repair")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bQUAD\\b", "Quad")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bWEST\\b", "West")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bEAST\\b", "East")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bDID NOT ACCEPT WORK OFFERED\\b", "Did Not Accept Work Offered")) %>%
  mutate(job_cat_desc = str_replace_all(job_cat_desc, "\\bANTICIPATED RELEASE\\b", "Anticipated Release")) %>%
 # PRISON LOCATION
  left_join(prison_lookup, by = "pris_loc") %>%
  select(-pris_loc) %>%
  rename(pris_loc = pris_loc_full) %>%
  relocate(pris_loc, .after = pris_loc) %>%
  relocate(ends_with("_raw"), .after = last_col()) %>%
  relocate(date_datapull, .after = pris_loc)

# ================================================================= ####
# Add Notes to Variables ####
# to view notes added use str() or comment()
# -- Cleaned Variables ####
comment(work$job_lvl) <- "Description of job level, no missing values, fully cleaned, created using Wrk_Asgnmt_Tp_raw and Wrk_Asgnmt_Nm_raw (6/10/25)"
comment(work$job_cat_desc) <- "Description of job and title, no missing values, NOT FULLY CLEANED, created using Catgry_Desc_raw"
comment(work$job_hrs_daily) <- "Number of hours worked in a shift, 505 missing values and 382 NULL (887 missing), WHY?, NOT FULLY CLEANED, created using InmDly_Hrs_raw"
comment(work$job_sch) <- "Days scheduled for work, 516 missing values and 370 NULL, WHY? (886 missing) NOT FULLY CLEANED, created using InmSchd_Cd_raw"
comment(work$job_field) <- "Description of job field, no missing values, fully cleaned, created using Job_Nm_raw"
comment(work$pris_loc) <- "Facility name, no missing values, fully cleaned, created using Fac_Cd"
comment(work$job_start_date) <- "Start date of work assignemnt, no missing values, created using WrkAsgnmtStart_Dt_raw (6/13/25)"
comment(work$job_end_date) <- "End date of work assignment, 8239/79182 missing values, created using WrkAsgnmtEnd_Dt_raw (6/13/25)"

# -- Raw Variables ####
comment(work$WrkAsgnmt_Tp_raw)
comment(work$WrkAsgnmt_Nm_raw)
comment(work$JobCatgry_Cd_raw)
comment(work$Catgry_Desc_raw)
comment(work$InmDly_Hrs_raw)
comment(work$InmSchd_Cd_raw)
comment(work$Job_Cd_raw)
comment(work$Job_Nm_raw)
comment(work$Fac_Cd)
# ================================================================= ####
# New Variables ####
# number of jobs for one research_id
work_summary <- work %>%
  filter(!is.na(job_cat_desc), job_cat_desc != "Unassigned") %>%  # Exclude missing or unassigned
  distinct(research_id, job_cat_desc) %>%                         # Keep only unique roles per person
  count(research_id, name = "job_num_of_jobs") %>%                 # Count roles per person
  mutate(research_id = as.character(research_id))   
# what is work_summary?
work <- work %>%
  left_join(work_summary, by = "research_id")
# ================================================================= ####
# Temporary Descriptive Stats ####
# -- number of jobs per unique control_number
jobs_per_person <- work %>%
  group_by(research_id) %>%
  summarize(n_jobs = n())
summary(jobs_per_person$n_jobs)

# -- summary of daily hours worked
work %>%
  summarize(
    mean_hours = mean(job_hrs_daily, na.rm = TRUE),
    median_hours = median(job_hrs_daily, na.rm = TRUE),
    max_hours = max(job_hrs_daily, na.rm = TRUE)
  )
# -- avg daily hours worked by job category descriptions
work %>%
  group_by(job_cat_desc) %>%
  summarize(
    avg_hours = mean(job_hrs_daily, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(avg_hours))

# frequency of each job category description
work %>%
  count(job_cat_desc, sort = TRUE)
# ================================================================= ####
# Reorganize Variables ####
work <- reorder_vars(work)
# ================================================================= ####
# Save Dataframe ####
saveRDS(work, file = "data/processed/2_work_cleaned.Rds")
# ================================================================= #### 