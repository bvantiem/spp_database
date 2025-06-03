# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Clean work data 
# -- Readme ####
# -- To do ####
# ================================================================= ####
# Set up ####
# -- Prepare environment ####
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
# -- Read in Data ####
work <- readRDS("data/processed/processing_layer_2/work_masked.Rds")


# ================================================================= ####
# Rename raw variables ####
# Append _raw to all columns except specified columns
work <- work |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(work), c("research_id","date_datapull", "control_number", "wave")))

# Rename columns and put them in order
work <- work |>
  mutate(job_type = WrkAsgnmt_Tp_raw,
         job_cat_desc = Catgry_Desc_raw,
         job_hrs_daily = InmDly_Hrs_raw,
         job_sch = InmSchd_Cd_raw,
         job_field = Job_Cd_raw) |>
  mutate(pris_loc = Fac_Cd_raw) |>
  relocate(ends_with("_raw"), .after = last_col()) 

# Clean variables ####
work <- work %>%
  # Set any empty strings to NA
  mutate(across(everything(), ~ replace(., grepl("^\\s*$", .), NA))) %>%
  # JOB RELATED VARIABLES
  mutate(job_type = case_when(
    job_type == "PRI" ~ "Primary",
    job_type == "SEC" ~ "Secondary",
    job_type == "TER" ~ "Tertiary",
    TRUE ~ job_type)) %>%
  # -- induced these by on table of job_field and 
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
    job_field %in% c("INMATE ACTIVITIES", "INMATE EMPLOYMENT") ~ "Inmate Engagement",
    job_field %in% c("COUNSELORS", "PSYCHOLOGICAL SERVICES") ~ "Behavioral Health",
    job_field %in% c("DRUG AND ALCOHOL TREATMENT", "DRUG AND ALCOHOL MOU", "SUBSTANCE USE DISORDER") ~ "Substance Use Treatment",
    job_field %in% c("RELIGIOUS SERVICES") ~ "Religious Services",
    job_field %in% c("FOOD SERVICES") ~ "Food Services",
    job_field %in% c("EDUCATION SERVICES") ~ "Education",
    job_field %in% c("FIRE - SAFETY", "FORESTRY CAMP") ~ "Fire Safety/Forestry",
    job_field %in% c("MAINTENANCE AND CONSTRUCTION", "MAINTENANCE  -  CONSTRUCTION", "CI - TV Repair Shop") ~ "Maintenance/Repair",
    job_field %in% c("MEDICAL SERVICE STAFF", "OTHER MEDICAL SERVICES") ~ "Medical Services",
    job_field %in% c("SECURITY") ~ "Security",
    job_field %in% c("CI - SHIPPING AND RECEIVING") ~ "Shipping and Receiving",
    job_field %in% c("CI - TECTILES") ~ "Textiles",
    job_field %in% c("CI - TRANSPORTATION AND FREIGHT") ~ "Transportation",
    job_field %in% c("COMMUNITY WORK PROGRAMS") ~ "Community Work Programs",
    TRUE ~ "Other"
  )) %>%
  relocate(job_field_cat, .after = job_field) %>%
  # there are 1438 unique descriptions, how should I handle this?
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
comment(work$job_type) <- "Description of the type of job, no missing values, fully cleaned, created using Wrk_Asgnmt_Tp_raw and Wrk_Asgnmt_Nm_raw"
comment(work$job_cat_desc) <- "Description of job and title, no missing values, NOT FULLY CLEANED, created using Catgry_Desc_raw"
comment(work$job_hrs_daily) <- "Number of hours worked in a shift, 505 missing values and 382 NULL (887 missing), WHY?, NOT FULLY CLEANED, created using InmDly_Hrs_raw"
comment(work$job_sch) <- "Days scheduled for work, 516 missing values and 370 NULL, WHY? (886 missing) NOT FULLY CLEANED, created using InmSchd_Cd_raw"
comment(work$job_field) <- "Description of job field, no missing values, fully cleaned, created using Job_Nm_raw"
comment(work$pris_loc) <- "Facility name, no missing values, fully cleaned, created using Fac_Cd"
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
  count(research_id, name = "num_unique_jobs") %>%                 # Count roles per person
  mutate(research_id = as.character(research_id))                    
work <- work %>%
  left_join(work_summary, by = "research_id") %>%
  relocate(num_unique_jobs, .after = "job_cat_desc")
# ================================================================= ####
# Save Dataframe ####
# ================================================================= #### 