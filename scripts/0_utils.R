# ======================================================================= ####
# Notes to Script #### # didn't know what exactly to write in these places
# Notes to Script: ####
# -- Objective ####
# -- Readme ####
# -- To do ####
# ======================================================================= ####
# Set Up ####
# -- Prepare Environment ####
rm(list=ls())
# -- Functions #### 
`%ni%` = Negate(`%in%`) 
# -- -- Assess Variable ####
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
# -- -- Create Dummy Vars ####
make_dummies <- function(df, var, drop_temp = TRUE) {
  var <- rlang::ensym(var)
  var_name <- rlang::as_string(var)
  
  # Built-in standardization maps
  built_in_maps <- list(
    sent_off_asca = tibble(
      original = c("Public Order", "Property", "Violent", "Drugs", "Not An Arrest"),
      simplified = c("puborder", "prop", "violent", "drugs", "nonarrest")
    ),
    sent_class = tibble(
      original = c("Commuted Life", "Definite", "Detention", "Indeterminate", "Life"),
      simplified = c("commutedlife", "definite", "detention", "indeterminate", "life")
    ),
    dem_race = tibble(
      original = c("Black", "White", "Asian", "Other", "American Indian"),
      simplified = c("black", "white", "asian", "other", "amerindian")
    ),
    test_name = tibble(
      original = c("Correctional Supervision Scale - Modified", "Hostile Interpretations Questionnaire", 
                   "Level of Service Inventory - Revised", "Risk Screen Tool", "Static 1999", 
                   "Texas Christian University Drug Screen"),
      simplified = c("cssm", "hiq", "lsir", "rst", "st99", "tcu")
    ),
    loc_unit_type = tibble(
      original = c("Verifying", "General Population", "Infirmary", "Outpatient Treatment Center", 
                   "Restrictive Housing Unit", "Therapeutic Community- General", "Therapeutic Community"),
      simplified = c("verifying", "gp", "inf", "otc", "rhu", "tcg", "tc")
    )
  )
  
  df_with_id <- df %>%
    mutate(row_id = row_number()) %>%
    mutate(!!var := as.character(!!var))
  
  # If the variable is in the mapping list, apply the standardization
  if (var_name %in% names(built_in_maps)) {
    rename_map <- built_in_maps[[var_name]]
    df_with_id <- df_with_id %>%
      left_join(rename_map, by = setNames("original", var_name)) %>%
      mutate(!!var := simplified)
  }
  
  # Lowercase everything before dummy creation
  df_with_id <- df_with_id %>%
    mutate(!!var := tolower(!!var))
  
  # Create dummy variables
  dummy_df <- df_with_id %>%
    select(row_id, !!var) %>%
    tidyr::pivot_wider(
      names_from = !!var,
      values_from = !!var,
      names_prefix = paste0(var_name, "_"),
      values_fn = length,
      values_fill = 0
    ) %>%
    mutate(across(starts_with(paste0(var_name, "_")), ~ ifelse(. > 0, 1, 0)))
  
  df_final <- df_with_id %>%
    left_join(dummy_df, by = "row_id") %>%
    select(-row_id)
  
  # Drop temporary column if used
  if (var_name %in% names(built_in_maps) && drop_temp) {
    df_final <- df_final %>%
      select(-simplified)
  }
  
  return(df_final)
}
# -- -- Reorder Vars Alphabetically ####
reorder_vars <- function(df) {
  all_vars <- names(df)
  
  # Identify variable groups
  id_var <- "research_id"
  trailing_vars <- c("date_datapull", "wave")
  raw_vars <- all_vars[grepl("_raw$", all_vars)]
  
  # Cleaned variables: all that are not in id, trailing, or raw
  clean_vars <- setdiff(all_vars, c(id_var, trailing_vars, raw_vars))
  clean_vars <- sort(clean_vars)  # Alphabetize cleaned variables
  
  # Final order
  final_order <- c(id_var, clean_vars, trailing_vars, raw_vars)
  
  # Return reordered dataframe
  df[, final_order]
}
# -- Set Seed ####
set.seed(1962)
# -- Read in Data ####
unit_mapping <- read.csv("data/raw/2_data_keys/unit_mapping.csv")
pcq_lookup <- read_xlsx("data/raw/5_pcq_survey_questions/250605_pcq_survey_questions_PA.xlsx")
# ======================================================================= ####
# Unit Mapping ####
# -- INF is not included due to small sample of this unit
unit_mapping <- unit_mapping %>%
  mutate(across(starts_with("unit_type_wave"),
                ~ case_when(
                  . == "rhu" ~ "1. Restrictive Housing",
                  . == "gp" ~ "2. General Population",
                  . == "gp-tc" ~ "3. Therapeutic Community", 
                  . == "rec" ~ "4. Recovery Unit",
                  . == "hons" ~ "5. Honor Block",
                  . == "gp-epu" ~ "5. Enhanced Privilege Unit",
                  . == "gp-senior" ~ "5. Senior Unit",
                  . == "ls" ~ "6. Little Scandinavia",
                  TRUE ~ NA_character_
                ),
                .names = "unit_type_named_{col}" # Creating new columns with names like unit_type_named_wave1, etc.
  ))

unit_mapping_nonumbers_complete <- unit_mapping %>%
  mutate(across(starts_with("unit_type_wave"),
                ~ case_when(
                  . == "rhu" ~ "Restrictive Housing",
                  . == "gp" ~ "General Population",
                  . == "gp-tc" ~ "Therapeutic Community", 
                  . == "rec" ~ "Recovery Unit",
                  . == "hons" ~ "Honor Block",
                  . == "gp-epu" ~ "Enhanced Privilege Unit",
                  . == "gp-senior" ~ "Senior Unit",
                  . == "ls" ~ "Little Scandinavia",
                  . == "inf" ~ "Inf",
                  . == "pv" ~ "Parole Violators",
                  TRUE ~ NA_character_
                ),
                .names = "unit_type_named_{col}" # Creating new columns with names like unit_type_named_wave1, etc.
  ))

unit_mapping$unit_type_nols <- with(unit_mapping, ifelse(unit %in% c("aa", "ab", "ac", "ad", "eb", "da", "db", "ca"), "gp",
                                                         ifelse(unit %in% c("bb", "bc", "bd"), "gp-tc",
                                                                ifelse(unit=="ba", "rec",
                                                                       ifelse(unit=="cb", "hons",
                                                                              ifelse(unit=="ea", "thu",
                                                                                     ifelse(unit %in% c("rhu","fa"), "rhu",
                                                                                            ifelse(unit %in% c("inf","ma"), "inf",NA))))))))

# ======================================================================= ####
# Pcq_lookup ####

pcq_lookup <- pcq_lookup %>%
  mutate(question_qno = paste0("q", question_no), .after = question_no)
pcq_lookup <- as.data.frame(pcq_lookup)

# -- For RCT Longitudinal paper ####
scale.key.longitudinal <- list(prisoners = c("q10", "q11", "q12", "q13", "q14") ,
                  staff = c("q15", "q16", "q17", "q18", "q19", "q20", "q21", "q22"), # includes procedure
                  safety = c("q26", "q29", "q30", "q31", "q32"),
                  visits = c("q139","q140", "q141", "q142", "q143", "q144"), # exclude  "q145", "q146"
                  contact = c("q135", "q136", "q137"),
                  sleep = c("q94", "q95", "q96"),
                  care = c("q111", "q112", "q119", "q120", "q121", "q122"),
                  shop = c("q99", "q100", "q101"),
                  complaints = c("q24", "q25"),
                  actsat = c("q62", "q63", "q64", "q65", "q66", "q67", "q68"),
                  actav = c("q69", "q70", "q71", "q72"),
                  reint = c("q73", "q74", "q75", "q76") ,
                  autonomy = c("q82", "q83", "q84", "q85"))

scale_names <- data.frame(scale_abbreviation = names(scale.key.longitudinal),
                          scale_theory_long = c("1. Prisoner relationships",
                                                "2. Staff-prisoner relationships and Procedural Justice",
                                                "3. Safety",
                                                "4. Satisfaction with Visits",
                                                "5. Satisfaction With Frequency of Contact",
                                                "6. Sleep Quality",
                                                "7. Quality of care",
                                                "8. Shop Quality",
                                                "9. Settlement of Complaints",
                                                "10. Satisfaction with Activities",
                                                "11. Availability of Meaningful Activities",
                                                "12. Reintegration",
                                                "13. Autonomy"))

experience.key <- list(actsat = c("q55", "q56", "q57", "q58", "q59", "q60", "q61"),
                       visits = c("q133", "q134", "q138"),
                       care = c("q115", "q116", "q117", "q118"),
                       safety = c("q43", "q44", "q45", "q46", "q47"),
                       shop = c("q107", "q108"),
                       discrimination = c("q147", "q148"))

service_use_qs <- c("q55","q56","q57","q58","q59","q61","q115","q116","q117","q118","q138" ,"q167", "q162", "q163")


# ======================================================================= ####
# Wave Dates ####
# -- dates each wave occurred to ensure standardization
wave1_date = lubridate::ymd(20220501)
wave2_date = lubridate::ymd(20221115)
wave3_date = lubridate::ymd(20230520)
wave4_date = lubridate::ymd(20231128)
wave5_date = lubridate::ymd(20240606)
wave6_date = lubridate::ymd(20241022)
wave7_date = lubridate::ymd(20250501)

# ======================================================================= #### 
# Randomization Dates ####
rand1_date <- lubridate::ymd(20220502)
rand2_date <- lubridate::ymd(20221114)
rand3_date <- lubridate::ymd(20230519)
rand4_date <- lubridate::ymd(20231127)
rand5_date <- lubridate::ymd(20240605)
rand6_date <- lubridate::ymd(20241106) # CONFIRM DATE - confirming with Jordan, email sent 20250425
rand7_date <- lubridate::ymd(20250522)
# ======================================================================= #### 
# Prison Lookup Table ####
# -- table for transforming the abr facility to full facility name. 
prison_lookup <- tribble(
  ~pris_loc,     ~pris_loc_full,
  "ALB",          "Albion",
  "BEN",          "Benner Township",
  "CAM",          "Cambridge Springs",
  "CHS",          "Chester",
  "COA",          "Coal Township",
  "DAL",         "Dallas",
  "FRA",          "Frackville",
  "FYT",          "Fayette",
  "FRS",         "Forest",
  "GRN",          "Greene",
  "HOU",          "Houtzdale",
  "HUN",          "Huntingdon",
  "LAU",          "Laurel Highlands",
  "MAH",          "Mahanoy",
  "MER",          "Mercer",
  "MUN",          "Muncy",
  "PHX",          "Phoenix",
  "PIT",          "Pittsburgh",
  "QUE",          "Quehanna Boot Camp",
  "RET",          "Retreat",
  "ROC",          "Rockview",
  "SMI",          "Smithfield",
  "SMR",          "Somerset",
  "WAM",          "Waymart",
  "GRA",          "Graterford",
  "GRE",          "Greensburg",
  "CRE",          "Cresson",
  "PNG",          "Pine Grove",
  "102",          "Philadelphia CCC Center #2",
  "104",          "Philadelphia CCC Center #4",
  "201",          "Scranton Center",
  "319",          "Riverside Center",
  "VA1",          "Virginia 1",
  "MI1",          "Michigan 1"
) 
# ======================================================================= ####