# ================================================================= ####
# Notes to Script ####
# -- Objective ####

# -- Readme ####

# -- To do ####
# Gabby: Fix dates in original excel sheets. Assigned to Gabby on Github. Rerun 2a and 2b when done. 
# Jonas: Write code that sets NA answers to 996 when answers were non-applicable (159, 160, 161) and when we know the question was non-applicable (e.g. because they did not live on the lsu)
# Jonas: Check unit mapping mapping against q89 in each wave to confirm accuracy - particularly to identify tc units 
# Jonas: Fix survey number code. I think this goes by dates, but dates are not necessarily in order. For the time being, go by wave. Some individuals complete two surveys in one wave. The count should continue to go up, but order can be random within wave until we have our dates sorted. 
# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions (delete if none) ####
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
pcq <- readRDS("data/processed/processing_layer_2/pcq_masked.Rds")
# -- TEMP from utils ####
unit_mapping <- read.csv("data/raw/2_data_keys/unit_mapping.csv")
# -- INF is not included due to small sample of this unit
unit_mapping <- read.csv("data/raw/2_data_keys/unit_mapping.csv")
unit_mapping <- unit_mapping %>%
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

pcq_lookup <- read_xlsx("data/raw/5_pcq_survey_questions/250605_pcq_survey_questions_PA.xlsx")
pcq_lookup <- pcq_lookup %>%
  mutate(question_qno = paste0("q", question_no), .after = question_no)
pcq_lookup <- as.data.frame(pcq_lookup)
# ================================================================= ####
# Clean PCQ ####
# -- Retain raw data as colname_raw ####
pcq <- pcq |>
  bind_cols(
    pcq |>
      rename_with(~ paste0(., "_raw"))
  )

# -- Clean Variables ####

pcq <- pcq %>%
  mutate(unit = tolower(unit_raw)) %>%
  mutate(block = case_when(
      unit %in% c("aa", "ab", "ac", "ad") ~ "a",
      unit %in% c("ba", "bb", "bc", "bd") ~ "b",
      unit %in% c("ca", "cb") ~ "c",
      unit %in% c("da", "db") ~ "d",
      unit %in% c("ea", "eb") ~ "e",
      TRUE ~ unit
  ))

# -- Recoding ####
# -- -- Recode such that 5 is always the most positive answer ####
# Get variable names to recode as positive reverse 
recode <- pcq_lookup[pcq_lookup$recode_positive==1,"question_qno"]

pcq <- pcq |>
  # reverse the score: (6-)1=5,  (6-)2=4, (6-)3=3, (6-)4=2, (6-)5=1
  mutate(
    across(
      .cols = all_of(recode),
      .fns = ~ ifelse(.x %in% 1:5, 6 - .x, .x)
    )
  )

# -- -- Recode such that yes == 1, no == 0 ####
pcq <- pcq |>
  # Recode 2 (No) as 0, 1 (Yes) can stay the same
  # q77 (Do you have a release plan) has a 3 (I'm not sure) option. Recode this to 999. 
  mutate(
    across(
      .cols = all_of(c("q77", "q90", "q147", "q148", "q167")),
      .fns = ~ ifelse(.x == 2, 0, ifelse(.x == 3, 999, .x))
    )
  )

pcq <- pcq |>
  # Recode 1 (No) as 0 and 2 (Yes) as 1
  mutate(
    across(
      .cols = all_of(c("q164", "q165", "q166")),
      .fns = ~ ifelse(.x == 1, 0, ifelse(.x == 2, 1, .x))
    )
  )

# -- -- Recode NAs, No Opinion and Not Applicable ####
pcq[pcq == 0] <- 995
pcq[pcq == 999] <- NA

# Mark questions you can skip as 996 if this did not apply to an individual
# Cell sharing questions, for individuals who don't share a cell
pcq$q91 <- ifelse(pcq$q90==2 & pcq$q91==999, 996, pcq$q91)
pcq$q92 <- ifelse(pcq$q90==2 & pcq$q92==999, 996, pcq$q92)
pcq$q93 <- ifelse(pcq$q90==2 & pcq$q93==999, 996, pcq$q93)

# Food questions - for individuals who don't live on LSU
pcq$q103 <- ifelse(pcq$q89==5 & pcq$q103==999, 996, pcq$q103)
pcq$q104 <- ifelse(pcq$q89==5 & pcq$q104==999, 996, pcq$q104)
pcq$q105 <- ifelse(pcq$q89==5 & pcq$q105==999, 996, pcq$q105)
pcq$q106 <- ifelse(pcq$q89==5 & pcq$q106==999, 996, pcq$q106)


# Comparative prison questions, for individuals who have only been incarcerated in CHS
pcq$q159 <- ifelse(pcq$q158==1 & pcq$q159==999, 996, pcq$q159)
pcq$q160 <- ifelse(pcq$q158==1 & pcq$q160==999, 996, pcq$q160)
pcq$q161 <- ifelse(pcq$q158==1 & pcq$q161==999, 996, pcq$q161)

# ================================================================= ####
# New Variables 
# 
# Add variables ####
# -- Link unit to unit type ####
unit_mapping_long <- unit_mapping %>%
  pivot_longer(
    cols = starts_with("unit_type_wave"), # Select columns with unit_type_named_waveX
    names_to = "survey_wave",
    names_prefix = "unit_type_wave", # Remove the prefix to keep just the wave number
    values_to = "unit_type" # Store the value as unit_type_named
  ) %>%
  mutate(survey_wave = as.numeric(survey_wave)) %>% # Convert survey_wave to numeric
  mutate(unit_type = ifelse(unit_type=="closed", NA, unit_type)) %>%
  select(unit, survey_wave, unit_type)

# Merge unit_mapping with pcq dataframe based on 'survey_wave' and 'unit'
pcq <- pcq %>%
  left_join(unit_mapping_long, by = c("survey_wave" = "survey_wave", "unit" = "unit")) %>%
  relocate(unit_type, .after = unit) %>%
  mutate(unit_type = as.factor(unit_type))

# -- Identify survey number by research_id ####
# This code needs to be fixed, see todo. 
pcq$survey_no <- NA
pcq <- as.data.frame(pcq)

for(i in unique(pcq$research_id)){
  dates.list <- sort(pcq[pcq$research_id==i,c("date")]) 
  for(k in 1:length(dates.list)){
    pcq[which(pcq$research_id==i & pcq$date==dates.list[k]),"survey_no"] <- k
  }
}

pcq <- pcq %>%
  relocate(survey_no, .after = research_id)

# ================================================================= ####
# Save pcq_masked_clean #####
saveRDS(pcq, file = "data/processed/processing_layer_3/pcq_masked_clean.Rds")
