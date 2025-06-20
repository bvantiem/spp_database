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
pcq <- readRDS("data/processed/de_identified/2b_pcq_masked.Rds")
# ================================================================= ####
# Clean PCQ ####
# -- Retain raw data as colname_raw ####
pcq <- pcq |>
  mutate(
    across(
      .cols = setdiff(names(pcq), c("date_datapull")),
      .fns = ~ .x,
      .names = "{.col}_raw"
    )) |>
  # Convert all question columns to numeric
  mutate(
    across(
      .cols = matches("^q\\d{1,3}(_raw)?$"),
      .fns = ~ as.numeric(.x)
    )
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
  )) %>%
  rename(wave = survey_wave) %>% # Standardize with other files
  relocate(date, .after = research_id) %>%
  relocate(block, .after = unit) %>%
  relocate(wave, .after = date)

# -- -- Clean dates ####
# standardize date format
pcq$date[which(pcq$date_raw %in% c("999", "9092099"))] <- NA # 9092099, Wave 3, in unidentified stack
pcq$date <- parse_date_time(pcq$date, c("ymd", "mdy")) # wave 1 stored as ymd, wave 2 stored dates as mdy # Gabby is this still accurate? 
pcq$date <- as.Date(pcq$date)

# When dates are NA, assign most common date in that wave and unit
# -- filter and count dates by unit and wave
temp1 <- pcq %>%
  filter(!is.na(date)) %>%
  group_by(wave, unit) %>%
  count(date) %>%
  slice_max(n, with_ties = FALSE) %>%
  ungroup() %>%
  rename(most_common_date = date) %>%
  select(-n)
# -- some are unidenfied unit, get common date by only wave for these cases
temp2 <- pcq %>%
  filter(!is.na(date)) %>%
  group_by(wave) %>%
  count(date) %>%
  slice_max(n, with_ties = FALSE) %>%
  ungroup() %>%
  rename(most_common_date_wave_only = date) %>%
  select(-n)
# -- Join both to merge in most_common_date: first by unit + wave, then fallback by wave only
pcq <- pcq %>%
  left_join(temp1, by = c("wave", "unit")) %>%
  left_join(temp2, by = "wave")

# -- fill in any missing dates using unit+wave fallback or wave-only fallback
pcq <- pcq %>%
  mutate(
    most_common_date_combined = coalesce(most_common_date, most_common_date_wave_only),
    date = coalesce(date, most_common_date_combined)
  ) %>% 
  select(-most_common_date, -most_common_date_wave_only, -most_common_date_combined)

# -- Recoding ####
# -- -- Recode such that 5 is always the most positive answer ####
# Get variable names to recode as positive reverse 
recode <- pcq_lookup |>
  filter(recode_positive == 1) |>
  pull(question_qno)

# reverse the score: (6-)1=5,  (6-)2=4, (6-)3=3, (6-)4=2, (6-)5=1
pcq <- pcq |>
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
# New Variables  ####
# -- Link unit to unit type ####
unit_mapping_long <- unit_mapping_nonumbers_complete%>%
  pivot_longer(
    cols = starts_with("unit_type_wave"), # Select columns with unit_type_named_waveX
    names_to = "wave",
    names_prefix = "unit_type_wave", # Remove the prefix to keep just the wave number
    values_to = "unit_type" # Store the value as unit_type_named
  ) %>%
  mutate(wave = as.numeric(wave)) %>% # Convert survey_wave to numeric
  mutate(unit_type = ifelse(unit_type=="closed", NA, unit_type)) %>%
  select(unit, wave, unit_type)

# Merge unit_mapping_nonumbers_complete with pcq dataframe based on 'survey_wave' and 'unit'
pcq <- pcq %>%
  left_join(unit_mapping_long, by = c("wave" = "wave", "unit" = "unit")) %>%
  relocate(unit_type, .after = unit) %>%
  mutate(unit_type = as.factor(unit_type))

# -- Evaluate coherence between self-selected unit in pcq vs. admin-recorded unit ####
df_q89_vs_unit_type <- pcq %>%
  count(wave, q89, unit_type) %>%
  group_by(wave, q89) %>%
  mutate(prop = round(n / sum(n)*100,0))

figure_q89_vs_unit_type <- ggplot(df_q89_vs_unit_type, aes(x = unit_type, y = factor(q89), fill = prop)) +
  geom_tile() +
  scale_fill_gradient() +
  geom_text(label=paste0(df_q89_vs_unit_type$n, " (", df_q89_vs_unit_type$prop, "%)"), size=2, color="white") +
  labs(title = "q89 vs unit_type by n (%)", x = "unit_type", y = "q89",
       subtitle = "Each row is the self-reported unit (q89); values count of people (percent rowise) correctly distributed with unit_type show number of distributed \n across unit_type. Rows sum to 100%.") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  facet_wrap(~wave, ncol=1)

ggsave(plot=figure_q89_vs_unit_type, "output/figures/figure_q89_vs_unit_type_2.png", width = 6, height = 8, dpi = 300)

# -- Count number of surveys and waves by research_id ####
pcq <- pcq %>%
  group_by(research_id) %>%
  arrange(date, .by_group = TRUE) %>%  # TODO: Switch from 'survey_wave' to 'date' once 'date' is cleaned
  mutate(
    n_wave_max = n_distinct(wave),  # Count of unique waves participated in (1, 2, 4 = 3 / 1, 2, 2 = 2) 
    n_survey = row_number()  # Count of total surveys (by 'survey_wave' order)
  ) %>%
  mutate(n_survey_max = max(n_survey)) %>%
  ungroup() %>% 
  mutate(n_survey = ifelse(is.na(research_id), NA, n_survey)) %>% 
  mutate(n_survey_max = ifelse(is.na(research_id), NA, n_survey_max)) %>% 
  relocate(n_survey, .after = wave) %>% 
  relocate(n_survey_max, .after = n_survey) %>%
  relocate(n_wave_max, .after = n_survey_max)


# ================================================================= ####
# Save pcq_masked_clean #####
saveRDS(pcq, file = "data/processed/de_identified/3_pcq_cleaned.Rds")
