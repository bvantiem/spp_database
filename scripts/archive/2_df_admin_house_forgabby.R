# Set up ---------------------------------- ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# Notes ####
# House for gabby

# Functions ####
remove_leading_zeros <- function(x) {
  # Remove leading zeros
  cleaned_x <- sub("^0+", "", x)

  # If a value contained only zeros (now an empty string), replace with "0"
  cleaned_x[grepl("^0*$", x)] <- "0"

  return(cleaned_x)
}

# Load Data ####
house <- readRDS("data/processed/processing_layer_1/house.Rds")
house <- house %>%
  select(-wave, -handicap_stat, -bed_status, -control_number_pull,
         -bed_number, -bed_type, -security_level, -housing_status)

saveRDS(house, file="data/temp/house_for_gabby.Rds")

# Data Manipulation ----------------------- ####
# house <- house[which(house$research_id=="rid_ad0328"),]  #047065

# Clean variables
house <- house %>%
  mutate(across(where(is.character), str_trim)) %>%
  # -- Date out is saved as either 00000000 or NA for the last assignment
  mutate(date_out = ifelse(date_out=="00000000", NA, date_out)) %>%
  # -- Sometimes dates are saved like 9012016 instead of 09012016
  mutate(date_in = ifelse(nchar(date_in)==7, paste0("0", date_in), date_in)) %>%
  mutate(date_out = ifelse(nchar(date_out)==7, paste0("0", date_out), date_out)) %>%
  # -- Dates are saved in different formats
  mutate(date_in = parse_date_time(date_in, orders = c("mdy", "ymd"))) %>%
  mutate(date_out = parse_date_time(date_out, orders = c("mdy", "ymd"))) %>%
  mutate(date_in = ymd(as_date(date_in))) %>%
  mutate(date_out = ymd(as_date(date_out))) %>%
  distinct()

# Deduplicate house
house <- house |>
  # Create a group key based on all columns EXCEPT the three exceptions
  group_by(across(-c(date_datapull))) |>

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

house.temp <- house[,c("research_id", "date_in", "date_out", "consecutive_within_stay", "sentence_no")]

# Some descriptives
# 1. Average number of sentences per person
house |>
  distinct(research_id, sentence_no) |>
  count(research_id, name = "n_sentences") |>
  summarise(avg_sentences = mean(n_sentences))

# 2. Average number of moves per sentence
house |>
  count(research_id, sentence_no, name = "n_moves") |>
  summarise(avg_moves = mean(n_moves))

# We don't have data on releases to the community because of the way we are pulling data.
# Always for people who are still in prison!



