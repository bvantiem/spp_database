# Our goal is to identify the average number of sentences per person and the average
# number of MOVES per sentence. For now - we will count every gap of more than one
# day between moves as a new prison sentence. (in all likelihood this is inaccurate
# but we will fix it later)

# things to deal with
#     - the dataset is combined from different datapulls. some moves are included
#       in more than one datapull. we want to get rid of duplicated rows.
#     - dates are stored in different formats- fix this!
#     - you want to get to a place where you order the dataframe by individual and
#        by date_in, and then identify consecutive moves. As above, we will count
#        every gap of more than one day between moves as a new prison sentence.



###############################################################################
# Gabrielle Crain
# Movement Data
# April 11th, 2025
###############################################################################

# required packages
library(dplyr)
library(lubridate)
library(stringr)

# ------------------------ LOAD AND REMOVE DUPLICATES --------------------------
# house_data <- readRDS("C:/Users/gebca/OneDrive/Documents/SPP/house_for_gabby.rds")
house_data <- readRDS("data/temp/house_for_gabby.rds")

# remove perfect duplication
movement_df <- house_data %>% distinct()

# ------------------- FIX DATE FORMATTING (date_in and date_out) ------------------------------
# Many date formats are inconsistent (e.g., MMDDYYYY vs YYYYMMDD)

# Clean and standardize date_in
movement_df <- movement_df %>%
  mutate(
    # Replace non-date placeholders with NA
    date_in = ifelse(date_in %in% c("NULL", "0000NULL", "", "NA", "N/A", "n/a"), NA, date_in),
    date_in_padded = str_pad(date_in, width = 8, side = "left", pad = "0")
  ) %>%
  mutate(
    ymd_parsed = suppressWarnings(ymd(if_else(
      str_starts(date_in_padded, "19") | str_starts(date_in_padded, "20"),
      date_in_padded, NA_character_
    ))),
    mdy_parsed = suppressWarnings(mdy(if_else(
      !(str_starts(date_in_padded, "19") | str_starts(date_in_padded, "20")),
      date_in_padded, NA_character_
    ))),
    date_in_clean = coalesce(ymd_parsed, mdy_parsed)
  )

# Clean and standardize date_out
movement_df <- movement_df %>%
  mutate(
    date_out = ifelse(date_out %in% c("NULL", "0000NULL", "", "NA", "N/A", "n/a"), NA, date_out),
    date_out_padded = str_pad(date_out, width = 8, side = "left", pad = "0"),
    ymd_out = suppressWarnings(ymd(if_else(
      str_starts(date_out_padded, "19") | str_starts(date_out_padded, "20"),
      date_out_padded, NA_character_
    ))),
    mdy_out = suppressWarnings(mdy(if_else(
      !(str_starts(date_out_padded, "19") | str_starts(date_out_padded, "20")),
      date_out_padded, NA_character_
    ))),
    date_out_clean = coalesce(ymd_out, mdy_out)
  )

# Replace original date columns with cleaned versions
movement_df <- movement_df %>%
  select(-date_in_padded, -ymd_parsed, -mdy_parsed, -date_out_padded, -ymd_out, -mdy_out) %>%
  mutate(
    date_in = date_in_clean,
    date_out = date_out_clean
  ) %>%
  select(-date_in_clean, -date_out_clean)

# ------------------ REMOVE BADLY FORMATTED CELL VALUES ------------------
# Some rows have 1- to 2-digit cell values, which are duplicates.
# Remove any row where cell is not 4 characters long
movement_df <- movement_df %>%
  filter(nchar(cell) == 4)


# Now that all formatting is fixed, remove more complex duplications from different datapulls
movement_df <- movement_df %>%
  arrange(research_id, date_in, desc(date_out), desc(date_datapull)) %>%
  group_by(research_id, date_in, facility, building, section, cell) %>%
  slice(1) %>%
  ungroup()
# --------------------------- IDENTIFY NEW SENTENCES --------------------------
# Assigns a unique sentence_id to each period of incarceration.
# A new sentence begins if there is a gap of more than 1 day between the previous move's date_out
# and the current move's date_in.
# Exception: If two moves happen on the same day (internal cell change), they remain in the same sentence.

movement_df <- movement_df %>%
  arrange(research_id, date_in, date_out) %>%
  group_by(research_id) %>%
  group_modify(~ {
    df <- .x
    n <- nrow(df)
    sentence_id <- numeric(n)

    current_sentence <- 1
    last_date_out <- as.Date(NA)
    last_date_in <- as.Date(NA)

    for (i in seq_len(n)) {
      this_date_in <- df$date_in[i]

      if (
        is.na(last_date_out) ||
        is.na(this_date_in) ||
        as.integer(this_date_in - last_date_out) > 2  # A new sentence begins if the gap is greater than 1 full day (e.g., > 1 calendar day).
        # This avoids falsely splitting moves that occur on back-to-back days.
      ) {
        if (!is.na(last_date_in) && !is.na(this_date_in) && this_date_in == last_date_in) {
          sentence_id[i] <- current_sentence - 1
        } else {
          sentence_id[i] <- current_sentence
          current_sentence <- current_sentence + 1
        }
      } else {
        sentence_id[i] <- current_sentence - 1
      }

      last_date_out <- df$date_out[i]
      last_date_in <- this_date_in
    }

    df$sentence_id <- sentence_id
    df
  }) %>%
  ungroup()

# -------------------------- AVERAGE SENTENCES PER PERSON ---------------------
# Count unique sentence_ids per person
sentences_per_person <- movement_df %>%
  group_by(research_id) %>%
  summarise(num_sentences = n_distinct(sentence_id))

# Calculate the average number of sentences per person
mean(sentences_per_person$num_sentences, na.rm = TRUE)
# → Result: Average number of sentences per person is 3.307247

# -------------------------- AVERAGE MOVES PER SENTENCE -----------------------
# Count number of moves per sentence
moves_per_sentence <- movement_df %>%
  group_by(research_id, sentence_id) %>%
  summarise(num_moves = n(), .groups = "drop")

# Get summary of number of moves per sentence
mean(moves_per_sentence$num_moves, na.rm = TRUE)
# → Result: Average number of moves per sentence is 12.64253
