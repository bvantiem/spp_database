# ================================================================= ####
# Notes to Script ####
# -- Objective ####

# -- Readme ####
# House contains movements betweeen housing assignments
# Move contains movement descriptions. It helps us to identify when individuals are "Added"
# I.e. when it is a new admission 
# We don't have data on releases to the community because of the way we are pulling data.
# Always for people who are still in prison!
# -- To do  ####
# Once we have the full datapull, we should have data on releases. Add in a variable for whether individual was released
# Add in number of admissions before and after treatment for RCT sample 
# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions (delete if none) ####


# -- Read in Data ####
house <- readRDS("data/processed/de_identified/2_house_cleaned.Rds")
move <- readRDS("data/processed/de_identified/2_move_cleaned.Rds")
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds")
# ================================================================= ####
# P
# Deduplicate house ####
house_deduped <- house |>
  # Drop fully NA rows 
  mutate(na_count = rowSums(is.na(across(everything())))) |>
  filter(na_count<10) |>
  select(-na_count) |>
  # Create a group key 
  # Wave, date_datapull, loc_days_spent, loc_date_out vary across waves
  # This is very slow if we include all raw columns, too. So specifying columns to group on
  group_by(across(c(research_id, dem_hndcap, loc_bed_num, loc_bed_stat, loc_bld, loc_cell, loc_date_in, loc_date_out))) |>
  
  # Keep only the row with the earliest date_datapull in each group
  slice_min(order_by = date_datapull, with_ties = FALSE) |>
  
  ungroup()

# The last assignment within datapulls has a NA date
# We want to drop it
house_deduped2 <- house_deduped %>%
  # -- Identify the last date within a datapull
  group_by(date_datapull) %>%
  mutate(last_date_within_pull = case_when(
    loc_date_in == max(loc_date_in, na.rm = TRUE) ~ 1,
    TRUE ~ 0)) %>%
  ungroup() %>%
  group_by(research_id) %>%
  # -- We want to drop this row except when this is the last datapull an individual was a part of.
  mutate(drop = case_when(
    last_date_within_pull == 1 & is.na(loc_date_out) & date_datapull != max(date_datapull) ~ 1,
    TRUE ~ 0)) %>%
  ungroup() %>%
  filter(drop == 0) %>%
  select(-last_date_within_pull, -drop)

# -- Within data pulls, for the last cell assignment, we observe two rows in the data, with the cell number saved as "2016" and as "061", or as "1014" and "014".
# -- Delete the second observation
house_deduped3 <- house_deduped2 %>%
  group_by(research_id) %>%
  mutate(drop = case_when(
    date_datapull == max(date_datapull) & nchar(loc_cell)!=4  ~ 1,
    TRUE ~ 0)) %>%
  filter(drop == 0) %>%
  select(-drop)

# -- We sometimes observe one date_in with multiple date_outs for the same individual. When this happens, we often see someone move in and out of a cell on the same day. We delete those instances.
house_deduped4 <- house_deduped3 %>%
  group_by(research_id, loc_date_in) %>%
  mutate(n_date_ins = n()) %>%
  ungroup() %>%
  filter(!(n_date_ins > 1 & loc_date_in == loc_date_out)) |>
  select(-n_date_ins)

# Deduplicate move ####
move_deduped <- move |>
  # Create a group key 
  # Wave, date_datapull, vary across waves
  # This is very slow if we include all raw columns, too. So specifying columns to group on
  group_by(across(c(research_id, mve_date, mve_desc))) |>
  
  # Keep only the row with the earliest date_datapull in each group
  slice_min(order_by = date_datapull, with_ties = FALSE) |>
  
  ungroup()

# Get move descriptions which suggest a new admission ####
# -- All move descriptions that start with "Add" suggest a new admission
# -- Some of these are court commitments, others are parole violators 
move_deduped_admissions <- move_deduped |>
  filter(grepl("Add", mve_desc)) %>%
  group_by(research_id, mve_date) |>
  summarise(
    mve_desc = paste(unique(mve_desc), collapse = "; "),
    .groups = "drop"
  )

# Merge new admission markers into house ####
house_deduped_merged <- left_join(
  house_deduped4,
  move_deduped_admissions,
  by = c(
    "research_id" = "research_id",
    "loc_date_in" = "mve_date"
  )
)

# Merge treatment status into house ####
house_deduped_merged2 <- left_join(
  house_deduped_merged,
  randassign,
  by = c(
    "research_id" = "research_id"
  )
)

# Identify distinct sentences and prison stays - for all time, since study start, and since treatment ####
house_final <- house_deduped_merged2 %>%
  # -- Identify admission dates and sequence numbers for all time ####
  # -- -- Sort data so we can detect chronological sequences within each individual 
  arrange(research_id, loc_date_in) %>%
  group_by(research_id) %>%
  # -- -- Identify number of days between entries that someone was 'not in prison'
  mutate(days_not_in_prison = loc_date_in - lag(loc_date_out)) %>%
  # -- -- Identify if current entry is consecutive within the current sentence
  # -- -- -- It is not consecutive if there is an "Add" move description or if it is the first observation 
  mutate(consecutive_within_stay = ifelse(!is.na(mve_desc)|loc_date_in == min(loc_date_in, na.rm=T), 0, 1),
         consecutive_within_stay_and_prison = ifelse((consecutive_within_stay == 1) & (pris_loc == lag(pris_loc)|loc_date_in == min(loc_date_in, na.rm=T)), 1, 0)) %>%
  # -- -- Create sentence number [increment when not consecutive (or on first row)] 
  mutate(adm_n = cumsum(consecutive_within_stay==0)) %>%
  mutate(adm_n_total = max(adm_n)) %>%
  ungroup() %>%
  group_by(research_id, adm_n) %>%
  # -- -- Identify sentence start date 
  mutate(adm_date = min(loc_date_in)) %>%
  mutate(pris_within_adm_n = cumsum(consecutive_within_stay_and_prison==0)) %>%
  mutate(pris_within_adm_n_total = max(pris_within_adm_n)) %>%
  ungroup() %>%
  relocate(adm_n, adm_n_total, pris_within_adm_n, pris_within_adm_n_total, .before = date_datapull) %>%
  # -- Identify admission dates and sequence numbers relative to study start date ####
  # -- -- Identify if the person was in prison at the study start 
  mutate(study_start_date_between_moves = ifelse(wave1_date >= loc_date_in & wave1_date <= loc_date_out, 1, 0)) %>%
  group_by(research_id) %>%
  mutate(in_prison_at_study_start = max(study_start_date_between_moves, na.rm=T)) %>%
  select(-study_start_date_between_moves) %>%
  # -- -- For people in prison at study start, first study admission is latest admission before study start
  # -- -- For people not in prison at study start, first admission is first admission after study start 
  mutate(
    adm_study = case_when(
      in_prison_at_study_start == 1 ~ {
        dates <- adm_date[adm_date < wave1_date]
        if (length(dates) > 0) max(dates, na.rm = TRUE) else as.Date(NA)
      },
      in_prison_at_study_start == 0 ~ {
        dates <- adm_date[adm_date > wave1_date]
        if (length(dates) > 0) min(dates, na.rm = TRUE) else as.Date(NA)
      },
      TRUE ~ as.Date(NA)
    )
  ) %>%
  ungroup() %>%
  # -- -- Identify the adm_n that corresponds to adm_study
  mutate(ref_sentence = ifelse(adm_date == adm_study, adm_n, 0)) %>%
  group_by(research_id) %>%
  mutate(ref_sentence = max(ref_sentence)) %>%
  # -- -- Normalize admission numbers relative to study reference admission
  mutate(adm_study_start_rel_n = adm_n - ref_sentence) |>
  # -- -- Calculate number of admissions before and after study start. 
  # -- -- -- After study start = 0 if the there is no NEW admission after the study start date
  mutate(adm_study_start_after_n_total = max(adm_study_start_rel_n)) %>%
  mutate(adm_study_start_before_n_total = ifelse(adm_study_start_rel_n<0, abs(adm_study_start_rel_n), 0)) %>%
  mutate(adm_study_start_before_n_total = max(adm_study_start_before_n_total)) %>%
  select(-ref_sentence) %>%
  # -- Identify admission dates and sequence numbers relative to RCT treatment date 
  mutate(
    adm_rct = case_when(
      rct %in% c(0,1) ~ {
        dates <- adm_date[adm_date < rct_treat_dt]
        if (length(dates) > 0) max(dates, na.rm = TRUE) else as.Date(NA)
      },
      TRUE ~ as.Date(NA)
    )
  ) %>%
  ungroup() %>%
  # -- -- Identify the adm_n that corresponds to adm_rct
  mutate(ref_sentence = ifelse(adm_date == adm_rct, adm_n, 0)) %>%
  group_by(research_id) %>%
  mutate(ref_sentence = max(ref_sentence)) %>%
  # -- -- Normalize admission numbers relative to rct reference admission
  mutate(adm_treatment_start_rel_n = adm_n - ref_sentence) |>
  # -- -- Calculate number of admissions before and after study start. 
  # -- -- -- After study start = 0 if the there is no NEW admission after the study start date
  mutate(adm_treatment_start_after_n_total = max(adm_treatment_start_rel_n)) %>%
  mutate(adm_treatment_start_before_n_total = ifelse(adm_treatment_start_rel_n<0, abs(adm_treatment_start_rel_n), 0)) %>%
  mutate(adm_treatment_start_before_n_total = max(adm_treatment_start_before_n_total)) %>%
  select(-ref_sentence) %>%
  ungroup()
  


# ================================================================= ####
# Reorder Variables ####
house_final <- reorder_vars(house_final)

# ================================================================= ####
# Descriptives ####
# -- Average number of admissions per person ####
house_final %>%
  distinct(research_id, adm_n_total) %>%
  summarize(avg_admissions = mean(adm_n_total, na.rm=T))

# -- Average number of prisons within admission ####
house_final |>
  distinct(research_id, adm_n, pris_within_adm_n_total) |>
  summarize(avg_prisons_within_admissions = mean(pris_within_adm_n_total, na.rm=T))

# -- Average number of sentence prior to study start sentence ####
house_final %>% 
  distinct(research_id, adm_study_start_before_n_total) %>%
  summarize(avg_prior_admissions = mean(adm_study_start_before_n_total, na.rm=T))

# ================================================================= ####
# Extract admission stats ####
admission.stats <- house_final %>%
  distinct(research_id,
           adm_date, 
           adm_n_total, 
           adm_study_start_after_n_total,
           adm_study_start_before_n_total,
           pris_within_adm_n_total, 
           in_prison_at_study_start,
           adm_treatment_start_after_n_total,
           adm_treatment_start_before_n_total)


# ================================================================= ####
# Save ####
# -- House with Admission Information ####
saveRDS(house_final, "data/processed/de_identified/2b_house_with_admissions.Rds")
# -- Admission Stats by Research ID ####
saveRDS(admission.stats, "data/processed/de_identified/2b_admissions.Rds")

  