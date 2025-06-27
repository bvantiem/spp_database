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
release <- readRDS("data/processed/de_identified/1b_release_masked.Rds")

# ================================================================= ####
# Admissions ####
# -- Get move descriptions which suggest a new admission ####
# -- All move descriptions that start with "Add" suggest a new admission
# -- Some of these are court commitments, others are parole violators 
move_admissions <- move |>
  filter(grepl("Add", mve_desc)) %>%
  group_by(research_id, mve_date) |>
  summarise(
    mve_desc = paste(unique(mve_desc), collapse = "; "),
    .groups = "drop"
  )

# -- Merge new admission markers into house ####
house_merged <- left_join(
  house,
  move_admissions,
  by = c(
    "research_id" = "research_id",
    "loc_date_in" = "mve_date"
  )
)

# -- Merge treatment status into house ####
house_merged2 <- left_join(
  house_merged,
  randassign,
  by = c(
    "research_id" = "research_id"
  )
)

# -- Identify distinct sentences and prison stays - for all time, since study start, and since treatment ####
house_final <- house_merged2 %>%
# -- -- Identify admission dates and sequence numbers for all time ####
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
# -- -- Identify admission dates and sequence numbers relative to study start date and rct treatment date ####
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
# Releases ####
# -- Get move descriptions which suggest a release ####
# -- "Delete - Discharge/Delete" suggest a release 
move_releases <- move |>
  mutate(mve_desc_release = mve_desc) %>%
  select(-mve_desc) %>%
  filter(mve_desc_release == "Delete - Discharge/Delete") %>%
  group_by(research_id, mve_date) |>
  summarise(
    mve_desc_release = paste(unique(mve_desc_release), collapse = "; "),
    .groups = "drop"
  )

# -- Merge release markers into house ####
house_final_with_releases <- left_join(
  house_final,
  move_releases,
  by = c(
    "research_id" = "research_id",
    "loc_date_out" = "mve_date"
  ))

# -- Identify release dates for the rct admission ####
# -- -- This code finds an rct release date that aligns with the delete_date in the release file shared by PADOC 
house_final_with_releases <- house_final_with_releases %>%
  group_by(research_id) %>%
  mutate(after_admission = ifelse(loc_date_out > rct_treat_dt, 1, 0)) %>%
  mutate(
    rel_rct = case_when(
      !is.na(adm_rct) & 
        adm_treatment_start_rel_n == 0 &
        after_admission == 1 & 
        !is.na(mve_desc_release) ~ loc_date_out,
      TRUE ~ as.Date(NA)
    ),
    rel_rct = if (all(is.na(rel_rct))) NA_Date_ else max(rel_rct, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-after_admission)

# -- Fix release dates for selected IDs ####
# -- -- Delete second release in release file ####
duplicates <- release %>%
  count(research_id) %>%
  filter(n > 1) %>%
  pull(research_id)

release <- release %>% 
  mutate(duplicate = ifelse(research_id %in% duplicates, 1, 0)) %>%
  group_by(research_id) %>%
  mutate(drop = ifelse(duplicate == 1 & delete_date == max(delete_date), 1, 0)) %>%
  filter(drop == 0)

# -- -- Merge in release dates ####
house_final_with_releases <- left_join(house_final_with_releases,
                                       release[,c("research_id", "delete_date")],
                                       relationship = "many-to-one")


# rid_034543 move data suggests was first released on 2023-02-20 (my date)
# -- came back in as a parole violator and was then released on 2023-08-30 (the date listed by PADOC)
# -- Keep my date
# rid_877173 - I have they were released on 2024-10-01, PADOC says they were released on 2023-07-12 - PADOC is likely correct
# -- The issue is that I have two releases in the adm_treatment_start_rel_n, and my code takes the last release
# rid_033938 and rid_034614 generated an error because there is a release date in move that doesn't exist in house
# -- so my code did not merge in. PADIC is correct FIX THIS in the overall code (needs some thinking through)
house_final_with_releases <- house_final_with_releases %>% 
  mutate(rel_rct_corrected = rel_rct) %>%
  mutate(rel_rct_corrected = if_else(research_id %in% c("rid_877173", "rid_033938", "rid_034614"), delete_date, rel_rct_corrected)) %>%
  mutate(rel_rct = rel_rct_corrected) %>%
  select(-rel_rct_corrected, -delete_date)

# ================================================================= ####
# Time treated and time since admission (for RCT Sample Only) ####
house_final_with_releases_and_treatment_time <- house_final_with_releases %>% 
  mutate(rel_rct = as.Date(rel_rct),
         adm_rct = as.Date(adm_rct)) %>% # To do: Fix where we generate this variable 
  mutate(rct_mnths_to_exit = interval(rct_treat_dt, rct_exit_dt)/months(1),
         rct_mnths_to_release = interval(rct_treat_dt, rel_rct)/months(1),
         rct_mnths_since_release = interval(rel_rct, today()) / months(1),
         rct_mnths_pretreat = interval(adm_rct, rct_treat_dt) / months(1))

# ================================================================= ####
# Reorder Variables ####
house_final_with_releases_and_treatment_time <- reorder_vars(house_final_with_releases_and_treatment_time)

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
admission.stats <- house_final_with_releases_and_treatment_time %>%
  distinct(research_id,
           adm_date, 
           adm_n_total, 
           adm_study,
           adm_study_start_after_n_total,
           adm_study_start_before_n_total,
           pris_within_adm_n_total, 
           in_prison_at_study_start,
           adm_rct,
           rct,
           rct_stratum,
           rct_treat_dt,
           rct_treat_wave,
           rct_mnths_to_exit,
           rct_mnths_to_release,
           rct_mnths_since_release,
           rct_mnths_pretreat,
           adm_rct,
           adm_treatment_start_after_n_total,
           adm_treatment_start_before_n_total,
           rel_rct)

# ================================================================= ####
# Save ####
# -- House with Admission Information ####
saveRDS(house_final_with_releases_and_treatment_time, "data/processed/de_identified/2b_house_with_admissions.Rds")
# -- Admission Stats by Research ID ####
saveRDS(admission.stats, "data/processed/de_identified/2b_admissions.Rds")

# ================================================================= ####
# Discarded code ####
# # Deduplicate house ####
# house <- house |>
#   # Drop fully NA rows 
#   mutate(na_count = rowSums(is.na(across(everything())))) |>
#   filter(na_count<10) |>
#   select(-na_count) |>
#   # Create a group key 
#   # Wave, date_datapull, loc_days_spent, loc_date_out vary across waves
#   # This is very slow if we include all raw columns, too. So specifying columns to group on
#   group_by(across(c(research_id, dem_hndcap, loc_bed_num, loc_bed_stat, loc_bld, loc_cell, loc_date_in, loc_date_out))) |>
#   
#   # Keep only the row with the earliest date_datapull in each group
#   slice_min(order_by = date_datapull, with_ties = FALSE) |>
#   
#   ungroup()
# 
# # The last assignment within datapulls has a NA date
# # We want to drop it
# house_deduped2 <- house %>%
#   # -- Identify the last date within a datapull
#   group_by(date_datapull) %>%
#   mutate(last_date_within_pull = case_when(
#     loc_date_in == max(loc_date_in, na.rm = TRUE) ~ 1,
#     TRUE ~ 0)) %>%
#   ungroup() %>%
#   group_by(research_id) %>%
#   # -- We want to drop this row except when this is the last datapull an individual was a part of.
#   mutate(drop = case_when(
#     last_date_within_pull == 1 & is.na(loc_date_out) & date_datapull != max(date_datapull) ~ 1,
#     TRUE ~ 0)) %>%
#   ungroup() %>%
#   filter(drop == 0) %>%
#   select(-last_date_within_pull, -drop)
# 
# # -- Within data pulls, for the last cell assignment, we observe two rows in the data, with the cell number saved as "2016" and as "061", or as "1014" and "014".
# # -- Delete the second observation
# house_deduped3 <- house_deduped2 %>%
#   group_by(research_id) %>%
#   mutate(drop = case_when(
#     date_datapull == max(date_datapull) & nchar(loc_cell)!=4  ~ 1,
#     TRUE ~ 0)) %>%
#   filter(drop == 0) %>%
#   select(-drop)
# 
# # -- We sometimes observe one date_in with multiple date_outs for the same individual. When this happens, we often see someone move in and out of a cell on the same day. We delete those instances.
# house_deduped4 <- house_deduped3 %>%
#   group_by(research_id, loc_date_in) %>%
#   mutate(n_date_ins = n()) %>%
#   ungroup() %>%
#   filter(!(n_date_ins > 1 & loc_date_in == loc_date_out)) |>
#   select(-n_date_ins)
# 
# # Deduplicate move ####
# move <- move |>
#   # Create a group key 
#   # Wave, date_datapull, vary across waves
#   # This is very slow if we include all raw columns, too. So specifying columns to group on
#   group_by(across(c(research_id, mve_date, mve_desc))) |>
#   
#   # Keep only the row with the earliest date_datapull in each group
#   slice_min(order_by = date_datapull, with_ties = FALSE) |>
#   
#   ungroup()

# # Trouble shoot IDS who don't match in release file and housing data ####
# # Check this code worked 
# temp <- house_final_with_releases %>%
#   filter(rct==1) %>%
#   select(research_id, adm_rct, rct_treat_dt, rel_rct, delete_date) %>%
#   mutate(correct = ifelse(delete_date==rel_rct, 1, 0)) %>%
#   distinct()
# house_final_with_releases %>% 
#   filter(research_id=="rid_034543") %>%
#   select(research_id, adm_rct, rct_treat_dt, loc_date_out, mve_desc_release, mve_desc, rel_rct, delete_date) %>%
#   print(n=200)
# move %>% filter(research_id == "rid_034543")
# move_releases %>% filter(research_id == "rid_034614")
