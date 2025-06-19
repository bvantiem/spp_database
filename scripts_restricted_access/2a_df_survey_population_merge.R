# ================================================================= ####
# Notes to Script ####
# -- Objective ####

# -- Readme ####

# -- To do ####


# Get password for callsheets wave 1 (if not - re-enter the callsheets?)
# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions (delete if none) ####


# -- Read in Data ####
# -- -- Control Number - Inmate ID Lookup Table ####
control_lookup <- readRDS("data_restricted_access/processed/identified/1a_control_nos_inmate_ids.Rds")
# -- -- Callsheets ####
# -- -- -- Wave 1 ####
# Note: We lost the password for the full file. 
wave1_callsheets <- read.csv("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave1_participation/pcq_wave1_participation_ids_datapull.csv")

# -- -- -- Wave 2 ####
aa1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_aa_kw.xlsx", skip=0, sheet=1)
ab1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ab_kw.xlsx", skip=0, sheet=1)
ac1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ac_kw.xlsx", skip=0, sheet=1)
ad1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ad_kw.xlsx", skip=0, sheet=1)
ba1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ba_cm.xlsx", skip=0, sheet=1)
bb1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_bb_kw.xlsx", skip=0, sheet=1)
bc1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_bc_cm.xlsx", skip=0, sheet=1)
bd1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_bd_cm.xlsx", skip=0, sheet=1)
ca1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ca_kw.xlsx", skip=0, sheet=1)
cb1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_cb_cm.xlsx", skip=0, sheet=1)
da1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_da_cm.xlsx", skip=0, sheet=1)
db1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_db_cm.xlsx", skip=0, sheet=1)
ea1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ea_cm.xlsx", skip=0, sheet=1)
eb1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_eb_kw.xlsx", skip=0, sheet=1)
inf1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_inf_kw.xlsx", skip=0, sheet=1)
rhu1 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_rhu_kw.xlsx", skip=0, sheet=1)

wave2_callsheets <- rbind(aa1, ab1, ac1, ad1,
                          ba1, bb1, bc1, bd1,
                          ca1, cb1,
                          da1, db1,
                          ea1, eb1,
                          inf1, rhu1)

# -- -- -- -- Wave 2.5 ####
# We do not have population data for this wave 
wave2_5_callsheets <- read.csv("data_restricted_access/raw/3_surveys/3_compensation_lists/compensation4_pcq_wave2_5.csv")
# -- -- -- Wave 3 ####
aa3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_aa_kw.xlsx", skip=0, sheet=1)
ab3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ab_kw.xlsx", skip=0, sheet=1)
ac3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ac_kw.xlsx", skip=0, sheet=1)
ad3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ad_kw.xlsx", skip=0, sheet=1)
ba3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ba_kw.xlsx", skip=0, sheet=1)
bb3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bb_kw.xlsx", skip=0, sheet=1)
bc3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bc_kw.xlsx", skip=0, sheet=1)
bd3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bd_kw.xlsx", skip=0, sheet=1)
ca3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ca_kw.xlsx", skip=0, sheet=1)
cb3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_cb_kw.xlsx", skip=0, sheet=1)
da3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_da_kw.xlsx", skip=0, sheet=1)
db3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_db_cm.xlsx", skip=0, sheet=1)
ea3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ea_cm.xlsx", skip=0, sheet=1)
eb3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_eb_cm.xlsx", skip=0, sheet=1)
inf3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_inf_cm.xlsx", skip=0, sheet=1)
rhu3 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_rhu_cm.xlsx", skip=0, sheet=1)

wave3_callsheets <- rbind(aa3, ab3, ac3, ad3,
                          ba3, bb3, bc3, bd3,
                          ca3, cb3,
                          da3, db3,
                          ea3, eb3,
                          inf3, rhu3)

# -- -- -- Wave 4 ####
aa4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_aa_oc.xlsx", skip=0, sheet=1)
ab4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ab_sc.xlsx", skip=0, sheet=1)
ac4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ac_oc.xlsx", skip=0, sheet=1)
ad4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ad_sc.xlsx", skip=0, sheet=1)
ba4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ba_oc.xlsx", skip=0, sheet=1)
bb4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bb_oc.xlsx", skip=0, sheet=1)
bc4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bc_sjc.xlsx", skip=0, sheet=1)
bd4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bd_sjc.xlsx", skip=0, sheet=1)
ca4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ca_sjc.xlsx", skip=0, sheet=1)
cb4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_cb_sjc.xlsx", skip=0, sheet=1)
da4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_da_sjc.xlsx", skip=0, sheet=1)
db4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_db_sjc.xlsx", skip=0, sheet=1)
ea4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ea_sjc.xlsx", skip=0, sheet=1)
eb4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_eb_oc.xlsx", skip=0, sheet=1)
inf4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_inf_oc.xlsx", skip=0, sheet=1)
rhu4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_rhu_oc.xlsx", skip=0, sheet=1)
other4 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_other_oc.xlsx", skip=0, sheet=1)


wave4_callsheets <- rbind(aa4, ab4, ac4, ad4,
                          ba4, bb4, bc4, bd4,
                          ca4, cb4,
                          da4, db4,
                          ea4, eb4,
                          inf4, rhu4, other4)

# -- -- -- Wave 5 ####
aa5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_aa_kw.xlsx", skip=0, sheet=1)
ab5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ab_kw.xlsx", skip=0, sheet=1)
ac5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ac_kw.xlsx", skip=0, sheet=1)
ad5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ad_kw.xlsx", skip=0, sheet=1)
ba5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ba_kw.xlsx", skip=0, sheet=1)
bb5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bb_kw.xlsx", skip=0, sheet=1)
bc5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bc_kw.xlsx", skip=0, sheet=1)
bd5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bd_kw.xlsx", skip=0, sheet=1)
ca5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ca_kw.xlsx", skip=0, sheet=1)
cb5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_cb_kw.xlsx", skip=0, sheet=1)
da5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_da_kw.xlsx", skip=0, sheet=1)
db5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_db_kw.xlsx", skip=0, sheet=1)
ea5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ea_kw.xlsx", skip=0, sheet=1)
eb5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_eb_kw.xlsx", skip=0, sheet=1)
inf5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_inf_kw.xlsx", skip=0, sheet=1)
rhu5 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_rhu_kw.xlsx", skip=0, sheet=1)

wave5_callsheets <- rbind(aa5, ab5, ac5, ad5,
                          ba5, bb5, bc5, bd5,
                          ca5, cb5,
                          da5, db5,
                          ea5, eb5,
                          inf5, rhu5)

# -- -- -- Wave 6 ####
aa6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_aa_bs.xlsx", skip=0, sheet=1)
ab6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ab_bs.xlsx", skip=0, sheet=1)
ac6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ac_gc.xlsx", skip=0, sheet=1)
ad6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ad_gc.xlsx", skip=0, sheet=1)
ba6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ba_gc.xlsx", skip=0, sheet=1)
bb6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_bb_bs.xlsx", skip=0, sheet=1)
# unit bc closed
bd6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_bd_gc.xlsx", skip=0, sheet=1)
ca6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ca_bs.xlsx", skip=0, sheet=1)
cb6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_cb_gc.xlsx", skip=0, sheet=1)
da6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_da_bs.xlsx", skip=0, sheet=1)
db6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_db_gc.xlsx", skip=0, sheet=1)
ea6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ea_bs.xlsx", skip=0, sheet=1)
eb6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_eb_gc.xlsx", skip=0, sheet=1)
inf6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_inf_gc.xlsx", skip=0, sheet=1)
rhu6 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_rhu_gc.xlsx", skip=0, sheet=1)

wave6_callsheets <- rbind(aa6, ab6, ac6, ad6,
                          ba6, bb6, bd6, #bc closed
                          ca6, cb6,
                          da6, db6,
                          ea6, eb6,
                          inf6, rhu6)

# -- -- -- Wave 7 ####
aa7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_aa_gc.xlsx", skip=0, sheet=1)
ab7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ab_gc.xlsx", skip=0, sheet=1)
ac7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ac_gc.xlsx", skip=0, sheet=1)
ad7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ad_gc.xlsx", skip=0, sheet=1)
ba7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ba_gc.xlsx", skip=0, sheet=1)
bb7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bb_gc.xlsx", skip=0, sheet=1)
bc7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bc_gc.xlsx", skip=0, sheet=1)
bd7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bd_gc.xlsx", skip=0, sheet=1)
ca7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ca_gc.xlsx", skip=0, sheet=1)
cb7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_cb_gc.xlsx", skip=0, sheet=1)
da7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_da_gc.xlsx", skip=0, sheet=1)
db7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_db_gc.xlsx", skip=0, sheet=1)
ea7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ea_gc.xlsx", skip=0, sheet=1)
eb7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_eb_gc.xlsx", skip=0, sheet=1)
inf7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_inf_gc.xlsx", skip=0, sheet=1)
rhu7 <- read_xlsx("data_restricted_access/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_rhu_gc.xlsx", skip=0, sheet=1)

wave7_callsheets <- rbind(aa7, ab7, ac7, ad7,
                          ba7, bb7, bd7, bc7,
                          ca7, cb7,
                          da7, db7,
                          ea7, eb7,
                          inf7, rhu7)

# ================================================================= ####
# Add wave column ####
wave2_callsheets$survey_wave <- 2
wave3_callsheets$survey_wave <- 3
wave4_callsheets$survey_wave <- 4
wave5_callsheets$survey_wave <- 5
wave6_callsheets$survey_wave <- 6
wave7_callsheets$survey_wave <- 7

# Append population across waves ####
# Fixes to enable merging
names(wave2_callsheets)[which(names(wave2_callsheets)=="location")] <- "cell"

wave1_callsheets <- data.frame(inmate_id = tolower(unique(wave1_callsheets$ids_for_data_pull)),
                                  unit = rep(NA, length(unique(wave1_callsheets$ids_for_data_pull))), 
                                  cell = rep(NA, length(unique(wave1_callsheets$ids_for_data_pull))), 
                                  survey_wave = rep(1, length(unique(wave1_callsheets$ids_for_data_pull))))

wave2_5_callsheets <- data.frame(inmate_id = wave2_5_callsheets$inmate_id,
                               unit = wave2_5_callsheets$unit, 
                               cell = rep(NA, length(wave2_5_callsheets$inmate_id)), 
                               survey_wave = rep(2, length(wave2_5_callsheets$inmate_id)))

# Rbind 
survey_population <- rbind(wave1_callsheets,
                           wave2_callsheets,
                           wave2_5_callsheets,
                           wave3_callsheets,
                           wave4_callsheets,
                           wave5_callsheets, 
                           wave6_callsheets,
                           wave7_callsheets)


invalid_id_index <- which(!grepl("^[a-zA-Z]{2}\\d{4}$", survey_population$inmate_id))
survey_population <- survey_population[-invalid_id_index,]

# Link inmate ids to control numbers ####
# -- Pivot control_lookup to long format
control_long <- control_lookup |>
  mutate(across(.cols = starts_with("inmate_id_"),.fns = tolower)) |>
  pivot_longer(
    cols = starts_with("inmate_id_"),
    names_to = "inmate_id_col",
    values_to = "inmate_id",
    values_drop_na = TRUE  # Drops rows where second, third, etc. inmate_id is NA
  ) |>
  select(control_number, inmate_id)

# -- Join with pcq on inmate_id
survey_population <- survey_population |>
  left_join(control_long, by = "inmate_id") %>%
  relocate(control_number)

# ================================================================= ####
# Save survey_population
saveRDS(survey_population, file = paste0("data_restricted_access/processed/identified/2a_survey_population.Rds"))
