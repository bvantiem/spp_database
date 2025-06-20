# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Read in raw PCQ data
# Clean inmate IDs
# Link IDs to control numbers
# Save unmasked dataframe
# -- Readme ####
# Anon ids are set to NA
# -- To do ####
# One wave 7 ID to fix: kw90565
# Lots of missing control numbers from wave 7 - because administrative data not yet merged when I ran this code [5 June 2025]
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
# -- -- PCQ Surveys ####
# -- -- -- Wave 1 ------
# A Block
dfaa <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_aa_eg.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ab_bvt.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ac_eg.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ad_eg.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ba_eg.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_bb_eg.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_bc_eg.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_bd_eg.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ca_eg.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_cb_eg.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_da_eg.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_db_eg.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ea_eg.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_eb_eg.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# RHU Block
dfrhu <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_rhu_eg.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Other
dfother <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_other_eg.xlsx", skip=2, sheet=1)
dfother$block <- "unidentified"

# Bind Blocks together
pcq1 <- rbind(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother)
pcq1$survey_wave <- 1
rm(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother)

# -- -- -- Wave 2 -----
# A Block
dfaa <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_aa_cm.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ab_cm.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ac_cm.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ad_kw.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ba_cm.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_bb_kw.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_bc_cm.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_bd_cm.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ca_kw.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_cb_kw.xlsx", skip=2, sheet=1)
dfcb2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_cb2_cm.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb, dfcb2)
rm(dfca, dfcb, dfcb2)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_da_cm.xlsx", skip=2, sheet=1)
dfda2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_da2_cm.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_db_kw.xlsx", skip=2, sheet=1)
dfdb2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_db2_cm.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfda2, dfdb, dfdb2)
rm(dfda, dfda2, dfdb, dfdb2)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ea_cm.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_eb_kw.xlsx", skip=2, sheet=1)
dfeb2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_eb2_cm.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb, dfeb2)
rm(dfea, dfeb, dfeb2)
dfe$block <- "e"

# RHU Block
dfrhu <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_rhu_cm.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Other
dfother <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_misc_kw.xlsx", skip=2, sheet=1)
dfother$block <- "unidentified"

# Unidentified
dfunid <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_unidentified_kw.xlsx", skip=2, sheet=1)
dfunid$block <- "unidentified"

# Bind Blocks together
pcq2 <- rbind(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother, dfunid)
pcq2$survey_wave <- 2
rm(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother, dfunid)

# -- -- -- Wave 3 ####
# A Block
dfaa <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_aa_cm.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ab_cm.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ac_kw.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ad_cm.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ba_cm.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_bb_cm.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_bc_cm.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_bd_sc.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ca_cm.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_cb_kw.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_da_cm.xlsx", skip=2, sheet=1) # To Do: Fix extra rows in final data
dfda2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_da_eg.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_db_kw.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfda2, dfdb)
rm(dfda, dfda2, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ea_eg.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_eb_eg.xlsx", skip=2, sheet=1)
dfeb2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_eb_kw.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb, dfeb2)
rm(dfea, dfeb, dfeb2)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_inf_kw.xlsx", skip=2, sheet=1)
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_rhu_kw.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Unidentified
dfunid <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_unidentified_kw.xlsx", skip=2, sheet=1)
dfunid$block <- "unidentified"

# Bind Blocks together
pcq3 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu, dfunid)
pcq3$survey_wave <- 3
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu, dfunid)

# -- -- -- Wave 4 ####
# A Block
dfaa1 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_aa_sc_bs.xlsx", skip=2, sheet=1)
dfaa2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_aa_sc_part2.xlsx", skip=2, sheet=1)
dfaa <- rbind(dfaa1, dfaa2)
rm(dfaa1, dfaa2)

dfab1 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ab_kw_part1.xlsx", skip=2, sheet=1)
dfab2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ab_oc_part2.xlsx", skip=2, sheet=1)
dfab <- rbind(dfab1, dfab2)
rm(dfab1, dfab2)

dfac1 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ac_sjc.xlsx", skip=2, sheet=1)
dfac2 <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ac_sjc_part2.xlsx", skip=2, sheet=1)
dfac <- rbind(dfac1, dfac2)
rm(dfac1, dfac2)

dfad <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ad_hc_oc.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ba_sjc_oc_bs.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_bb_sjc.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_bc_hc.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_bd_sc.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ca_oc.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_cb_hc.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_da_hc.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_db_oc.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ea_bs.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_eb_bs.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_inf_bs.xlsx", skip=2, sheet=1)
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_rhu_bs.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Unidentified
dfunid <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_unidentified_bs.xlsx", skip=2, sheet=1)
dfunid$block <- "unidentified"

# Bind Blocks together
pcq4 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu, dfunid)
pcq4$survey_wave <- 4
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu, dfunid)


# -- -- -- Wave 5 ####
# A Block
dfaa <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_aa_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfab <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ab_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfac <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ac_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfad <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ad_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ba_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bb_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbc <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bc_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbd <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bd_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ca_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfcb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_cb_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_da_kw.xlsx", skip=2, sheet=1, col_types = c("text"))
dfdb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_db_kw.xlsx", skip=2, sheet=1, col_types = c("text"))
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ea_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfeb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_eb_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_inf_kw.xlsx", skip=2, sheet=1, col_types = c("text"))
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_rhu_kw.xlsx", skip=2, sheet=1, col_types = c("text"))
dfrhu$block <- "rhu"


# Bind Blocks together
pcq5 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq5$survey_wave <- 5
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)

# -- -- -- Wave 6 ####
# A Block
dfaa <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_aa_gc.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ab_gc.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ac_cl.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ad_es.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ba_lj.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_bb_mh.xlsx", skip=2, sheet=1)
# Note: Unit BC was closed at the time of this survey wave
dfbd <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_bd_mp.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbd) # BC is missing, see to do note.
rm(dfba, dfbb, dfbd) # BC is missing
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ca_lg.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_cb_mb.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_da_gc.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_db_lg.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ea_mh.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_eb_hk.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_inf_gc.xlsx", skip=2, sheet=1)
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_rhu_bs.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Bind Blocks together
pcq6 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq6$survey_wave <- 6
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)

# -- -- -- Wave 7 ####
# A Block
dfaa <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_aa_mp.xlsx", skip=2, sheet=1, col_types = c("text"))
dfab <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ab_mp.xlsx", skip=2, sheet=1, col_types = c("text"))
dfac <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ac_mh.xlsx", skip=2, sheet=1, col_types = c("text"))
dfad <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ad_mh.xlsx", skip=2, sheet=1, col_types = c("text"))
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ba_lg.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bb_lg.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbc <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bc_lg.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbd <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bd_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfb <- rbind(dfba, dfbb, dfbc, dfbd) 
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ca_jv.xlsx", skip=2, sheet=1, col_types = c("text"))
dfcb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_cb_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_da_mp.xlsx", skip=2, sheet=1, col_types = c("text"))
dfdb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_db_es.xlsx", skip=2, sheet=1, col_types = c("text"))
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ea_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfeb <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_eb_mb.xlsx", skip=2, sheet=1, col_types = c("text"))
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_inf_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data_restricted_access/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_rhu_mb.xlsx", skip=2, sheet=1, col_types = c("text"))
dfrhu$block <- "rhu"

# Bind Blocks together
pcq7 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq7$survey_wave <- 7
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)


# ================================================================= ####
# Merge waves ####
pcq <- rbind(pcq1, pcq2, pcq3, pcq4, pcq5, pcq6, pcq7)
# ================================================================= ####
# Create clean inmate_id using id_num and id_num_2 #### 
# -- Clean id_num column2 
names(pcq)[which(names(pcq)=="id_num")] <- "inmate_id"
pcq$inmate_id <- tolower(pcq$inmate_id)
pcq$id_num_2 <- tolower(pcq$id_num_2)
pcq <- pcq %>%
  relocate(inmate_id) 

# -- Set anon ids to NA 
pcq[which(pcq$id_num=="anon"), "inmate_id"] <- NA

# -- Identify invalid IDs
invalid_id1_index <- which(!grepl("^[a-zA-Z]{2}\\d{4}$", pcq$inmate_id))
invalid_id2_index <- which(!grepl("^[a-zA-Z]{2}\\d{4}$", pcq$id_num_2))

# -- Use id_num_2 when inmate_id is invalid and id_num_2 is valid 
ids_to_replace <- which(invalid_id1_index %ni% invalid_id2_index)
pcq$inmate_id[invalid_id1_index[ids_to_replace]] <- pcq$id_num_2[invalid_id1_index[ids_to_replace]]

# -- To do - correct the following IDs 
ids_that_need_review_index <- which(!grepl("^[a-zA-Z]{2}\\d{4}$", pcq$inmate_id) & !is.na(pcq$inmate_id))
pcq <- pcq[-ids_that_need_review_index, ]


# Link inmate ids on surveys to control numbers ####
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
pcq <- pcq |>
  left_join(control_long, by = "inmate_id") %>%
  relocate(control_number)

# ================================================================= ####
# Save pcq_unmasked with control numbers #####
saveRDS(pcq, file = paste0("data_restricted_access/processed/identified/2a_pcq.Rds"))
