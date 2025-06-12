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
control_lookup <- readRDS("data/processed/1a_control_nos_inmate_ids.Rds")
# -- -- PCQ Surveys ####
# -- -- -- Wave 1 ------
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_aa_eg.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ab_bvt.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ac_eg.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ad_eg.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ba_eg.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_bb_eg.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_bc_eg.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_bd_eg.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ca_eg.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_cb_eg.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_da_eg.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_db_eg.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_ea_eg.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_eb_eg.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_rhu_eg.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Other
dfother <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave1_survey_responses/pcq_wave1_other_eg.xlsx", skip=2, sheet=1)
dfother$block <- "unidentified"

# Bind Blocks together
pcq1 <- rbind(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother)
pcq1$survey_wave <- 1
rm(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother)

# -- -- -- Wave 2 -----
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_aa_cm.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ab_cm.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ac_cm.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ad_kw.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ba_cm.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_bb_kw.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_bc_cm.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_bd_cm.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ca_kw.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_cb_kw.xlsx", skip=2, sheet=1)
dfcb2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_cb2_cm.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb, dfcb2)
rm(dfca, dfcb, dfcb2)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_da_cm.xlsx", skip=2, sheet=1)
dfda2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_da2_cm.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_db_kw.xlsx", skip=2, sheet=1)
dfdb2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_db2_cm.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfda2, dfdb, dfdb2)
rm(dfda, dfda2, dfdb, dfdb2)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_ea_cm.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_eb_kw.xlsx", skip=2, sheet=1)
dfeb2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_eb2_cm.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb, dfeb2)
rm(dfea, dfeb, dfeb2)
dfe$block <- "e"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_rhu_cm.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Other
dfother <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_misc_kw.xlsx", skip=2, sheet=1)
dfother$block <- "unidentified"

# Unidentified
dfunid <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_unidentified_kw.xlsx", skip=2, sheet=1)
dfunid$block <- "unidentified"

# Bind Blocks together
pcq2 <- rbind(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother, dfunid)
pcq2$survey_wave <- 2
rm(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother, dfunid)

# -- -- -- Wave 3 ####
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_aa_cm.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ab_cm.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ac_kw.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ad_cm.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ba_cm.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_bb_cm.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_bc_cm.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_bd_sc.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ca_cm.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_cb_kw.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_da_cm.xlsx", skip=2, sheet=1) # To Do: Fix extra rows in final data
dfda2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_da_eg.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_db_kw.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfda2, dfdb)
rm(dfda, dfda2, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_ea_eg.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_eb_eg.xlsx", skip=2, sheet=1)
dfeb2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_eb_kw.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb, dfeb2)
rm(dfea, dfeb, dfeb2)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_inf_kw.xlsx", skip=2, sheet=1)
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_rhu_kw.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Unidentified
dfunid <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_unidentified_kw.xlsx", skip=2, sheet=1)
dfunid$block <- "unidentified"

# Bind Blocks together
pcq3 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu, dfunid)
pcq3$survey_wave <- 3
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu, dfunid)

# -- -- -- Wave 4 ####
# A Block
dfaa1 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_aa_sc_bs.xlsx", skip=2, sheet=1)
dfaa2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_aa_sc_part2.xlsx", skip=2, sheet=1)
dfaa <- rbind(dfaa1, dfaa2)
rm(dfaa1, dfaa2)

dfab1 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ab_kw_part1.xlsx", skip=2, sheet=1)
dfab2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ab_oc_part2.xlsx", skip=2, sheet=1)
dfab <- rbind(dfab1, dfab2)
rm(dfab1, dfab2)

dfac1 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ac_sjc.xlsx", skip=2, sheet=1)
dfac2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ac_sjc_part2.xlsx", skip=2, sheet=1)
dfac <- rbind(dfac1, dfac2)
rm(dfac1, dfac2)

dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ad_hc_oc.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ba_sjc_oc_bs.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_bb_sjc.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_bc_hc.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_bd_sc.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ca_oc.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_cb_hc.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_da_hc.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_db_oc.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ea_bs.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_eb_bs.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_inf_bs.xlsx", skip=2, sheet=1)
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_rhu_bs.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Unidentified
dfunid <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_unidentified_bs.xlsx", skip=2, sheet=1)
dfunid$block <- "unidentified"

# Bind Blocks together
pcq4 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu, dfunid)
pcq4$survey_wave <- 4
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu, dfunid)


# -- -- -- Wave 5 ####
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_aa_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ab_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfac <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ac_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ad_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ba_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bb_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbc <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bc_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bd_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ca_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_cb_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_da_kw.xlsx", skip=2, sheet=1, col_types = c("text"))
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_db_kw.xlsx", skip=2, sheet=1, col_types = c("text"))
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ea_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_eb_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_inf_kw.xlsx", skip=2, sheet=1, col_types = c("text"))
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_rhu_kw.xlsx", skip=2, sheet=1, col_types = c("text"))
dfrhu$block <- "rhu"


# Bind Blocks together
pcq5 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq5$survey_wave <- 5
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)

# -- -- -- Wave 6 ####
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_aa_gc.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ab_gc.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ac_cl.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ad_es.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ba_lj.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_bb_mh.xlsx", skip=2, sheet=1)
# Note: Unit BC was closed at the time of this survey wave
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_bd_mp.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbd) # BC is missing, see to do note.
rm(dfba, dfbb, dfbd) # BC is missing
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ca_lg.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_cb_mb.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_da_gc.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_db_lg.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_ea_mh.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_eb_hk.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_inf_gc.xlsx", skip=2, sheet=1)
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave6_survey_responses/pcq_wave6_rhu_bs.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Bind Blocks together
pcq6 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq6$survey_wave <- 6
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)

# -- -- -- Wave 7 ####
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_aa_mp.xlsx", skip=2, sheet=1, col_types = c("text"))
dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ab_mp.xlsx", skip=2, sheet=1, col_types = c("text"))
dfac <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ac_mh.xlsx", skip=2, sheet=1, col_types = c("text"))
dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ad_mh.xlsx", skip=2, sheet=1, col_types = c("text"))
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ba_lg.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bb_lg.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbc <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bc_lg.xlsx", skip=2, sheet=1, col_types = c("text"))
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bd_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfb <- rbind(dfba, dfbb, dfbc, dfbd) 
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ca_jv.xlsx", skip=2, sheet=1, col_types = c("text"))
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_cb_bs.xlsx", skip=2, sheet=1, col_types = c("text"))
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_da_mp.xlsx", skip=2, sheet=1, col_types = c("text"))
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_db_es.xlsx", skip=2, sheet=1, col_types = c("text"))
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ea_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_eb_mb.xlsx", skip=2, sheet=1, col_types = c("text"))
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_inf_gc.xlsx", skip=2, sheet=1, col_types = c("text"))
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_rhu_mb.xlsx", skip=2, sheet=1, col_types = c("text"))
dfrhu$block <- "rhu"

# Bind Blocks together
pcq7 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq7$survey_wave <- 7
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)


# ================================================================= ####
# Merge waves ####
pcq <- rbind(pcq1, pcq2, pcq3, pcq4, pcq5, pcq6, pcq7)
# ================================================================= ####
# Rename raw variables ####
# Append _raw to all columns except specified columns
pcq <- pcq |>
  rename_with(~ paste0(., "_raw"), .cols = setdiff(names(pcq), c("id_num","date_datapull", "unit", "wave")))
# Change variable order ####
pcq <- pcq %>%
  rename(wave = survey_wave_raw) %>%
  relocate(unit, .after = id_num) %>%
  relocate(block_raw, .after = unit) %>%
  relocate(wave, .after = block_raw) %>%
  # survey questions
  mutate(date = date_raw,
         q1 = q1_raw,
         q2 = q2_raw,
         q3 = q3_raw,
         q4 = q4_raw,
         q5 = q5_raw,
         q6 = q6_raw,
         q7 = q7_raw,
         q8 = q8_raw,
         q9 = q9_raw,
         q10 = q10_raw,
         q11 = q11_raw,
         q12 = q12_raw,
         q13 = q13_raw,
         q14 = q14_raw,
         q15 = q15_raw,
         q16 = q16_raw,
         q17 = q17_raw,
         q18 = q18_raw,
         q19 = q19_raw,
         q20 = q20_raw,
         q21 = q21_raw,
         q22 = q22_raw,
         q23 = q23_raw,
         q24 = q24_raw,
         q25 = q25_raw,
         q26 = q26_raw,
         q27 = q27_raw,
         q28 = q28_raw,
         q29 = q29_raw,
         q30 = q30_raw,
         q31 = q31_raw,
         q32 = q32_raw,
         q33 = q33_raw,
         q34 = q34_raw,
         q35 = q35_raw,
         q36 = q36_raw,
         q37 = q37_raw,
         q38 = q38_raw,
         q39 = q39_raw,
         q40 = q40_raw,
         q41 = q41_raw,
         q42 = q42_raw,
         q43 = q43_raw,
         q44 = q44_raw,
         q45 = q45_raw,
         q46 = q46_raw,
         q47 = q47_raw,
         q48 = q48_raw,
         q49 = q49_raw,
         q50 = q50_raw,
         q51 = q51_raw,
         q52 = q52_raw,
         q53 = q53_raw,
         q54 = q54_raw,
         q55 = q55_raw,
         q56 = q56_raw,
         q57 = q57_raw,
         q58 = q58_raw,
         q59 = q59_raw,
         q60 = q60_raw,
         q61 = q61_raw,
         q62 = q62_raw,
         q63 = q63_raw,
         q64 = q64_raw,
         q65 = q65_raw,
         q66 = q66_raw,
         q67 = q67_raw,
         q68 = q68_raw,
         q69 = q69_raw,
         q70 = q70_raw,
         q71 = q71_raw,
         q72 = q72_raw,
         q73 = q73_raw,
         q74 = q74_raw,
         q75 = q75_raw,
         q76 = q76_raw,
         q77 = q77_raw,
         q78 = q78_raw,
         q79 = q79_raw,
         q80 = q80_raw,
         q81 = q81_raw,
         q82 = q82_raw,
         q83 = q83_raw,
         q84 = q84_raw,
         q85 = q85_raw,
         q86 = q86_raw,
         q87 = q87_raw,
         q88 = q88_raw,
         q89 = q89_raw,
         q90 = q90_raw,
         q91 = q91_raw,
         q92 = q92_raw,
         q93 = q93_raw,
         q94 = q94_raw,
         q95 = q95_raw,
         q96 = q96_raw,
         q97 = q97_raw,
         q98 = q98_raw,
         q99 = q99_raw,
         q100 = q100_raw,
         q101 = q101_raw,
         q102 = q102_raw,
         q103 = q103_raw,
         q104 = q104_raw,
         q105 = q105_raw,
         q106 = q106_raw,
         q107 = q107_raw,
         q108 = q108_raw,
         q109 = q109_raw,
         q110 = q110_raw,
         q111 = q111_raw,
         q112 = q112_raw,
         q113 = q113_raw,
         q114 = q114_raw,
         q115 = q115_raw,
         q116 = q116_raw,
         q117 = q117_raw,
         q118 = q118_raw,
         q119 = q119_raw,
         q120 = q120_raw,
         q121 = q121_raw,
         q122 = q122_raw,
         q123 = q123_raw,
         q124 = q124_raw,
         q125 = q125_raw,
         q126 = q126_raw,
         q127 = q127_raw,
         q128 = q128_raw,
         q129 = q129_raw,
         q130 = q130_raw,
         q131 = q131_raw,
         q132 = q132_raw,
         q133 = q133_raw,
         q134 = q134_raw,
         q135 = q135_raw,
         q136 = q136_raw,
         q137 = q137_raw,
         q138 = q138_raw,
         q139 = q139_raw,
         q140 = q140_raw,
         q141 = q141_raw,
         q142 = q142_raw,
         q143 = q143_raw,
         q144 = q144_raw,
         q145 = q145_raw,
         q146 = q146_raw,
         q147 = q147_raw,
         q148 = q148_raw,
         q149 = q149_raw,
         q150 = q150_raw,
         q151 = q151_raw,
         q152 = q152_raw,
         q153 = q153_raw,
         q154 = q154_raw,
         q155 = q155_raw,
         q156 = q156_raw,
         q157 = q157_raw,
         q158 = q158_raw,
         q159 = q159_raw,
         q160 = q160_raw,
         q161 = q161_raw,
         q162 = q162_raw,
         q163 = q163_raw,
         q164 = q164_raw,
         q165 = q165_raw,
         q166 = q166_raw,
         q167 = q167_raw,
         q168 = q168_raw,
         q169 = q169_raw,
         q170 = q170_raw,
         q171 = q171_raw,
         q172 = q172_raw,
         q173 = q173_raw,
         your_comments = your_comments_raw,
         text_answer = text_answer_raw,
         notes = notes_raw) %>%
  relocate(ends_with("_raw"), .after = last_col()) 

# ================================================================= ####
# Create clean inmate_id using id_num and id_num_2 #### 
# -- Clean id_num column2 
pcq$id_num <- tolower(pcq$id_num_raw)
pcq$id_num_2 <- tolower(pcq$id_num_2_raw)
pcq$inmate_id <- pcq$id_num
pcq <- pcq %>%
  relocate(inmate_id) %>%
  relocate(id_num_raw, .after = notes_raw) %>%
  relocate(id_num_2_raw, .after = id_num_raw)

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

# Clean dates ####
# standardize date format
pcq$date[which(pcq$date_raw %in% c("999", "9092099"))] <- NA # 9092099, Wave 3, in unidentified stack
pcq$date <- parse_date_time(pcq$date_raw, c("ymd", "mdy")) # wave 1 stored as ymd, wave 2 stored dates as mdy
pcq$date <- as.Date(pcq$date)

# When dates are NA, assign most common date in that wave and unit
# -- filter and count dates by unit and wave
temp1 <- pcq %>%
  filter(!is.na(date)) %>%
  group_by(wave, unit) %>%
  count(date) %>%
  slice_max(n, with_ties = FALSE) %>%
  ungroup() %>%
  rename(most_common_date = date)
# -- some are unidenfied unit, get common date by only wave for these cases
temp2 <- pcq %>%
  filter(!is.na(date)) %>%
  group_by(wave) %>%
  count(date) %>%
  slice_max(n, with_ties = FALSE) %>%
  ungroup() %>%
  rename(most_common_date_wave_only = date)
# -- Join both: first by unit + wave, then fallback by wave only
pcq <- pcq %>%
  left_join(temp1, by = c("wave", "unit")) %>%
  left_join(temp2, by = "wave")

# -- fill in any missing dates using unit+wave fallback or wave-only fallback
pcq <- pcq %>%
  mutate(
    most_common_date_combined = coalesce(most_common_date, most_common_date_wave_only),
    date = coalesce(date, most_common_date_combined)
  ) %>% 
  select(-most_common_date, -most_common_date_wave_only, -most_common_date_combined) %>%
  relocate(date, .after = unit)

# ================================================================= ####
# Save pcq_unmasked with control numbers #####
saveRDS(pcq, file = paste0("data/processed/processing_layer_2/pcq_unmasked.Rds"))
