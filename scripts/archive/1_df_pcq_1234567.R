# Data set-up ----
rm(list=ls())
set.seed(1962)
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")

# Libraries ----
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse)
# library(naniar) # No package called this?
library(matrixStats)
library(Amelia) # To impute data
set.seed(0630231106)
`%ni%` = Negate(`%in%`)


# Load PCQ data --------------------------- ---------
# PCQ Wave 1 ------
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
dfother$block <- "other"

# Bind Blocks together
pcq1 <- rbind(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother)
pcq1$survey_wave <- 1
rm(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother)

# PCQ Wave 2 -----
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
dfother$block <- "other"

# Unidentified
dfunid <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave2_survey_responses/pcq_wave2_unidentified_kw.xlsx", skip=2, sheet=1)
dfunid$block <- "unidentified"

# Bind Blocks together
pcq2 <- rbind(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother, dfunid)
pcq2$survey_wave <- 2
rm(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother, dfunid)

# PCQ Wave 3 ####
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

# PCQ Wave 4 ####
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


# PCQ Wave 5 ####
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_aa_bs.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ab_bs.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ac_bs.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ad_bs.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ba_gc.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bb_bs.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bc_gc.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_bd_gc.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ca_bs.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_cb_gc.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_da_kw.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_db_kw.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_ea_gc.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_eb_gc.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_inf_kw.xlsx", skip=2, sheet=1)
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_rhu_kw.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Bind Blocks together
pcq5 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq5$survey_wave <- 5
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)

# PCQ Wave 6 ####
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

# PCQ Wave 7 ####
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_aa_mp.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ab_mp.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ac_mh.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ad_mh.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ba_lg.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bb_lg.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bc_lg.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_bd_bs.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd) 
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ca_jv.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_cb_bs.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_da_mp.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_db_es.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_ea_gc.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_eb_mb.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# INF Block
dfinf <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_inf_gc.xlsx", skip=2, sheet=1)
dfinf$unit <- "inf"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave7_survey_responses/pcq_wave7_rhu_mb.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Bind Blocks together
pcq7 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq7$survey_wave <- 7
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)


# Bind all waves together to make pcq ####
pcq <- rbind(pcq1, pcq2, pcq3, pcq4, pcq5, pcq6, pcq7)
rm(pcq1, pcq2, pcq3, pcq4, pcq5, pcq6, pcq7)

# Clean PCQ data --------------------------- ####
pcq$unit <- tolower(pcq$unit)

# Clean dates ####
# date format
pcq$date_old <- pcq$date
pcq$date_old[which(pcq$date_old %in% c("999", "9092099"))] <- NA # 9092099, Wave 3, in unidentified stack
pcq$date <- parse_date_time(pcq$date_old, c("ymd", "mdy")) # wave 1 stored as ymd, wave 2 stored dates as mdy
pcq$date <- as.Date(pcq$date)
pcq <- pcq[,-which(names(pcq)=="date_old")]

# When dates are NA, assign most common date in that wave and unit
temp <- pcq %>% filter(!is.na(date)) %>% group_by(survey_wave, unit) %>% dplyr::count(date)
temp$most.common.date <- NA
temp <- as.data.frame(temp)
for(i in unique(temp[!is.na(temp$unit),]$unit)){
  for(k in unique(temp$survey_wave)){
  index <- which(temp$unit==i & temp$survey_wave==k)
  temp[index,"most.common.date"] <- temp[index,"n"][which.max(temp[index,"n"])]
  }
}
temp <- temp[which(temp$n==temp$most.common.date), c("survey_wave", "unit", "date")]
names(temp)[which(names(temp)=="date")] <- "most_common_date"
pcq <- left_join(pcq, temp)
pcq[is.na(pcq$date), "date"] <- pcq[is.na(pcq$date), "most_common_date"]
pcq <- pcq[,-which(names(pcq)=="most_common_date")]

# For observations with no unit, assign latest most common date in that unit
# Fix funny dates in wave 5
pcq$date[which(pcq$date==as.Date(ymd(20430204)))] <- as.Date(ymd(20240501))
pcq$date[which(pcq$date==as.Date(ymd(20501204)))] <- as.Date(ymd(20240501))
for(i in c(1:5)){
  pcq[is.na(pcq$date) & pcq$survey_wave == i,"date"] <- max(pcq$date[pcq$survey_wave == i], na.rm = TRUE)
}

# cleaning / new variables
names(pcq)[grep("your_comments", names(pcq))] <- "q174"
pcq$unit <- tolower(pcq$unit)
pcq$id_num <- tolower(pcq$id_num)
pcq$id_num <- ifelse(pcq$id_num=="anon", NA, pcq$id_num)

# Set all zero values (for no opinion) to 111
# Such that: 999 is set to NA, no opinion = 111, not applicable = 996
pcq[pcq==0] <- 111
pcq <- as.data.frame(pcq)

# Replace all 999 values with NA
pcq[pcq == 999] <- NA

# Recode all prison climate variables to ensure they are positive
# Prison Climate Variables
recode <- pcq_lookup[pcq_lookup$recode_pa_2022a==1,"question_qno"]

for(i in recode){
  # reverse the score: (6-)1=5,  (6-)2=4, (6-)3=3, (6-)4=2, (6-)5=1
  for(k in 1:nrow(pcq)){
    pcq[k,i] <- ifelse(pcq[k,i] %in% c(1,2,3,4,5), 6-pcq[k,i], pcq[k,i])
  }
}

# Yes coded as 1 in these questions
pcq[,"q90"] <- ifelse(pcq$q90==1, 2, ifelse(pcq$q90==2, 1, pcq$q90))
pcq[,"q147"] <- ifelse(pcq$q147==1, 2, ifelse(pcq$q147==2, 1, pcq$q147))
pcq[,"q148"] <- ifelse(pcq$q148==1, 2, ifelse(pcq$q148==2, 1, pcq$q148))


# Mask IDs --------------------------- ####
names(pcq)[which(names(pcq)=="id_num")] <- "original_id"
pcq$original_id <- tolower(pcq$original_id)
i <- unique(pcq$original_id)
id.link <- mask_ids(i) # Generate masked IDs
pcq <- left_join(pcq, id.link, by="original_id") # Merge basic on original ID
pcq <- pcq[,-which(names(pcq)=="original_id")] # delete original ID
pcq <- pcq[,c(ncol(pcq),1:ncol(pcq)-1)] # reorder columns
pcq <- pcq %>%
  relocate(date, .after = research_id) %>%
  relocate(block, survey_wave, .after = unit)

# Assign unique IDs to individuals with NA values for their research ID
pcq[which(is.na(pcq$research_id)), "research_id"] <- paste0("rid_na", c(1:length(pcq[which(is.na(pcq$research_id)), "research_id"])))

# Identify survey number for a particular respondent
pcq$survey_no <- NA
pcq <- as.data.frame(pcq)

for(i in unique(pcq$research_id)){
  dates.list <- sort(pcq[pcq$research_id==i,c("date")]) # Dates not necessarily in order in pcq
  for(k in 1:length(dates.list)){
    pcq[which(pcq$research_id==i & pcq$date==dates.list[k]),"survey_no"] <- k
  }
}


# For questions you can skip, we mark them 996 if this did not apply to an individual

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

# Link unit to unit type
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

# Save --------------------------- ####
saveRDS(pcq, file="data/processed/processing_layer_1/pcq.rds")





