# PCQ Wave 4 ####
# A Block
dfaa1 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_aa_sc_bs.xlsx", skip=2, sheet=1)
dfaa2 <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_aa_sc_part2.xlsx", skip=2, sheet=1)
dfaa <- rbind(dfaa1, dfaa2)
rm(dfaa1, dfaa2)

dfab <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave4_survey_responses/pcq_wave4_ab_oc_part2.xlsx", skip=2, sheet=1) # Error to fix: Get Part 1 of this unit
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

# Select surveys for quality control
set.seed <- 8675309
index <- sample(1:nrow(pcq4), 30)
qc4 <- pcq4[index, c("unit", "id_num")]
qc4 <- qc4[order(qc4$unit),]
# qc4 <- qc4[-which(is.na(qc4$unit)|qc4$id_num=="anon"),]
qc4$any_errors <- 0
qc4$error_count <- 0
write.csv(qc4, "data/processed/dedicated_analyses/quality_control/ids_for_double_entry_wave4.csv", row.names = FALSE)


# Select original surveys
# Note some individuals may have completed more than one survey, so match on id and unit
pcq4_original <- left_join(qc4[,c("unit", "id_num")], pcq4)
write.csv(pcq4_original, "data/processed/dedicated_analyses/quality_control/original_surveys_wave4.csv", row.names = FALSE)

# Record of IDs selected
# [1] "qp7625" "qp0997" "qp3342" "qp3176" "qp5461" "ny8674" "qp6952" "ql2151" "qp0829" "qc8499" "qn6806" "qp1628"
# [13] "bm8320" "cy9623" "ay5858" "ny1033" "du1332" "qf2517" "qp8161" "gj4542" "qe7285" "nt6044" "ej9181" "jm1679"
# [25] "qp4910" "dd4284" "qp0711" "jq8364" "qn6498" "ay3342"
