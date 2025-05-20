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

# Select surveys for quality control
set.seed <- 8676309
index <- sample(1:nrow(pcq6), 30)
qc6 <- pcq6[index, c("unit", "id_num")]
qc6 <- qc6[order(qc6$unit),]
# qc6 <- qc6[-which(is.na(qc6$unit)|qc6$id_num=="anon"),]
qc6$any_errors <- 0
qc6$error_count <- 0
write.csv(qc6, "data/processed/dedicated_analyses/quality_control/ids_for_double_entry_wave6.csv", row.names = FALSE)


# Select original surveys
# Note some individuals may have completed more than one survey, so match on id and unit
pcq6_original <- left_join(qc6[,c("unit", "id_num")], pcq6)
write.csv(pcq6_original, "data/processed/dedicated_analyses/quality_control/original_surveys_wave6.csv", row.names = FALSE)
