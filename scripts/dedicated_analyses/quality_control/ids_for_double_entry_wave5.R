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
dfinf$unit <- "in"
dfinf$block <- "inf"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave5_survey_responses/pcq_wave5_rhu_kw.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Bind Blocks together
pcq5 <- rbind(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)
pcq5$survey_wave <- 5
rm(dfa, dfb, dfc, dfd, dfe, dfinf, dfrhu)

# Select surveys for quality control
set.seed <- 8675309
index <- sample(1:nrow(pcq5), 30)
qc5 <- pcq5[index, c("unit", "id_num")]
qc5 <- qc5[order(qc5$unit),]
# qc5 <- qc5[-which(is.na(qc5$unit)|qc5$id_num=="anon"),]
qc5$any_errors <- 0
qc5$error_count <- 0
write.csv(qc5, "data/processed/dedicated_analyses/quality_control/ids_for_double_entry_wave5.csv", row.names = FALSE)


# Select original surveys
# Note some individuals may have completed more than one survey, so match on id and unit
pcq5_original <- left_join(qc5[,c("unit", "id_num")], pcq5)
write.csv(pcq5_original, "data/processed/dedicated_analyses/quality_control/original_surveys_wave5.csv", row.names = FALSE)
