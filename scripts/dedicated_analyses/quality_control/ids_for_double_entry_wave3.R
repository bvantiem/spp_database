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
dfda <- read_xlsx("data/raw/3_surveys/2_survey_responses/pcq_wave3_survey_responses/pcq_wave3_da_cm.xlsx", skip=2, sheet=1) # To Do: Fix extra rows in final data ####
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

# Select surveys for quality control
set.seed <- 8675309
index <- sample(1:nrow(pcq3), 30)
qc3 <- pcq3[index, c("unit", "id_num")]
qc3 <- qc3[order(qc3$unit),]
qc3 <- qc3[-which(is.na(qc3$unit)|qc3$id_num=="anon"),]
qc3$any_errors <- 0
qc3$error_count <- 0
write.csv(qc3, "data/processed/dedicated_analyses/quality_control/ids_for_double_entry_wave3.csv", row.names = FALSE)

# Ids used in practice
ids.selected <- c("qp0406","qp1765","ny8674","mj9219","qx1717","nr9300","kx1822",
                  "qh3842","qd3323","qp2962","qn6989","qn8603","qf2261","qg4576",
                  "dt5760","mz4676","mw9984","ew1942","kw5313","qf1460","ql0894",
                  "qp4118","hu4193","ql5985","qc3881","qn6848","qh7886","jd4967",
                  "ma1234")


# Select original surveys
index <- which(pcq3$id_num %in% ids.selected)
pcq3_original <- pcq3[index,]
pcq3_original <- pcq3_original[-which(pcq3_original$id_num=="kx1822" & pcq3_original$unit!="bb"),]
pcq3_original <- pcq3_original[-which(pcq3_original$id_num=="ql5985" & pcq3_original$unit!="eb"),]
write.csv(pcq3_original, "data/processed/dedicated_analyses/quality_control/original_surveys_wave3.csv", row.names = FALSE)
