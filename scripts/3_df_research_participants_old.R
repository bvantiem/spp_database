# set-up ----
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")

# Data ####
pcq <- readRDS("data/processed/processing_layer_1/pcq.Rds")
pcq <- as.data.frame(pcq)
randassign <- readRDS("data/processed/randassign.Rds")
basic <- readRDS("data/processed/basic.Rds")
load("data/processed/wave2_callsheets.Rda")
load("data/processed/wave3_callsheets.Rda")
load("data/processed/wave4_callsheets.Rda")
load("data/processed/wave5_callsheets.Rda")
load("data/processed/wave6_callsheets.Rda")
pcqwave2_5 <- read.csv("data/raw/3_surveys/3_compensation_lists/compensation4_pcq_wave2_5.csv")
load("data/processed/wave5_participation.Rda") # temporary!
load("data/processed/wave6_participation.Rda") # temporary!

# Mask ids for wave 2.5 first ####
names(pcqwave2_5)[which(names(pcqwave2_5)=="inmate_id")] <- "original_id"
pcqwave2_5$original_id <- tolower(pcqwave2_5$original_id)
i <- unique(pcqwave2_5$original_id)
id.link <- mask_ids(i) # Generate masked IDs
pcqwave2_5 <- left_join(pcqwave2_5, id.link, by="original_id") # Merge pcqwave2_5 on original ID
pcqwave2_5 <- pcqwave2_5[,-which(names(pcqwave2_5)=="original_id")] # delete original ID
pcqwave2_5 <- pcqwave2_5[,c(ncol(pcqwave2_5),1:ncol(pcqwave2_5)-1)] # reorder columns

# Unique IDS ####
unique_ids <- unique(c(pcq$research_id,
                       randassign$research_id,
                       basic$research_id,
                       wave2_callsheets$research_id,
                       wave3_callsheets$research_id,
                       wave4_callsheets$research_id,
                       wave5_callsheets$research_id,
                       wave6_callsheets$research_id,
                       pcqwave2_5$research_id))

# Set up dataframe to track survey participation and admin data ####
research_participants <- data.frame(research_id = unique_ids,
                            treated = rep(NA, length(unique_ids)),
                            treatment_wave = rep(NA, length(unique_ids)),
                            stratum = rep(NA, length(unique_ids)),
                            pcq_wave1 = rep(0, length(unique_ids)),
                            pcq_wave2 = rep(0, length(unique_ids)),
                            pcq_wave2_5 = rep(0, length(unique_ids)),
                            pcq_wave3 = rep(0, length(unique_ids)),
                            pcq_wave4 = rep(0, length(unique_ids)),
                            pcq_wave5 = rep(0, length(unique_ids)),
                            pcq_wave6 = rep(0, length(unique_ids)),
                            nsurveys = rep(0, length(unique_ids)),
                            nsurveys_pre = rep(0, length(unique_ids)),
                            nsurveys_post = rep(0, length(unique_ids)),
                            adm_wave1 = rep(0, length(unique_ids)),
                            adm_wave2 = rep(0, length(unique_ids)),
                            adm_wave3 = rep(0, length(unique_ids)),
                            adm_wave4 = rep(0, length(unique_ids)),
                            adm_wave5 = rep(0, length(unique_ids)),
                            adm_wave6 = rep(0, length(unique_ids)),
                            nadmin = rep(0, length(unique_ids)),
                            pcq_callsheets_wave2 = rep(0, length(unique_ids)),
                            pcq_callsheets_wave3 = rep(0, length(unique_ids)),
                            pcq_callsheets_wave4 = rep(0, length(unique_ids)),
                            pcq_callsheets_wave5 = rep(0, length(unique_ids)),
                            pcq_callsheets_wave6 = rep(0, length(unique_ids)))

# Populate with relevant values by ID
# treatment status, wave and stratum (update with wave 3!)
i <- match(research_participants$research_id, randassign$research_id)
research_participants$stratum <- randassign$stratum[i]
research_participants$treated <- randassign$treated[i]
research_participants$treatment_wave <- randassign$treatment_wave[i]

# pcq wave 1, 2, 2.5, 3, 4, 5, 6 ####
ids <- unique(pcq[which(pcq$survey_wave==1),"research_id"])
research_participants$pcq_wave1 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$pcq_wave1))

ids <- unique(pcq[which(pcq$survey_wave==2),"research_id"])
research_participants$pcq_wave2 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$pcq_wave2))

ids <- unique(pcqwave2_5$research_id)
research_participants$pcq_wave2_5 <- with(research_participants,
                                              ifelse(research_id %in% ids, 1, research_participants$pcq_wave2_5))

ids <- unique(pcq[which(pcq$survey_wave==3),"research_id"])
research_participants$pcq_wave3 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$pcq_wave3))

ids <- unique(pcq[which(pcq$survey_wave==4),"research_id"])
ids <- unique(c(ids, "qp6461", "qp6173", "qp7509", "qp5737", "ly0069", "qp5419", "qp3762","lj0042", "mc2053", "qp2605", "qn2146")) # Temporary: add because these ids are still to be entered
research_participants$pcq_wave4 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$pcq_wave4))

ids <- unique(wave5_participation$research_id)
research_participants$pcq_wave5 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$pcq_wave5))

ids <- unique(wave6_participation$research_id)
research_participants$pcq_wave6 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$pcq_wave6))

# nsurveys
research_participants$nsurveys <- with(research_participants, pcq_wave1 + pcq_wave2 + pcq_wave2_5 + pcq_wave3 + pcq_wave4 + pcq_wave5 + pcq_wave6)
research_participants$nsurveys_pre <- with(research_participants,
                                           ifelse(treatment_wave %in% c(0,1), pcq_wave1,
                                                  ifelse(treatment_wave %in% c(2, 2.5), pcq_wave1 + pcq_wave2 + pcq_wave2_5,
                                                         ifelse(treatment_wave==3, pcq_wave1 + pcq_wave2 + pcq_wave2_5 + pcq_wave3,
                                                                ifelse(treatment_wave==4, pcq_wave1 + pcq_wave2 + pcq_wave2_5 + pcq_wave3 + pcq_wave4,
                                                                       ifelse(treatment_wave==5, pcq_wave1 + pcq_wave2 + pcq_wave2_5 + pcq_wave3 + pcq_wave4 + pcq_wave5,
                                                                              ifelse(treatment_wave==6, pcq_wave1 + pcq_wave2 + pcq_wave2_5 + pcq_wave3 + pcq_wave4 + pcq_wave5 + pcq_wave6, NA)))))))

research_participants$nsurveys_post <- with(research_participants,
                                           ifelse(treatment_wave %in% c(0,1), pcq_wave2 + pcq_wave2_5 + pcq_wave3,
                                                  ifelse(treatment_wave %in% c(2, 2.5), pcq_wave3 + pcq_wave4,
                                                         ifelse(treatment_wave==3, pcq_wave4 + pcq_wave5,
                                                                ifelse(treatment_wave==4, pcq_wave5,
                                                                       ifelse(treatment_wave==5, pcq_wave6,
                                                                              ifelse(treatment_wave==6, 0, NA)))))))

# adm wave 1, 2, 3, 4, 5- ADD in wave 6!
ids <- unique(basic[which(basic$date_datapull %in% c(ymd(20220625), ymd(20220903))),"research_id"])
research_participants$adm_wave1 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$adm_wave1))

ids <- unique(basic[which(basic$date_datapull %in% ymd(20220912)),"research_id"])
research_participants$adm_wave2 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$adm_wave2))

ids <- unique(basic[which(basic$date_datapull %in% ymd(20230410)),"research_id"])
research_participants$adm_wave3 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$adm_wave3))

ids <- unique(basic[which(basic$date_datapull %in% ymd(20231107)),"research_id"])
research_participants$adm_wave4 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$adm_wave4))

ids <- unique(basic[which(basic$date_datapull %in% ymd(20240508)),"research_id"])
research_participants$adm_wave5 <- with(research_participants,
                                        ifelse(research_id %in% ids, 1, research_participants$adm_wave5))

# nadmin
research_participants$nadmin <- with(research_participants, adm_wave1 + adm_wave2 + adm_wave3 + adm_wave4 + adm_wave5)

# callsheets wave 2, 3, 4, 5
# callsheets wave 2
ids <- unique(wave2_callsheets$research_id)
research_participants$pcq_callsheets_wave2 <-  with(research_participants,
                                                    ifelse(research_id %in% ids, 1, research_participants$pcq_callsheets_wave2))

# callsheets wave 3
ids <- unique(wave3_callsheets$research_id)
research_participants$pcq_callsheets_wave3 <-  with(research_participants,
                                                    ifelse(research_id %in% ids, 1, research_participants$pcq_callsheets_wave3))

# callsheets wave 4
ids <- unique(wave4_callsheets$research_id)
research_participants$pcq_callsheets_wave4 <-  with(research_participants, ifelse(research_id %in% ids, 1, research_participants$pcq_callsheets_wave4))

# callsheets wave 5
ids <- unique(wave5_callsheets$research_id)
research_participants$pcq_callsheets_wave5 <-  with(research_participants, ifelse(research_id %in% ids, 1, research_participants$pcq_callsheets_wave5))

# callsheets wave 6
ids <- unique(wave6_callsheets$research_id)
research_participants$pcq_callsheets_wave6 <-  with(research_participants, ifelse(research_id %in% ids, 1, research_participants$pcq_callsheets_wave6))

# Merge with static characteristics from basic ####
static_vars <- c("est_days_served_on_20220501",
                 "min_sent_days", "max_sent_days",
                 "days_to_min_at_treatment",
                 "life",
                 "commit_cnty", "cnty_name",
                 "asca","violent_offense", "property_offense", "drugs_offense", "publicorder_offense", "notanarrest_offense",
                 "race_code", "race","race_white", "race_black",
                 "sex_type",
                 "marital_status_code",
                 "mh_code", "STG",
                 "grade_complete", "high_school",
                 "age_at_treatment") # "age_on_20220501"


research_participants <- left_join(research_participants, basic[-which(duplicated(basic$research_id)),c("research_id", static_vars)])

# Merge with static characteristics from pcq survey wave 1, 2, 3, 4 ####
i <- match(research_participants$research_id, pcq$research_id)

research_participants$children <- ifelse(pcq$q167[i]==1, 0, # do you have children?
                       ifelse(pcq$q167[i]==2, 1, NA))
research_participants$cell <- ifelse(pcq$q90[i]==1, 1, # do you share a cell?
                   ifelse(pcq$q90[i]==2, 0, NA))
research_participants$foreign_born <- ifelse(pcq$q170[i]==1, 1, # In what country were you born?
                           ifelse(pcq$q170[i]==2, 0, NA))
research_participants$partner <- ifelse(pcq$q132[i] %in% c(4,5),1,
                      ifelse(pcq$q132[i] %in% c(1,2,3),0,NA))

# How many surveys?
sum(research_participants$nsurveys)

# How many unique individuals?
length(unique(research_participants[which(research_participants$nsurveys>0),"research_id"]))

# Save ####
saveRDS(research_participants, file="data/processed/research_participants.Rds")


