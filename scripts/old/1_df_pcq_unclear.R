# data set-up ---- 
rm(list=ls())
set.seed(1962)
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")

# libraries ----
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse)
library(naniar)
library(matrixStats)
`%ni%` = Negate(`%in%`)


# Load PCQ data --------------------------- ---------
# PCQ Wave 1 ------
# A Block
dfaa <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_aa_eg.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_ab_bvt.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_ac_eg.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_ad_eg.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_ba_eg.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_bb_eg.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_bc_eg.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_bd_eg.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
dfca <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_ca_eg.xlsx", skip=2, sheet=1)
dfcb <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_cb_eg.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb)
rm(dfca, dfcb)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_da_eg.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_db_eg.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfdb)
rm(dfda, dfdb)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_ea_eg.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_eb_eg.xlsx", skip=2, sheet=1)
# ISSUE TO FIX ####
dfeb <- dfeb[1:72,] # Clean up spreadsheet so that it doesn't generate these NA rows  
dfe <- rbind(dfea, dfeb)
rm(dfea, dfeb)
dfe$block <- "e"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_rhu_eg.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Other
dfother <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave1_survey_responses/pcq_wave1_other_eg.xlsx", skip=2, sheet=1)
dfother$block <- "other"

# Bind Blocks together
pcq <- rbind(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother)
pcq$survey_wave <- 1
rm(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother)

# PCQ Wave 2 -----
# ISSUE TO FIX ####
# Fix generation of NA rows in spreadsheets: ab, cb
# Fix missing unit name in: ad

# A Block
dfaa <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_aa_cm.xlsx", skip=2, sheet=1)
dfab <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_ab_cm.xlsx", skip=2, sheet=1)
dfac <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_ac_cm.xlsx", skip=2, sheet=1)
dfad <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_ad_kw.xlsx", skip=2, sheet=1)
dfa <- rbind(dfaa, dfab, dfac, dfad)
rm(dfaa, dfab, dfac, dfad)
dfa$block <- "a"

# B Block
dfba <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_ba_cm.xlsx", skip=2, sheet=1)
dfbb <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_bb_kw.xlsx", skip=2, sheet=1)
dfbc <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_bc_cm.xlsx", skip=2, sheet=1)
dfbd <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_bd_cm.xlsx", skip=2, sheet=1)
dfb <- rbind(dfba, dfbb, dfbc, dfbd)
rm(dfba, dfbb, dfbc, dfbd)
dfb$block <- "b"

# C Block
# ISSUE TO FIX ####
dfca <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_ca_kw.xlsx", skip=2, sheet=1) # Fix the one date that is not in correct format!
dfcb <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_cb_kw.xlsx", skip=2, sheet=1)
dfcb2 <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_cb2_cm.xlsx", skip=2, sheet=1)
dfc <- rbind(dfca, dfcb, dfcb2)
rm(dfca, dfcb, dfcb2)
dfc$block <- "c"

# D Block
dfda <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_da_cm.xlsx", skip=2, sheet=1)
dfda2 <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_da2_cm.xlsx", skip=2, sheet=1)
dfdb <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_db_kw.xlsx", skip=2, sheet=1)
dfdb2 <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_db2_cm.xlsx", skip=2, sheet=1)
dfd <- rbind(dfda, dfda2, dfdb, dfdb2)
rm(dfda, dfda2, dfdb, dfdb2)
dfd$block <- "d"

# E Block
dfea <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_ea_cm.xlsx", skip=2, sheet=1)
dfeb <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_eb_kw.xlsx", skip=2, sheet=1)
dfeb2 <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_eb2_cm.xlsx", skip=2, sheet=1)
dfe <- rbind(dfea, dfeb, dfeb2)
rm(dfea, dfeb, dfeb2)
dfe$block <- "e"

# RHU Block
dfrhu <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_rhu_cm.xlsx", skip=2, sheet=1)
dfrhu$block <- "rhu"

# Other
dfother <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_misc_kw.xlsx", skip=2, sheet=1)
dfother$block <- "other"

# Unidentified
dfunid <- read_xlsx("data/raw/3_surveys/survey_responses/pcq_wave2_survey_responses/pcq_wave2_unidentified_kw.xlsx", skip=2, sheet=1)
dfunid$block <- "unidentified"

# Bind Blocks together
pcq2 <- rbind(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother, dfunid)
pcq2$survey_wave <- 2

# Bind PCQ1 and PCQ2 together to make PCQ ####
pcq <- rbind(pcq, pcq2)
rm(dfa, dfb, dfc, dfd, dfe, dfrhu, dfother, dfunid)

# Clean PCQ data --------------------------- ####
# clean date format and create format date_l
pcq$date[which(pcq$date=="999",)] <- NA
pcq$date_l <- parse_date_time(pcq$date, c("ymd", "mdy")) # wave 1 stored as ymd, wave 2 stored dates as mdy
pcq <- pcq %>% relocate(date_l, .after = date)

# cleaning / new variables 
names(pcq)[grep("your_comments", names(pcq))] <- "q174"
pcq$unit <- tolower(pcq$unit) 
pcq$id_num <- tolower(pcq$id_num)
pcq$id_num <- ifelse(pcq$id_num=="anon", NA, pcq$id_num)

# Set all zero values (for no opinion) to 111  
# Such that: 999 is set to NA, no opinion = 111, not applicable = 996
pcq[pcq==0] <- 111
pcq <- as.data.frame(pcq)
pcq <- pcq %>% replace_with_na_all(condition = ~.x == 999) # Because this is the true NA

# Recode all prison climate variables to ensure they are positive 
# CAUTION: prison climate variables only at this point!
recode <- pcq_lookup[pcq_lookup$recode_pa_2022a==1,"question_qno"]

for(i in recode){
  # reverse the score: (6-)1=5,  (6-)2=4, (6-)3=3, (6-)4=2, (6-)5=1
  for(k in 1:nrow(pcq)){
    pcq[k,i] <- ifelse(pcq[k,i] %in% c(1,2,3,4,5), 6-pcq[k,i], pcq[k,i]) 
  }
}

# Add variables to pcq data ----------------- ####
pcq$children <- ifelse(pcq$q167==1, 0, # do you have children?
                       ifelse(pcq$q167==2, 1, NA))
pcq$cell <- ifelse(pcq$q90==1, 1, # do you share a cell?
                   ifelse(pcq$q90==2, 0, NA))
pcq$foreign_born <- ifelse(pcq$q170==1, 1, # In what country were you born?
                           ifelse(pcq$q170==2, 0, NA))
pcq$partner <- ifelse(pcq$q132 %in% c(4,5),1,
                      ifelse(pcq$q132 %in% c(1,2,3),0,NA))
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
pcq <- left_join(pcq, unit_mapping, by="unit")
pcq$unit_type <- as.factor(pcq$unit_type)

# Create PCQ2 ####
# pcq & pcq2 - to distinguish responses that are complete but may include 'no opinion' or 'not applicable' answers, from cases that have missing data (999)
# pcq - 999 is set to NA, no opinion = 111, not applicable = 996
# pcq2 - 999, 111, and 996 are all set to NA

pcq2 <- pcq
pcq2 <- pcq2 %>% replace_with_na_all(condition = ~.x == 111)
pcq2 <- pcq2 %>% replace_with_na_all(condition = ~.x == 996)

# Add scale scores for each individual, to facilitate calculations later on ------- ####
# Add to main pcq2 code 
retain <- pcq_lookup[which(pcq_lookup$include_comparative_psych_analysis=="yes"),]$question_qno
scales.list <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain,c("scale_theory", "pc_theory")])

# scale mean, calculated over all answered questions
names.cols <- paste0("maq_", scales.list$scale_theory) 
pcq2[,names.cols] <- NA

names.cols <- paste0("nqs_", scales.list$scale_theory)
pcq2[,names.cols] <- NA

names.cols <- paste0("sd_", scales.list$scale_theory)
pcq2[,names.cols] <- NA

# scale means with missing values imputed by median 
names.cols <- paste0("mip_", scales.list$scale_theory)
pcq2[,names.cols] <- NA

# Impute unanswered questions by taking the column median 
col.names <- names(pcq2)[grep("^q", names(pcq2))]
pcq2_imputed <- as.data.frame(pcq2)
for(i in col.names){
  pcq2_imputed[which(is.na(pcq2_imputed[,i])),i] <- median(pcq2_imputed[,i], na.rm=TRUE)
}

for(i in scales.list$scale){
  # Means and SDs
  ## Identify scales and accompanying question numbers 
  qnos <- data.frame(no = unique(pcq_lookup[which(pcq_lookup$scale_theory==i & pcq_lookup$pc_theory=="prison climate" &
                                                    pcq_lookup$include_comparative_psych_analysis=="yes"),"question_no_pa_2022a"]), 
                     qno = NA)
  qnos$qno <- paste0("q",qnos$no)
  
  rows.without.nas <- which(complete.cases(pcq[,qnos$qno])==TRUE) # Complete cases include those that have no opinion or not applicable answers, but exclude questions left blank
  
  # Calculate mean of identified questions that were answered for each individual 
  
  col_maq <- grep(paste0("maq_",i), names(pcq2)[grep(i, names(pcq2))], value=TRUE)
  col_nqs <- grep(paste0("nqs_",i), names(pcq2)[grep(i, names(pcq2))], value=TRUE)
  pcq2[rows.without.nas,col_maq] <- rowSums(pcq2[rows.without.nas,qnos$qno], na.rm=TRUE) #summing answered questions
  for(j in 1:nrow(pcq2)){
    answered_questions <- length(qnos$qno)-length(which(pcq[j,qnos$qno]==111)) # note counting cases as complete if answered 111, but cannot count it for mean calculation
    pcq2[j,col_nqs] <- answered_questions
    if(answered_questions %in% c(0, NA)){
      pcq2[j,col_maq] <- NA
    } 
    else{
      pcq2[j,col_maq] <- pcq2[j,col_maq]/pcq2[j,col_nqs]
    }
  }
  
  # Calculate sds of identified questions that were answered for each individual 
  col_sd <- grep("sd_", names(pcq2)[grep(i, names(pcq2))], value=TRUE)
  pcq2[rows.without.nas,col_sd] <- rowSds(as.matrix(pcq2[rows.without.nas,qnos$qno]), na.rm=TRUE)
  
  # Calculate scale means based on pcq_imputed, where missing values are imputed by the median
  questions_in_scale <- length(qnos$qno)
  col_mip <- grep(paste0("mip_",i), names(pcq2_imputed)[grep(i, names(pcq2_imputed))], value=TRUE)
  if(i != "overall"){
  pcq2[,col_mip] <- rowSums(pcq2_imputed[,qnos$qno], na.rm=TRUE)/questions_in_scale 
  }
  else{
  pcq2[,col_mip] <- pcq2_imputed[,qnos$qno]
  }
}

# Create two additional columns with the visits scale split 
pcq_lookup$scale_exploratory <- with(pcq_lookup, ifelse(scale_theory=="visits" & question_no_pa_2022a %in% c(139:144), "visits_infrastructure",
                                                                            ifelse(scale_theory=="visits" & question_no_pa_2022a %in% c(145:146),"visits_feelings", scale_theory)))

# scale means with missing values imputed by median 
pcq2$mip_visits_infrastructure <- NA
pcq2$mip_visits_feelings <- NA

for(i in c("visits_infrastructure", "visits_feelings")){
  ## Identify scales and accompanying question numbers 
  qnos <- data.frame(no = unique(pcq_lookup[which(pcq_lookup$scale_exploratory==i & pcq_lookup$pc_theory=="prison climate" &
                                                    pcq_lookup$include_comparative_psych_analysis=="yes"),"question_no_pa_2022a"]), 
                     qno = NA)
  qnos$qno <- paste0("q",qnos$no)
  
  # Calculate scale means based on pcq_imputed, where missing values are imputed by the median
  questions_in_scale <- length(qnos$qno)
  col_mip <- grep(paste0("mip_",i), names(pcq2)[grep(i, names(pcq2))], value=TRUE)
  pcq2[,col_mip] <- rowSums(pcq2_imputed[,qnos$qno], na.rm=TRUE)/questions_in_scale 
}

rm(pcq_imputed)

# Add same columns to pcq - HARD CODED!
start.col <- which(names(pcq2)=="maq_prisoners")
end.col <- which(names(pcq2)=="mip_severity")
pcq <- cbind(pcq,pcq2[,start.col:end.col])


# save --------------------------- ####
# save(pcq2, file="data/processed/pcq2.Rda")
# save(pcq, file="data/processed/pcq.Rda")





