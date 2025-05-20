# ---------- Data set-up ---------- ----
rm(list=ls())

## Set Seed ----
set.seed(1962)

# Source ####
source("scripts/0_id_masking_function.R")
# source("scripts/0_utils.R")

library(readxl)
# library(dplyr)
library("excel.link")
`%ni%` = Negate(`%in%`)

# Prepare data ####
# Note - there are no lifers in the round5 dataset
round5 <- xl.read.file("data/raw/4_random_assignment/eligibility/20240415_LSRecruitmentData_CEN_round5.xlsx", xl.sheet=1, password="LS2024")
randassign <- readRDS("data/processed/randassign.Rds")
exclusions <- xl.read.file("data/temp/LSExclusions r1 r2 r3 all 2023-05-09 for bvt.xlsx", xl.sheet=1, password="LS2023")

exclusions$inmate_number <- tolower(exclusions$InmateID)
exclusions$research_id <- mask_ids(exclusions$inmate_number)$research_id
round5$inmate_number <- tolower(round5$inmate_number)
round5$research_id <- mask_ids(round5$inmate_number)$research_id #1035

# Identify the duplicated IDs
# Appear to be duplicated from previous rounds
for(i in unique(exclusions$inmate_number)){
  temp <- exclusions[which(exclusions$inmate_number==i), ]
  for(k in c("Optout_R1", "CHS_Sec_R1", "Optout_R2",
             "CHS_Sec_R2", "TotalExclusions", "Optout_R3", "CHS_Sec_R3")){
    exclusions[which(exclusions$inmate_number==i), k] <-  temp[,k][which.max(temp[,k])]
  }
}
exclusions <- exclusions[-which(duplicated(exclusions$inmate_number)),]

# Merge round5 and exclusions
round5 <- left_join(round5, exclusions)
for(k in c("Optout_R1", "CHS_Sec_R1", "Optout_R2",
           "CHS_Sec_R2", "TotalExclusions", "Optout_R3", "CHS_Sec_R3")){
  round5[,k] <- ifelse(is.na(round5[,k]), 0, round5[,k])
}
round5$SecFail <- ifelse(round5$CHS_Sec_R1==1|round5$CHS_Sec_R2|round5$CHS_Sec_R3,1,0)
round5$OptOutFail <- with(round5, Optout_R1+Optout_R2+Optout_R3>0,1,0)

# Merge files
round5 <- left_join(round5, randassign, by = "research_id")

# # Understand the data ####
# # LS column includes individuals who were Alternates
# table(round5$LS, round5$treated, useNA = "always")
# table(round5$treated, round5$LSAlternate, useNA = "always")
# table(round5$treated)
# table(round5$sentence_class) # no lifers in the data

# Check - Failure variables ####
# Reasons for failure
names(round5)[which(names(round5)=="Z-CodeFail")] <- "Z_CodeFail"
fail_vars <- c("Z_CodeFail", "MinFail", "TCFail", "STGFail", "StaffAssaultFail", "ViolentMisconFail", "DrugMisconFail", "RHUFail", "LSExclusions", "OptOutFail")
for(i in fail_vars){
  print(i)
  k <- which(names(round5)==i)
  print(length(which(round5[,k]==1)))
}

# "Nos" have at least one failure on the failure variables
round5$failure_check <- rowSums(round5[,fail_vars])
round5$LSEligible_updated <- ifelse(round5$failure_check>0|round5$treated %in% c(0,1),"No", "Maybe")
table(round5$LSEligible, useNA = "always")
table(round5$LSEligible_updated, useNA = "always")

# Mismatches
# qh6988 and jb3289 are mismatched but they are both housed in the infirmary

round5$change_in_eligibility <- ifelse(round5$LSEligible!=round5$LSEligible_updated,1,0)
round5 <- round5 %>% relocate(LSEligible_updated, .after = LSEligible)
round5 <- round5 %>% relocate(change_in_eligibility, .after = LSEligible_updated)
round5 <- round5 %>% relocate(treated, .after = LS)

check <- round5[which(round5$change_in_eligibility==1),c("inmate_number","LSEligible", "LSEligible_updated", "LS", "LSAlternate","LSExclusions","LSCohort","treatment_date", "treated", "failure_check", "notes", fail_vars[-which(fail_vars=="LSExclusions")])]
check <- check[-which(check$LSCohort==20231127),]
write_xlsx(check, "data/raw/4_random_assignment/assignment/temp/231102_eligibility_inconsistencies.xlsx")



