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
# Note - there are no lifers in the round4 dataset
round4 <- xl.read.file("data/raw/4_random_assignment/eligibility/20231101_tbl_99_LSRecruitmentData_round4.xlsx", xl.sheet=1, password="LS2023")
randassign <- readRDS("data/processed/randassign.Rds")
exclusions <- xl.read.file("data/temp/LSExclusions r1 r2 r3 all 2023-05-09 for bvt.xlsx", xl.sheet=1, password="LS2023")

exclusions$inmate_number <- tolower(exclusions$InmateID)
exclusions$research_id <- mask_ids(exclusions$inmate_number)$research_id
round4$inmate_number <- tolower(round4$inmate_number)
round4$research_id <- mask_ids(round4$inmate_number)$research_id #1035

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

# Merge round4 and exclusions
round4 <- left_join(round4, exclusions)
for(k in c("Optout_R1", "CHS_Sec_R1", "Optout_R2",
           "CHS_Sec_R2", "TotalExclusions", "Optout_R3", "CHS_Sec_R3")){
  round4[,k] <- ifelse(is.na(round4[,k]), 0, round4[,k])
}
round4$SecFail <- ifelse(round4$CHS_Sec_R1==1|round4$CHS_Sec_R2|round4$CHS_Sec_R3,1,0)
round4$OptOutFail <- with(round4, Optout_R1+Optout_R2+Optout_R3>0,1,0)

# Merge files
round4 <- left_join(round4, randassign, by = "research_id")

# # Understand the data ####
# # LS column includes individuals who were Alternates
# table(round4$LS, round4$treated, useNA = "always")
# table(round4$treated, round4$LSAlternate, useNA = "always")
# table(round4$treated)
# table(round4$sentence_class) # no lifers in the data

# Check - Failure variables ####
# Reasons for failure
names(round4)[which(names(round4)=="Z-CodeFail")] <- "Z_CodeFail"
fail_vars <- c("Z_CodeFail", "MinFail", "TCFail", "STGFail", "StaffAssaultFail", "ViolentMisconFail", "DrugMisconFail", "RHUFail", "LSExclusions", "OptOutFail")
for(i in fail_vars){
  print(i)
  k <- which(names(round4)==i)
  print(length(which(round4[,k]==1)))
}

# "Nos" have at least one failure on the failure variables
round4$failure_check <- rowSums(round4[,fail_vars])
round4$LSEligible_updated <- ifelse(round4$failure_check>0|round4$treated %in% c(0,1),"No", "Maybe")
table(round4$LSEligible, useNA = "always")
table(round4$LSEligible_updated, useNA = "always")

# Mismatches
# mv5646 is treated in the eligibility spreadsheet, but whom I do not have on file as treated mv5646
# qn7876 is a control in their data - treated in ours!

round4$change_in_eligibility <- ifelse(round4$LSEligible!=round4$LSEligible_updated,1,0)
round4 <- round4 %>% relocate(LSEligible_updated, .after = LSEligible)
round4 <- round4 %>% relocate(change_in_eligibility, .after = LSEligible_updated)
round4 <- round4 %>% relocate(treated, .after = LS)

check <- round4[which(round4$change_in_eligibility==1),c("inmate_number","LSEligible", "LSEligible_updated", "LS", "LSAlternate","LSExclusions","LSCohort","treatment_date", "treated", "failure_check", "notes", fail_vars[-which(fail_vars=="LSExclusions")])]
check <- check[-which(check$LSCohort==20230518),]
write_xlsx(check, "data/raw/4_random_assignment/assignment/temp/231102_eligibility_inconsistencies.xlsx")

# There are 11 people - all from the second treatment wave whom our data suggests they are controls and eligibility data suggests they are treated, and vice versa.
temp <- round4[which(round4$LS!=round4$treated),]
temp <- temp[,c("inmate_number", "LS", "LSAlternate","treated","LSCohort","treatment_date","release_type","release_date","notes")]

write_xlsx(temp, "data/raw/4_random_assignment/assignment/temp/231102_treatment_status_inconsistencies.xlsx")


