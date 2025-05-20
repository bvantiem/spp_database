# ---------- Data set-up ---------- ---- 
rm(list=ls())

## Set Seed ----
set.seed(1962)

# Source ####
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")

library(readxl)
library(dplyr)
library("excel.link")
source("scripts/0_id_masking_function.R")
`%ni%` = Negate(`%in%`)

# Prepare data ####
# Note - there are no lifers in the round3 dataset 
round3 <- xl.read.file("data/raw/4_random_assignment/eligibility/20230424_tbl_99_LSRecruitmentData_eligibility_round3.xlsx", xl.sheet=1, password="LS2023")
exclusions <- xl.read.file("data/temp/LSExclusions r1 r2 r3 all 2023-05-09 for bvt.xlsx", xl.sheet=1, password="LS2023")
load("data/processed/basic.Rda")
load("data/processed/randassign.Rda")

exclusions$inmate_number <- tolower(exclusions$InmateID)
exclusions$research_id <- mask_ids(exclusions$inmate_number)$research_id
round3$inmate_number <- tolower(round3$inmate_number)
round3$research_id <- mask_ids(round3$inmate_number)$research_id #1035

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

# 35 individuals on the exclusions list are not in round 3
# Likely explained by the fact that this includes security exclusions from round 1, and some of them will have been released since then
exclusions$inmate_number[which(exclusions$inmate_number %ni% round3$inmate_number)]

# It does also include some individuals who were excluded in the security review
#  "df9036" "eg5999" "hw1978" "qa9292"
exclusions$inmate_number[which(exclusions$CHS_Sec_R3==1 & exclusions$inmate_number %ni% round3$inmate_number)]

# Merge round3 and exclusions
round3 <- left_join(round3, exclusions)
for(k in c("Optout_R1", "CHS_Sec_R1", "Optout_R2",
           "CHS_Sec_R2", "TotalExclusions", "Optout_R3", "CHS_Sec_R3")){
  round3[,k] <- ifelse(is.na(round3[,k]), 0, round3[,k])
}
round3$SecFail <- ifelse(round3$CHS_Sec_R1==1|round3$CHS_Sec_R2|round3$CHS_Sec_R3,1,0) 
# round3$SecFail2 <- ifelse(round3$CHS_Sec_R1==1|round3$CHS_Sec_R2==1|round3$CHS_Sec_R3==1,1,0) # Correct!
round3$OptOutFail <- with(round3, Optout_R1+Optout_R2+Optout_R3>0,1,0)

# # Treated_ids
treated_ids_data <- unique(randassign[which(randassign$treated %in% c(0,1)), c("research_id", "treated")])
treated_ids_data$research_id[which(treated_ids_data$research_id %ni% basic$research_id)] # missing from basic!
treated_ids_data <- left_join(treated_ids_data, unique(basic[basic$research_id %in% treated_ids_data$research_id, c("research_id", "treated", "stratum", "treatment_wave")]))
round3 <- left_join(round3, treated_ids_data, by = "research_id")

# # Understand the data ####
# # LS column includes individuals who were Alternates 
# table(round3$LS, round3$treated, useNA = "always")
# table(round3$treated, round3$LSAlternate, useNA = "always")
# table(round3$treated)
# table(round3$sentence_class) # no lifers in the data

# Check - Failure variables ####
# Reasons for failure
names(round3)[which(names(round3)=="Z-CodeFail")] <- "Z_CodeFail"
fail_vars <- c("Z_CodeFail", "MinFail", "TCFail", "STGFail", "StaffAssaultFail", "ViolentMisconFail", "DrugMisconFail", "RHUFail", "SecFail", "OptOutFail")
for(i in fail_vars){
  print(i)
  k <- which(names(round3)==i)
  print(length(which(round3[,k]==1)))
}

# "Nos" have at least one failure on the failure variables
round3$failure_check <- rowSums(round3[,fail_vars])
round3$LSEligible_updated <- ifelse(round3$failure_check>0|round3$treated %in% c(0,1),"No", "Maybe")
table(round3$LSEligible, useNA = "always")
table(round3$LSEligible_updated, useNA = "always")

# Mismatches to point out to Jordan 
round3$change_in_eligibility <- ifelse(round3$LSEligible!=round3$LSEligible_updated,1,0)
# check <- round3[which(round3$change_in_eligibility==1),c("inmate_number", "LS", "LSAlternate", "treated", "failure_check", "LSEligible", "LSEligible_updated", "CHS_Sec_R1", "CHS_Sec_R2")]
round3 <- round3 %>% relocate(LSEligible_updated, .after = LSEligible)
round3 <- round3 %>% relocate(change_in_eligibility, .after = LSEligible_updated)
write.csv(round3, "data/temp/20230424_eligibility_round3_updated.csv", row.names=FALSE)

# Check - # Find released IDS from wave 2 and identify the stratum they were in 
released_ids <- treated_ids_data[which(treated_ids_data$research_id %ni% round3$research_id),c("research_id", "stratum", "treatment_wave", "treated")]
table(released_ids[which(released_ids$treatment_wave==2 & released_ids$treated==1),]$stratum)
table(released_ids[which(released_ids$treatment_wave==1 & released_ids$treated==1),]$stratum)
table(released_ids$treatment_wave, released_ids$treated)
released_ids_to_unmask <- released_ids[which(released_ids$treatment_wave %in% c(1,2) & released_ids$treated==1),c("research_id")]



# Check overlap between my list and Jordan's list 
temp <- as.data.frame(unmask_ids(released_ids_to_unmask))
temp.jordan <- tolower(c("QN1884",
                 "QM8079",
                 "NM1963",
                 "MX2788",
                 "NS9433",
                 "QH9433",
                 "NX7007",
                 "HX8307",
                 "NC0861",
                 "QG2468",
                 "QN0083",
                 "QN0566",
                 "QN5431",
                 "QH5079",
                 "QL9123",
                 "QM2723",
                 "QC7740",
                 "BF6556",
                 "QG7429",
                 "QN2340"))

# Non overlap between lists 
comp <- data.frame(original_id = c(temp$original_id, temp.jordan),
                   in_jordan = NA,
                   in_britte = NA)
comp$in_jordan <- comp$original_id %in% temp.jordan
comp$in_britte <- comp$original_id %in% temp$original_id
temp <- mask_ids(comp$original_id)
comp$research_id <- temp$research_id
comp <- left_join(comp, unique(basic[,c("research_id", "treated", "treatment_wave", "sentence_class")]))
comp$mismatch <- ifelse(comp$in_britte==FALSE|comp$in_jordan==FALSE, TRUE, FALSE)

# Look at individuals who I did not think had been released yet
ids <- comp$original_id[comp$in_britte==FALSE]
check <- round3[round3$inmate_number %in% ids,]

ids <- comp$research_id[comp$in_britte==TRUE & comp$in_jordan==FALSE]
check <- basic[basic$research_id %in% ids,]

comp$notes <- NA
comp$notes[comp$research_id %in% ids] <- "not_in_eligibility_list"
comp$notes[comp$original_id=="qn1884"] <- "left_ls_2022_05_19"
comp$notes[comp$original_id=="qn2340"] <- "removed_ls_2023_04"
comp$notes[comp$original_id=="ns9433"] <- "no_admin_data"
comp$notes[comp$original_id=="bf6556"] <- "lifers_not_in_eligibility"

# There are five individuals who do not appear in the eligibility list. I therefore assumed they had left the unit. These individuals are: "ns1433" "ql3372" "qk4987" "nr9667" "nm6441"

comp$original_id[which(comp$notes=="not_in_eligibility_list")]

# There is one individual ("ns9433") for whom I do not have administrative data from any of the three waves. This person was not in my 'treated' list. Who is this individual? We should get administrative data for this individual - and confirm if there is anything special about them we should be aware of. 
mask_ids("ns9433")
which(basic$research_id=="rid_jc3565")
"ns9433"
