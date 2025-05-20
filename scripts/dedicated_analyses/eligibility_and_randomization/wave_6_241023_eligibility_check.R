# ---------- Data set-up ---------- ----
rm(list=ls())

## Set Seed ----
set.seed(1962)

# Source ####
source("scripts/00_packages.R")
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")

randassign <- readRDS("data/processed/randassign.Rds")
stopifnot(length(unique(randassign$research_id))==nrow(randassign))
round6 <- xl.read.file("data/raw/4_random_assignment/eligibility/20241021_tbl_99_LSRecruitmentData_eligibility_round6.xlsx", xl.sheet=1, password="LS2024")
names(round6)[which(names(round6)=="Z-CodeFail")] <- "Z_CodeFail"

exclusions <- xl.read.file("data/raw/4_random_assignment/opt_outs_exclusions/20241021_LSExclusions_r1_6.xlsx", xl.sheet=1, password="LS2024")

exclusions$inmate_number <- tolower(exclusions$InmateID)
exclusions$research_id <- mask_ids(exclusions$inmate_number)$research_id
round6$inmate_number <- tolower(round6$inmate_number)
round6$research_id <- mask_ids(round6$inmate_number)$research_id #1035

# Exclusions
# Merge IDs who were included in more than one round into one row
exclusions <- exclusions %>%
  group_by(inmate_number) %>%
  mutate(across(starts_with("Opt")|starts_with("CHS"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  distinct(across(starts_with("Opt")|starts_with("CHS")|"inmate_number"), .keep_all=TRUE) %>%
  mutate(SecFail = ifelse(CHS_Sec_R6>0, 1,0)) %>%
  mutate(OptOutFail = ifelse(Optout_R1+Optout_R2+Optout_R3+Optout_R4+Optout_R6>0, 1, 0))

# Merge files
round6 <- left_join(round6, exclusions[,c("research_id", "SecFail", "OptOutFail")], by = "research_id")
round6 <- left_join(round6, randassign[,c("research_id", "treated")], by = "research_id")
round6$TreatedFail <- ifelse(round6$treated %in% c(0,1), 1, 0)

# Round 6 clean file
fail_vars <- c("Z_CodeFail", "MinFail", "TCFail", "STGFail", "StaffAssaultFail", "ViolentMisconFail", "DrugMisconFail", "RHUFail", "LSExclusions", "SecFail", "OptOutFail", "TreatedFail")
round6 <- round6 %>%
  mutate(across(all_of(fail_vars), \(x) ifelse(is.na(x), 0, x))) %>%
  mutate(FailTotal = rowSums(across(all_of(fail_vars)), na.rm = TRUE)) %>%
  mutate(LSEligible_updated = ifelse(FailTotal>0, "No", "Maybe"))

# Check - Failure variables ####
# Reasons for failure
for(i in fail_vars){
  print(i)
  k <- which(names(round6)==i)
  print(length(which(round6[,k]==1)))
}

# Check Individuals who are eligible on our books but not on CHS's
# These two individuals are both in the infirmary. Update Manually
round6[which(round6$LSEligible_updated=="Maybe" & round6$LSEligible=="No"),]
round6[which(round6$LSEligible_updated=="Maybe" & round6$LSEligible=="No"),"LSEligible_updated"] <- "No"

# Check Individuals who are ineligible on our books but were eligible on CHS's
round6[which(round6$LSEligible_updated=="No" & round6$LSEligible=="Maybe"),]
# These are all individuals who failed security or opted out

# Double-check no treated individuals are classified as maybe
table(round6$treated, round6$LSEligible, useNA = "always")
table(round6$treated, round6$LSEligible_updated, useNA = "always")

# 12 individuals have shifted to No because of security or opt out restrictions
# Otherwise everything has stayed the same
table(round6$LSEligible, round6$LSEligible_updated, useNA = "always")

# Move LSEligible_update column next to LSElgible column
round6 <- round6 %>%
  relocate(LSEligible_updated, .after = LSEligible)

# Examine eligible individuals
table(round6[which(round6$LSEligible_updated=="Maybe"), "Strata"])

# Save file
write_xlsx(round6, "data/raw/4_random_assignment/eligibility/20241026_tbl_99_LSRecruitmentData_eligibility_round6_updated.xlsx")
