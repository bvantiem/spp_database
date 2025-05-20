# ---------- Data set-up ---------- ----
rm(list=ls())

## Set Seed ----
set.seed(1962)

# Source ####
source("scripts/00_packages.R")
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")

randassign <- readRDS("data/processed/randassign.Rds")
stopifnot(length(unique(randassign$id_num))==nrow(randassign))
round7 <- xl.read.file("data/raw/4_random_assignment/eligibility/tbl_99_LSRecruitmentDataCriteriaChecks_20250516.xlsx", xl.sheet=1)
names(round7)[which(names(round7)=="Z-CodeFail")] <- "Z_CodeFail"
round7$id_num <- tolower(round7$inmate_number)
# Use file from wave 6 as no opt outs ahead of wave 7
exclusions <- xl.read.file("data/raw/4_random_assignment/opt_outs_exclusions/20241021_LSExclusions_r1_6.xlsx", xl.sheet=1, password="LS2024")
exclusions$id_num <- tolower(exclusions$InmateID)


# Exclusions
# Merge IDs who were included in more than one round into one row
exclusions <- exclusions %>%
  group_by(id_num) %>%
  mutate(across(starts_with("Opt")|starts_with("CHS"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  distinct(across(starts_with("Opt")|starts_with("CHS")|"id_num"), .keep_all=TRUE) %>%
  mutate(SecFail = 0) %>% # Not applicable in wave 7 as no new exlusions
  mutate(OptOutFail = ifelse(Optout_R1+Optout_R2+Optout_R3+Optout_R4+Optout_R6>0, 1, 0))

# Merge files
round7 <- left_join(round7, exclusions[,c("id_num", "SecFail", "OptOutFail")], by = "id_num")
round7 <- left_join(round7, randassign[,c("id_num", "treated")], by = "id_num")
round7$TreatedFail <- ifelse(round7$treated %in% c(0,1), 1, 0)

# Round 6 clean file
fail_vars <- c("Z_CodeFail", "MinFail", "TCFail", "STGFail", "StaffAssaultFail", "ViolentMisconFail", "DrugMisconFail", "RHUFail", "LSExclusions", "SecFail", "OptOutFail", "TreatedFail")
round7 <- round7 %>%
  mutate(across(all_of(fail_vars), \(x) ifelse(is.na(x), 0, x))) %>%
  mutate(FailTotal = rowSums(across(all_of(fail_vars)), na.rm = TRUE)) %>%
  mutate(LSEligible_updated_research_team_review = ifelse(FailTotal>0, "No", "Maybe"))

# Check - Failure variables ####
# Reasons for failure
for(i in fail_vars){
  print(i)
  k <- which(names(round7)==i)
  print(length(which(round7[,k]==1)))
}

# Check Individuals who are eligible on our books but not on CHS's
round7[which(round7$LSEligible_updated_research_team_review=="Maybe" & round7$LSEligible=="No"),]

# Manually override some who were correctly assigned no's
# Two individuals appear to have been treated in April 2025? No data on this has been shared with me. ("am8176", "bm8320")
# These individuals are in the infirmary ("qq7024", "jb3289")
round7[which(round7$LSEligible_updated_research_team_review=="Maybe" & round7$LSEligible=="No" & round7$id_num %in% c("am8176", "bm8320", "qq7024", "jb3289")),"LSEligible_updated_research_team_review"] <- "No"

# The remaining individuals may have been blocked because of offense type? If this is happening it should be tracked as a security veto
round7[which(round7$LSEligible_updated_research_team_review=="Maybe" & round7$LSEligible=="No"), ]
round7[which(round7$LSEligible_updated_research_team_review=="Maybe" & round7$LSEligible=="No"), "id_num"]

# Check Individuals who are ineligible on our books but were eligible on CHS's
# These are all lifers
round7[which(round7$LSEligible_updated_research_team_review=="No" & round7$LSEligible=="Maybe"),]
round7[which(round7$LSEligible_updated_research_team_review=="No" & round7$LSEligible=="Maybe"),"id_num"]
randassign[which(randassign$id_num %in% c("as2609", "ay5858", "be8100", "bh6778", "bj8105", "am8328", "as0160", "cy7366", "bm7165", "ce6457")),]


# Move LSEligible_update column next to LSElgible column
round7$LSEligible_discrepancies <- ifelse(round7$LSEligible != round7$LSEligible_updated_research_team_review, 1, 0)
round7 <- round7 %>%
  relocate(LSEligible_updated_research_team_review, .after = LSEligible) %>%
  relocate(LSEligible_discrepancies, .after = LSEligible_updated_research_team_review)
round7[which(round7$LSEligible_discrepancies==1),]

# Examine eligible individuals
table(round7[which(round7$LSEligible_updated_research_team_review=="Maybe"), "Strata"])

# Save file
write_xlsx(round7, "data/raw/4_random_assignment/eligibility/20250520_tbl_99_LSRecruitmentData_eligibility_round7_updated.xlsx")

# Emailed Jordan on 5/20/2025 at around 3:40pm from umd email
