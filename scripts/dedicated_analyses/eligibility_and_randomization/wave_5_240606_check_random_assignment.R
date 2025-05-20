library("excel.link")
library("writexl")
source("scripts/0_id_masking_function.R")

# Check balance in previous rounds
randassign <- readRDS("data/processed/randassign.Rds")
table(randassign$treated, randassign$stratum)

# Check balance in this round
# Correctly assigned across cohorts
wave5 <- xl.read.file("data/raw/4_random_assignment/assignment/20240605_random_assignment_round5.xlsx", password = "LS2024", xl.sheet=1)
table(wave5$RandomAssignment, wave5$Strata)

# Confirmed that none of the ids are already in the treated pool
names(wave5)[which(names(wave5)=="inmate_number")] <- "original_id"
wave5$original_id <- tolower(wave5$original_id)
i <- wave5$original_id
id.link <- mask_ids(i) # Generate masked IDs
wave5 <- left_join(wave5, id.link, by="original_id") # Merge basic on original ID
wave5$research_id %in% randassign$research_id[which(randassign$treatment_wave!=5)]

# Confirm all newly assigned people were eligible in eligibility spreadsheet
round5 <- xl.read.file("data/raw/4_random_assignment/eligibility/20240415_LSRecruitmentData_CEN_round5.xlsx", xl.sheet=1, password="LS2024")
names(round5)[which(names(round5)=="inmate_number")] <- "original_id"
round5$original_id <- tolower(round5$original_id)
i <- round5$original_id
id.link <- mask_ids(i) # Generate masked IDs
round5 <- left_join(round5, id.link, by="original_id") # Merge basic on original ID
round5 <- round5[which(round5$LSEligible=="Maybe"),]
wave5$research_id %in% round5$research_id
