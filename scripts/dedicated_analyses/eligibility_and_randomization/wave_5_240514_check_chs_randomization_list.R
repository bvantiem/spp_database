library("excel.link")
source("scripts/0_id_masking_function.R")
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# Load my random assignment list for waves 1234
randassign_cen <- xl.read.file("data/raw/4_random_assignment/assignment/tbl_00_LSStudyParticipants_20240510 pw.xlsx", password = "LS2024", xl.sheet=1)
randassign <- readRDS(file="data/processed/randassign.Rds")
randassign_cen$inmate_number <- tolower(randassign_cen$inmate_number)
randassign_cen$research_id <- mask_ids(randassign_cen$inmate_number)$research_id


# The CEN list does not include individuals not in my list
randassign_cen[which(randassign_cen$research_id %ni% randassign$research_id &
                    randassign_cen$Alternate==0),]

randassign[which(randassign$research_id %ni% randassign_cen$research_id &
                       randassign_cen$Alternate==0),]

# 1 individual too many assigned to control?
table(randassign$treatment_wave, randassign$treated)
prop.table(table(randassign$treatment_wave, randassign$treated), margin=1)

# They do not appear to have been assigned a stratum. Strata are balanced
temp <- randassign_cen[which(randassign_cen$Cohort=="20231127" & randassign_cen$Alternate==0),]
with(temp, table(RandomAssignment))
with(temp, table(RandomAssignment, Strata))

# Integrity checks passed ####
# Only duplicated IDs in here are those that were alternates in a previous round
temp <- randassign_cen[which(randassign_cen$research_id %in% c(randassign_cen$research_id[which(duplicated(randassign_cen$research_id))])),]
temp <- temp[which(temp$Alternate==0),]
