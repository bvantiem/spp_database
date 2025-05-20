library("excel.link")

# Load my random assignment list for waves 123 and load CEN list
randassign4 <- xl.read.file("data/raw/4_random_assignment/assignment/tbl_00_LSStudyParticipants_202404.xlsx", password = "LS2024", xl.sheet=1)
randassign <- readRDS(file="data/processed/randassign.Rds")
randassign4$inmate_number <- tolower(randassign4$inmate_number)
randassign4$research_id <- mask_ids(randassign4$inmate_number)$research_id


# Issue 1 ####
# The CEN list includes 1 individual in the control group from wave 1 that I don't have on my list: nn0585
# My list appears correct as the assignment is balanced (29(t), 58(c)). The additional individual brings it to 59(c).
randassign4[which(randassign4$research_id %ni% randassign$research_id &
                    randassign4$Cohort!="20231127" &
                    randassign4$Alternate==0),]

table(randassign$treatment_wave, randassign$treated)
prop.table(table(randassign$treatment_wave, randassign$treated), margin=1)
with(randassign4[which(randassign4$Cohort=="20220502" & randassign4$Alternate==0),], table(RandomAssignment))

# Issue 2: Random assignment in fourth wave ####
# Two individuals too many assigned to the control group in the 0-6 Months Stratum. We've got 18(c) and 8(t).
temp <- randassign4[which(randassign4$Cohort=="20231127" & randassign4$Alternate==0),]
with(temp, table(RandomAssignment))
with(temp, table(RandomAssignment, Strata))

# Integrity checks passed ####
# Only duplicated IDs in here are those that were alternates in a previous round
temp <- randassign4[which(randassign4$research_id %in% c(randassign4$research_id[which(duplicated(randassign4$research_id))])),]
temp <- temp[which(temp$Alternate==0),]
