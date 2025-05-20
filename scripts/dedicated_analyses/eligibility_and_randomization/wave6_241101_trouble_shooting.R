# Old data wave 4
randassign4 <- xl.read.file("data/raw/4_random_assignment/assignment/20240510_random_assignment_round4_donotuse_has_errors.xlsx", password = "LS2024", xl.sheet=1)
randassign4<- randassign4[,-which(names(randassign4)=="DateLeft")]
randassign4 <- randassign4[-with(randassign4, which(Alternate==1 & Cohort=="20231127")),] # These are alternates that we confirmed were never assigned
randassign4 <- randassign4[which(randassign4$Cohort=="20231127"),c("RandomAssignment", "inmate_number", "Strata")]

# Old data wave 5
randassign5 <- xl.read.file("data/raw/4_random_assignment/assignment/20240605_random_assignment_round5_donotuse_has_errors.xlsx", password = "LS2024", xl.sheet=1)
randassign5 <- randassign5[,c("RandomAssignment", "inmate_number", "Strata")]

# New data all waves
randassign_all<- xl.read.file("data/raw/4_random_assignment/assignment/20241101_random_assignment_round12345.xlsx", password = "LS2024", xl.sheet=1)

# Check round 4
randassign_all_4 <- randassign_all[which(randassign_all$Cohort=="20231127" & randassign_all$Alternate==0),c("RandomAssignment", "inmate_number", "Strata")]
table(randassign4$Strata)
table(randassign_all_4$Strata)

table(randassign4$RandomAssignment)
table(randassign_all_4$RandomAssignment)

randassign4==randassign_all_4

# Check round 5
randassign_all_5 <- randassign_all[which(randassign_all$Cohort=="20240605" & randassign_all$Alternate==0),c("RandomAssignment", "inmate_number", "Strata")]
table(randassign5$Strata)
table(randassign_all_5$Strata)

table(randassign5$RandomAssignment)
table(randassign_all_5$RandomAssignment)

randassign5==randassign_all_5
unique(randassign5$inmate_number)

