# Notes to Script ------------------------- ####
# -- Script Objective: 	Checking Preliminary Applicant Data	(Pre-Eligibility Check)
# -- Script Notes: This script is up to date for waves 1-6

# Set up ------------------------- ####
# Libraries ####
rm(list=ls())
library(readxl)
library("lubridate")
library("excel.link")
library("writexl")
source("scripts/0_id_masking_function.R")
source("scripts/0_control_no_masking_function.R")
`%ni%` = Negate(`%in%`)

# Set Seed ####
set.seed(1962)

# Load data  ####
control <- readRDS("data/processed/processing_layer_1/control_nos_inmate_ids.Rds")
# Life sentenced individuals ####
# BH6778 and AY5858 were treated after one lifer was released due to commuted sentence
randassign0 <- data.frame(treated = c(rep(1,7), rep(0,7), rep(1,1), rep(0,2)),
                          id_num = c("as0160", "bf6556", "bm7165", "ce6457", "cy7366",
                                     "as2609","bh6778",
                                     "af4296", "am8328", "ap1973", "be8100", "eg5999",
                                     "bj8105", "ay5858",
                                     "dl3766", "eq1988", "jc9806"), 
                          stratum = c(rep("lifer",14), rep("commuted death", 3)),
                          treatment_wave = c(rep(0,6), 2.5, rep(0,6), 2.5, rep(NA,3)),
                          treatment_date = c(rep(NA,6), "2023/4/4", rep(NA,6), "2023/4/4", rep("2024/11/6", 3)))

# Treatment waves ####
randassign1 <- read_xlsx("data/raw/4_random_assignment/assignment//20220818_random_assignment_round1.xlsx", sheet=1) # randassign1ndom assignment
randassign2 <- xl.read.file("data/raw/4_random_assignment/assignment/20221201_random_assignment_round2.xlsx", password = "LS2022", xl.sheet=1)
randassign3 <- xl.read.file("data/raw/4_random_assignment/assignment/20231107_random_assignment_round3.xlsx", password = "LS2023", xl.sheet=1)

# Corrected assignments for waves 4 and 5
# This file contains all random assignments for waves 1-5 as held by CEN on 241101
randassign_all<- xl.read.file("data/raw/4_random_assignment/assignment/20241101_random_assignment_round12345.xlsx", password = "LS2024", xl.sheet=1)
randassign4 <- randassign_all[which(randassign_all$Cohort=="20231127" & randassign_all$Alternate==0),c("RandomAssignment", "inmate_number", "Strata")]
randassign5 <- randassign_all[which(randassign_all$Cohort=="20240605" & randassign_all$Alternate==0),c("RandomAssignment", "inmate_number", "Strata")]
randassign6 <- xl.read.file("data/raw/4_random_assignment/assignment/20250108_random_assignment_round123456.xlsx", xl.sheet=1)
randassign6 <- randassign6[which(is.na(randassign6$Cohort)),c("RandomAssignment", "inmate_number", "Strata")] # No date provided - asked Jordan for randomization date

# Release dates ####
# Using just the latest release date file which was updated ahead of wave 7
# Old release date code at the bottom of this script
rel <- xl.read.file("data/raw/4_random_assignment/release_dates/release_dates_updated_prior_to_wave_7.xlsx", xl.sheet=1)
names(rel)[which(names(rel)=="original_id")] <- "id_num"
# rel <- rel[,-which(names(rel)=="treatment_date")]
rel$release_date <- ymd(rel$release_date)

stopifnot(length(unique(rel$id_num))==nrow(rel))

# Fix dataframes ####
names(randassign2) <- c("treated", "id_num", "stratum") # align with names in first dataframe
randassign2$stratum <- with(randassign2, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

randassign3 <- randassign3[,-which(names(randassign3)=="Alternates")]
names(randassign3) <- c("treated", "id_num", "stratum")

randassign3$stratum <- with(randassign3, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

names(randassign4) <- c("treated", "id_num", "stratum") # align with names in first dataframe
randassign4$stratum <- with(randassign4, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

names(randassign5) <- c("treated", "id_num", "stratum") # align with names in first dataframe
randassign5$stratum <- with(randassign5, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

names(randassign6) <- c("treated", "id_num", "stratum") # align with names in first dataframe
randassign6$stratum <- with(randassign6, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

randassign1$treatment_wave <- 1
randassign2$treatment_wave <- 2
randassign3$treatment_wave <- 3
randassign4$treatment_wave <- 4
randassign5$treatment_wave <- 5
randassign6$treatment_wave <- 6

randassign1$treatment_date <- "2022/05/02"
randassign2$treatment_date <- "2022/11/14"
randassign3$treatment_date <- "2023/5/19"
randassign4$treatment_date <- "2023/11/27"
randassign5$treatment_date <- "2024/06/05"
randassign6$treatment_date <- "2999/01/01" # DATE MISSING - confirming with Jordan, email sent 20250425

# Merge assignment data into one dataframe ####
randassign <- rbind(randassign0, randassign1, randassign2, randassign3, randassign4, randassign5, randassign6)
randassign$id_num <- tolower(randassign$id_num)
rm(randassign0, randassign1, randassign2, randassign3, randassign4, randassign5, randassign6)

# Notes on specific individuals ####
randassign$notes <- NA
randassign$notes[which(randassign$id_num=="qn1884")] <- "left_ls_2022_05_19"
randassign$notes[which(randassign$id_num=="ns9433")] <- "missing_admin_data"
randassign$notes[which(randassign$id_num=="qn2340")] <- "removed_ls_2023_04"

# Merge in release dates ####
rel$release_date[which(rel$release_date=="")] <- NA
rel$id_num <- gsub(" ", "", rel$id_num)
rel$release_type <- gsub("(^ )(.*)", "\\2", rel$release_type)
rel$release_date <- gsub("(^ )(.*)", "\\2", rel$release_date)
rel$release_date <- as.Date(rel$release_date)

randassign <- left_join(randassign, rel)

# Link random assignment with control numbers
control$inmate_id_1 <- tolower(control$inmate_id_1)
control$inmate_id_2 <- tolower(control$inmate_id_2)
# Join on id_1
join_1 <- randassign |>
  left_join(control[,c("control_number", "inmate_id_1")], by = c("id_num" = "inmate_id_1"))

# Join on id_2
join_2 <- randassign |>
  left_join(control[,c("control_number", "inmate_id_2")], by = c("id_num" = "inmate_id_2"))

# Merge results: prefer match on id_1, fallback to id_2
randassign <- join_1 |>
  mutate(
    control_number = coalesce(control_number, join_2$control_number)
  ) 

# Missing data on 3 individuals in our sample! ####
randassign[which(is.na(randassign$control_number)),]

randassign <- randassign %>%
  relocate(control_number, .after = id_num)

# Save unmasked file ####
stopifnot(length(unique(randassign$id_num))==nrow(randassign))
saveRDS(randassign, file="data/processed/randassign.Rds")

# Mask IDs  ====================== ####
i <- unique(control$control_number)
id.link <- mask_control_nos(i) # Generate masked Research IDs

randassign_masked <- randassign %>%
    left_join(id.link, by = "control_number") %>%
    select(-any_of(c("id_num", "control_number"))) %>%
    relocate(research_id) # moves research id to the front
  
# Save masked file ####
saveRDS(randassign_masked, file="data/processed/randassign_masked.Rds")


# OLD ####
# Old release date code
# rel1 <- read.delim("data/raw/4_random_assignment/release_dates/230504_release_dates.txt", header = TRUE, sep = "-", dec = ".", row.names = NULL)
# rel1$release_date <- mdy(rel1$release_date)
# rel1$id_num <- gsub(" ", "", rel1$id_num)
# rel2 <- xl.read.file("data/raw/4_random_assignment/release_dates/20231031_ca_releases_between_r3_and_r4.xlsx", password = "LS2023", xl.sheet=1)
# rel2$release_date <- ymd(rel2$release_date)
# rel2$release_date[which(rel2$id_num=="QP4844")] <- ymd(20230519) # Individual never moved to unit, so set release date to treatment date
# rel2$id_num <- gsub(" ", "", rel2$id_num)
# rel3 <- xl.read.file("data/raw/4_random_assignment/assignment/tbl_00_LSStudyParticipants_20240510 pw.xlsx", password = "LS2024", xl.sheet=1) # This file includes release dates that I haven't yet reviewed
# rel3 <- rel3[!is.na(rel3$DateLeft),c("inmate_number","DateLeft")]
# names(rel3)[which(names(rel3)=="inmate_number")] <- "id_num"
# names(rel3)[which(names(rel3)=="DateLeft")] <- "release_date"
# rel3$release_type <- NA
# rel3 <- rel3[,c("id_num", "release_type", "release_date")]
# rel3$id_num <- gsub(" ", "", rel3$id_num) # Remove spaces in ID numbers

