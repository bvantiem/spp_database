# =============================================================== ####
# Notes to Script ####
# -- Objective ####
# -- Readme ####
# Includes random assignment data from waves 1-7
# We are missing control numbers for 3 individuals, af4296 (lifer, treat wave 0), eq1988 (commuted death, treat wave 6), qp1616 (0-6m, treat wave 2)
# -- Todo ####
# =============================================================== ####
# Set up  ####
# -- Prepare Environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts_restricted_access/0_id_masking_function.R")
source("scripts_restricted_access/0_control_no_masking_function.R")
# -- Functions ####
`%ni%` = Negate(`%in%`)
# -- Set Seed ####
set.seed(1962)

# -- Read in Data  ####
# -- -- Control lookup table ####
control_lookup <- readRDS("data_restricted_access/processed/identified/1a_control_nos_inmate_ids.Rds")

# -- -- Treatment waves ####
randassign1 <- read_xlsx("data_restricted_access/raw/4_random_assignment/assignment/20220818_random_assignment_round1.xlsx", sheet=1) 
randassign2 <- xl.read.file("data_restricted_access/raw/4_random_assignment/assignment/20221201_random_assignment_round2.xlsx", password = "LS2022", xl.sheet=1)
randassign3 <- xl.read.file("data_restricted_access/raw/4_random_assignment/assignment/20231107_random_assignment_round3.xlsx", password = "LS2023", xl.sheet=1)

# Corrected assignments for waves 4 and 5
# This file contains all random assignments for waves 1-5 as held by CEN on 241101
randassign_all<- xl.read.file("data_restricted_access/raw/4_random_assignment/assignment/20241101_random_assignment_round12345.xlsx", password = "LS2024", xl.sheet=1)
randassign4 <- randassign_all[which(randassign_all$Cohort=="20231127" & randassign_all$Alternate==0),c("RandomAssignment", "inmate_number", "Strata")]
randassign5 <- randassign_all[which(randassign_all$Cohort=="20240605" & randassign_all$Alternate==0),c("RandomAssignment", "inmate_number", "Strata")]
randassign6 <- xl.read.file("data_restricted_access/raw/4_random_assignment/assignment/20250108_random_assignment_round123456.xlsx", xl.sheet=1)
randassign6 <- randassign6[which(is.na(randassign6$Cohort)),c("RandomAssignment", "inmate_number", "Strata")] # No date provided - asked Jordan for randomization date
randassign7 <- read_xlsx("data_restricted_access/raw/4_random_assignment/assignment/20250522_random_assignment_round7.xlsx", sheet=1) 

# -- -- Life sentenced individuals ####
# BH6778 and AY5858 were treated after one lifer was released due to commuted sentence
randassign0 <- data.frame(treated = c(rep(1,7), rep(0,7), rep(1,1), rep(0,2)),
                          inmate_id = c("as0160", "bf6556", "bm7165", "ce6457", "cy7366",
                                        "as2609","bh6778",
                                        "af4296", "am8328", "ap1973", "be8100", "eg5999",
                                        "bj8105", "ay5858",
                                        "dl3766", "eq1988", "jc9806"), 
                          stratum = c(rep("lifer",14), rep("commuted death", 3)),
                          treatment_wave = c(rep(0,6), 2.5, rep(0,6), 2.5, rep(6,3)),
                          treatment_date = ymd(c(rep(rand0_date,6), 
                                                 rand2.5_date,
                                             rep(rand0_date,6),
                                             rand2.5_date,
                                             rep(rand6_date, 3))))

# -- -- Release dates ####
# Using just the latest release date file which was updated ahead of wave 7
rel <- xl.read.file("data_restricted_access/raw/4_random_assignment/release_dates/release_dates_updated_prior_to_wave_7.xlsx", xl.sheet=1)
# ================================================================ ####
# Cleaning ####
# -- Release dates ####
rel <- rel |>
  rename(inmate_id = id_num) |>
  mutate(
    release_date = ymd(release_date),
    treatment_date = ymd(treatment_date),
    inmate_id = gsub(" ", "", inmate_id),
    release_type = gsub("(^ )(.*)", "\\2", release_type)
  )

# -- Prepare random assignment data for merging ####
names(randassign1) <- c("treated", "inmate_id", "stratum")

names(randassign2) <- c("treated", "inmate_id", "stratum") # align with names in first dataframe
randassign2$stratum <- with(randassign2, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

randassign3 <- randassign3[,-which(names(randassign3)=="Alternates")]
names(randassign3) <- c("treated", "inmate_id", "stratum")

randassign3$stratum <- with(randassign3, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

names(randassign4) <- c("treated", "inmate_id", "stratum") # align with names in first dataframe
randassign4$stratum <- with(randassign4, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

names(randassign5) <- c("treated", "inmate_id", "stratum") # align with names in first dataframe
randassign5$stratum <- with(randassign5, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

names(randassign6) <- c("treated", "inmate_id", "stratum") # align with names in first dataframe
randassign6$stratum <- with(randassign6, ifelse(
  stratum=="0-6 months", "00_06_m", ifelse(
    stratum=="6-12 months", "06_12_m", ifelse(
      stratum=="12-60 months", "12_60_m", ifelse(
        stratum==">60 months", "60_pl_m", NA)))))

randassign7 <- randassign7[,c(2:4)]
names(randassign7) <- c("treated", "inmate_id", "stratum") # align with names in first dataframe
randassign7$stratum <- with(randassign7, ifelse(
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
randassign7$treatment_wave <- 7

randassign1$treatment_date <- rand1_date
randassign2$treatment_date <- rand2_date
randassign3$treatment_date <- rand3_date
randassign4$treatment_date <- rand4_date
randassign5$treatment_date <- rand5_date
randassign6$treatment_date <- rand6_date # DATE TO BE CONFIRMED - confirming with Jordan, email sent 20250425
randassign7$treatment_date <- rand7_date

# ================================================================ ####
# Merge ####
# -- Merge assignment data into one dataframe ####
randassign <- rbind(randassign0, randassign1, randassign2, randassign3, randassign4, randassign5, randassign6, randassign7)
randassign$inmate_id <- tolower(randassign$inmate_id)

# -- Merge in release dates ####
rel <- rel %>%
  rename(release_from_unit_date = release_date) %>%
  rename(release_type_raw = release_type) %>%
  mutate(release_type = case_when(
    release_type_raw %in% c("Removed - Misconducts", 
                            "Removed from unit due to misconduct(s) on ",
                            "Removal to BB Unit",
                            "Removed due to misconduct/behavior",
                            "Removal to RHU") ~ "Removed From LSU",
    release_type_raw %in% c("Never on CA - May have been selected and replaced due to ATA", # This is not technically a refusal - discuss. TODO
                            "Refused Program, moved to AC unit",
                            "Refused Participation",
                            "Never on CA - Refusal from 2nd wave",
                            "never on CA he was selected and then refused participation",
                            "Never Selected",
                            "Refused to move to CA. Paroled") ~ "Refused to Move to LSU",
    release_type_raw %in% c("Transferred to SCI BEN to QUE ",
                            "Transfer",
                            "Hospital",
                            "Transferred to SCI BEN 1/31/23 to be paroled to ICE Detainer/Deportation ",
                            "Transferred to SCI Benner on day of moving onto unit (wave 2) ",
                            "Transferred - ICE Detainer",
                            "Transferred to LAU for mandatory programming",
                            "Transferred to QUE for SDTP",
                            "Camp Hill Transfer") ~ "Transferred Out of CHS",
    is.na(release_type_raw) ~ "Not Yet Released",
    TRUE ~ "Released to Community"
  )) %>%
  # Temporarily replace release dates for these individuals with their treatment date
  # Confirm these are the correct dates - might be that they moved for a few days until they were moved back? TODO
  mutate(release_from_unit_date = if_else(release_type == "Refused to Move to LSU", treatment_date, release_from_unit_date)) %>%
  select(-treatment_date, - treatment_wave)

randassign <- left_join(randassign, rel, by = c("inmate_id", "treated", "stratum"))
randassign$release_type_removed <- ifelse(randassign$release_type == "Removed From LSU", 1, 0)
randassign$release_type_refused <- ifelse(randassign$release_type == "Refused to Move to LSU", 1, 0)
randassign$release_type_transferred <- ifelse(randassign$release_type == "Transferred Out of CHS", 1, 0)
randassign$release_type_community <- ifelse(randassign$release_type == "Released to Community", 1, 0)

# -- Link inmate ids to control numbers ####
# -- Pivot control_lookup to long format
control_long <- control_lookup |>
  mutate(across(.cols = starts_with("inmate_id_"),.fns = tolower)) |>
  pivot_longer(
    cols = starts_with("inmate_id_"),
    names_to = "inmate_id_col",
    values_to = "inmate_id",
    values_drop_na = TRUE  # Drops rows where second, third, etc. inmate_id is NA
  ) |>
  select(control_number, inmate_id)

# -- Join with control_lookup on inmate_id
randassign <- randassign |>
  left_join(control_long, by = "inmate_id") %>%
  relocate(control_number)

randassign <- randassign %>%
  relocate(control_number, .after = inmate_id)

# Rename and reorder variables 
randassign <- randassign %>%
  rename(rct = treated,
         rct_stratum = stratum,
         rct_treat_wave = treatment_wave,
         rct_treat_dt = treatment_date,
         rct_notes = notes,
         rct_release_dt = release_from_unit_date,
         rct_release = release_type,
         rct_release_removed = release_type_removed,
         rct_release_refused = release_type_refused,
         rct_release_transferred = release_type_transferred,
         rct_release_community = release_type_community) %>%
  relocate(inmate_id,
           control_number,
           rct,
           rct_stratum,
           rct_treat_wave,
           rct_treat_dt,
           rct_release,
           rct_release_dt,
           rct_release_removed,
           rct_release_refused,
           rct_release_transferred,
           rct_release_community)

# ================================================================ ####
# Save Dataframes
# -- Save unmasked file ####
stopifnot(length(unique(randassign$inmate_id))==nrow(randassign))
saveRDS(randassign, file="data_restricted_access/processed/identified/1b_randassign.Rds")

# -- Save masked file ####
i <- unique(control_lookup$control_number)
id.link <- mask_control_nos(i) # Generate masked Research IDs

# Missing data on 3 individuals in our sample! TODO ####
# Temporarily assign them zz9999 
randassign[which(is.na(randassign$control_number)),"control_number"] <- "zz9999"

randassign_masked <- randassign %>%
  left_join(id.link, by = "control_number") %>%
  select(-any_of(c("inmate_id", "control_number"))) %>%
  relocate(research_id) # moves research id to the front

saveRDS(randassign_masked, file="data/processed/de_identified/1b_randassign_masked.Rds")
# ================================================================ ####

# OLD ####
# Old release date code
# rel1 <- read.delim("data_restricted_access/raw/4_random_assignment/release_dates/230504_release_dates.txt", header = TRUE, sep = "-", dec = ".", row.names = NULL)
# rel1$release_date <- mdy(rel1$release_date)
# rel1$id_num <- gsub(" ", "", rel1$id_num)
# rel2 <- xl.read.file("data_restricted_access/raw/4_random_assignment/release_dates/20231031_ca_releases_between_r3_and_r4.xlsx", password = "LS2023", xl.sheet=1)
# rel2$release_date <- ymd(rel2$release_date)
# rel2$release_date[which(rel2$id_num=="QP4844")] <- ymd(20230519) # Individual never moved to unit, so set release date to treatment date
# rel2$id_num <- gsub(" ", "", rel2$id_num)
# rel3 <- xl.read.file("data_restricted_access/raw/4_random_assignment/assignment/tbl_00_LSStudyParticipants_20240510 pw.xlsx", password = "LS2024", xl.sheet=1) # This file includes release dates that I haven't yet reviewed
# rel3 <- rel3[!is.na(rel3$DateLeft),c("inmate_number","DateLeft")]
# names(rel3)[which(names(rel3)=="inmate_number")] <- "id_num"
# names(rel3)[which(names(rel3)=="DateLeft")] <- "release_date"
# rel3$release_type <- NA
# rel3 <- rel3[,c("id_num", "release_type", "release_date")]
# rel3$id_num <- gsub(" ", "", rel3$id_num) # Remove spaces in ID numbers

