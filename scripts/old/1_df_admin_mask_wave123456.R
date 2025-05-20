rm(list=ls())
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library("excel.link")
source("scripts/0_id_masking_function.R")
`%ni%` = Negate(`%in%`)

# Notes ####
# Each wave pulls a full history.
# So we have duplicate rows across waves. We delete those duplicate rows, keeping only the first time they showed up
# So id someone is in wave 1 and wave 2, we keep only the original rows from wave 1 and add only new (non-duplicate) rows from wave 2.

# Functions ####
add_wave_data <- function(df, df2, date_datapull, wave_no){
  # Sometimes the only difference is whitespace - we don't want those rows to be seen as different
  df2 <- df2 %>% mutate(across(where(is.character), str_trim))

  # Bind the dataframes together, except the wave columns
  df_combined <- rbind(df[,which(names(df) %ni% c("wave", "date_datapull", "control_number_pull"))], df2)

  # Subset to the rows in the new dataframe that are new
  df2_new <- df2[!duplicated(df_combined)[(nrow(df) + 1):nrow(df_combined)],]


  df2_new <- df2_new %>%
    mutate(wave = wave_no) %>%
    mutate(date_datapull = ymd(date_datapull)) %>%
    mutate(control_number_pull = paste0(control_number, paste0("_", wave_no)))
  df <- rbind(df, df2_new)
  return(df)
}

# TO FIX: Control numbers Issue! Emailed Jordan 10/12/24 ####
# Control numbers appear to be unique to inmate_ids within data pulls.
# For example, control number 136257 belongs to DL8705 in waves 1 and 2, and to QP2490 in waves 3, 4, and 5
# Have not used the below function yet as it gets complicated quickly! Set aside several hours.
update_control_numbers <- function(df1, df2) {

  # Step 1: Extract the base six-digit part of the control numbers and the suffix
  df1 <- df1 %>%
    mutate(base_control = sub("_.*", "", control_number),
           suffix = sub(".*_", "", control_number))

  df2 <- df2 %>%
    mutate(base_control = sub("_.*", "", control_number),
           suffix = sub(".*_", "", control_number))

  # Step 2: Identify overlapping control numbers
  overlapping_controls <- intersect(df1$base_control, df2$base_control)

  # Step 3: Check for overlapping control numbers and compare IDs
  df2_updated <- df2 %>%
    # Filter rows where control numbers overlap with df1
    filter(base_control %in% overlapping_controls) %>%
    left_join(df1, by = "base_control", suffix = c("_df2", "_df1")) %>%
    # Update control numbers in df2 where IDs do not match
    mutate(
      control_number = if_else(
        inmate_number_df1 != inmate_number_df2,
        paste0(base_control, "_", as.numeric(suffix_df1) + 1),
        control_number_df2
      )
    ) %>%
    # Select the original columns
    select(inmate_number = inmate_number_df2, control_number)

  return(df2_updated)
}

# Wave 1a ####
# -- Read in Admin Data
basic1 <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=1)
move1 <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=4)
assess1 <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=5)
house1 <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=6)
program1 <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=7)
conduct1 <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=8)
work1 <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=9)
visit1 <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=10)

move1 <- move1 %>% distinct()

for (df_name in c("basic1", "move1", "assess1", "house1", "program1", "conduct1", "work1", "visit1")) {
  assign(df_name, get(df_name) %>%
           mutate(wave = 1) %>%
           mutate(date_datapull = ymd(20220625)) %>%
           mutate(control_number_pull = paste0(control_number, "_1a")) %>%
           mutate(across(where(is.character), str_trim)))
}

basic <- basic1
move <- move1
assess <- assess1
house <- house1
program <- program1
conduct <- conduct1
work <- work1
visit <- visit1

# Wave 1b ####
basic1_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=1, password="LS_2022")
move1_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=2, password="LS_2022")
assess1_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=3, password="LS_2022")
house1_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=4, password="LS_2022")
program1_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=5, password="LS_2022")
conduct1_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=6, password="LS_2022")
work1_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=7, password="LS_2022")
visit1_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=8, password="LS_2022")

move1_cor <- move1_cor %>% distinct()

# -- Merge in
date_datapull <- 20220903
wave_no <- 1.1

basic <- add_wave_data(basic, basic1_cor, date_datapull, wave_no)
move <- add_wave_data(move, move1_cor, date_datapull, wave_no)
assess <- add_wave_data(assess, assess1_cor, date_datapull, wave_no)
house <- add_wave_data(house, house1_cor, date_datapull, wave_no)
program <- add_wave_data(program, program1_cor, date_datapull, wave_no)
conduct <- add_wave_data(conduct, conduct1_cor, date_datapull, wave_no)
work <- add_wave_data(work, work1_cor, date_datapull, wave_no)
visit <- add_wave_data(visit, visit1_cor, date_datapull, wave_no)

# Wave 2 ####
# -- Read in Admin Data
# Note: somehow these take very long to read in
basic2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=1, password="LS2022")
move2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=2, password="LS2022")
assess2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=3, password="LS2022")
house2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=4, password="LS2022")
program2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=5, password="LS2022")
conduct2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=6, password="LS2022")
work2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=7, password="LS2022")
visit2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=8, password="LS2022")

# -- Merge in
date_datapull <- 20220912
wave_no <- 2

basic <- add_wave_data(basic, basic2, date_datapull, wave_no)
move <- add_wave_data(move, move2, date_datapull, wave_no)
assess <- add_wave_data(assess, assess2, date_datapull, wave_no)
house <- add_wave_data(house, house2, date_datapull, wave_no)
program <- add_wave_data(program, program2, date_datapull, wave_no)
conduct <- add_wave_data(conduct, conduct2, date_datapull, wave_no)
work <- add_wave_data(work, work2, date_datapull, wave_no)
visit <- add_wave_data(visit, visit2, date_datapull, wave_no)

# Wave 3 ####
# -- Read in Admin Data
# Note: somehow these take very long to read in
basic3 <- xl.read.file("data/raw/1_admin_data/pcq_wave3/PrisonClimateSurveyCHSData_20230410.xlsx", xl.sheet=1, password="LS2023")
move3 <- xl.read.file("data/raw/1_admin_data/pcq_wave3/PrisonClimateSurveyCHSData_20230410.xlsx", xl.sheet=2, password="LS2023")
assess3 <- xl.read.file("data/raw/1_admin_data/pcq_wave3/PrisonClimateSurveyCHSData_20230410.xlsx", xl.sheet=3, password="LS2023")
house3 <- xl.read.file("data/raw/1_admin_data/pcq_wave3/PrisonClimateSurveyCHSData_20230410.xlsx", xl.sheet=4, password="LS2023")
program3 <- xl.read.file("data/raw/1_admin_data/pcq_wave3/PrisonClimateSurveyCHSData_20230410.xlsx", xl.sheet=5, password="LS2023")
conduct3 <- xl.read.file("data/raw/1_admin_data/pcq_wave3/PrisonClimateSurveyCHSData_20230410.xlsx", xl.sheet=6, password="LS2023")
work3 <- xl.read.file("data/raw/1_admin_data/pcq_wave3/PrisonClimateSurveyCHSData_20230410.xlsx", xl.sheet=7, password="LS2023")
visit3 <- xl.read.file("data/raw/1_admin_data/pcq_wave3/PrisonClimateSurveyCHSData_20230410.xlsx", xl.sheet=8, password="LS2023")

# -- Merge in
date_datapull <- 20230410
wave_no <- 3

basic <- add_wave_data(basic, basic3, date_datapull, wave_no)
move <- add_wave_data(move, move3, date_datapull, wave_no)
assess <- add_wave_data(assess, assess3, date_datapull, wave_no)
house <- add_wave_data(house, house3, date_datapull, wave_no)
program <- add_wave_data(program, program3[,-which(names(program3) %in% c("Grp_Cd", "VantageStatus"))], date_datapull, wave_no)
conduct <- add_wave_data(conduct, conduct3, date_datapull, wave_no)
work <- add_wave_data(work, work3, date_datapull, wave_no)
visit <- add_wave_data(visit, visit3, date_datapull, wave_no)

# Wave 4 ####
# -- Read in Admin Data
# Wave 4 data was pulled slightly differently because SCI Chester changed systems
# Work assignment start and end dates are wrongly coded in wave 4
basic4 <-xl.read.file("data/raw/1_admin_data/pcq_wave4/PrisonClimateSurveyParticipantsData_ALL_CHS_20231107_2.xlsx", xl.sheet=1, password="LS2023")
move4 <-xl.read.file("data/raw/1_admin_data/pcq_wave4/PrisonClimateSurveyParticipantsData_ALL_CHS_20231107_2.xlsx", xl.sheet=2, password="LS2023")
assess4 <-xl.read.file("data/raw/1_admin_data/pcq_wave4/PrisonClimateSurveyParticipantsData_ALL_CHS_20231107_2.xlsx", xl.sheet=3, password="LS2023")
house4 <-xl.read.file("data/raw/1_admin_data/pcq_wave4/PrisonClimateSurveyParticipantsData_ALL_CHS_20231107_2.xlsx", xl.sheet=4, password="LS2023")
program4 <-xl.read.file("data/raw/1_admin_data/pcq_wave4/PrisonClimateSurveyParticipantsData_ALL_CHS_20231107_2.xlsx", xl.sheet=5, password="LS2023")
conduct4 <-xl.read.file("data/raw/1_admin_data/pcq_wave4/PrisonClimateSurveyParticipantsData_ALL_CHS_20231107_2.xlsx", xl.sheet=6, password="LS2023")
work4 <-xl.read.file("data/raw/1_admin_data/pcq_wave4/PrisonClimateSurveyParticipantsData_ALL_CHS_20231107_2.xlsx", xl.sheet=7, password="LS2023")
visit4 <-xl.read.file("data/raw/1_admin_data/pcq_wave4/PrisonClimateSurveyParticipantsData_ALL_CHS_20231107_2.xlsx", xl.sheet=8, password="LS2023")

# -- Ensure Compatibility
basic4 <- basic4 %>%
  rename(STG = stg,
         `ASCA Category - Ranked` = Ranking,
         MHCode = mh_code) %>%
  mutate(min_expir_date = as.character(ifelse(min_expir_date==0,
                                 00000000, min_expir_date)),
         max_expir_date = as.character(ifelse(max_expir_date==0,
                                              00000000, max_expir_date)),
         date_of_birth = as.character(date_of_birth),
         delete_date = as.character(delete_date),
         RecmpMax_Dt = NA)

basic4 <- as.data.frame(basic4)

visit4 <- visit4 %>%
  mutate(VstEvnt_TmOut = as.Date(VstEvnt_TmOut))

visit4 <- as.data.frame(visit4)

# -- Merge in
date_datapull <- 20231107 # Confirm this is the date data was pulled; not the date data was sent
wave_no <- 4

basic <- add_wave_data(basic, basic4, date_datapull, wave_no)
move <- add_wave_data(move, move4, date_datapull, wave_no)
assess <- add_wave_data(assess, assess4, date_datapull, wave_no)
house <- add_wave_data(house, house4, date_datapull, wave_no)
program <- add_wave_data(program, program4, date_datapull, wave_no)
conduct <- add_wave_data(conduct, conduct4, date_datapull, wave_no)
work <- add_wave_data(work[,which(names(work) %ni% c("WrkAsgnmtStrt_Dt", "WrkAsgnmtEnd_Dt"))], work4[,which(names(work4) %ni% c("WrkAsgnmtStrt_Dt", "WrkAsgnmtEnd_Dt"))], date_datapull, wave_no)
visit <- add_wave_data(visit, visit4, date_datapull, wave_no)

# Wave 5 ####
# -- Read in Admin Data
basic5 <-xl.read.file("data/raw/1_admin_data/pcq_wave5/PrisonClimateSurveyParticipantsData_ALL_CHS_20240508.xlsx", xl.sheet=1, password="LS2024")
move5 <-xl.read.file("data/raw/1_admin_data/pcq_wave5/PrisonClimateSurveyParticipantsData_ALL_CHS_20240508.xlsx", xl.sheet=2, password="LS2024")
assess5 <-xl.read.file("data/raw/1_admin_data/pcq_wave5/PrisonClimateSurveyParticipantsData_ALL_CHS_20240508.xlsx", xl.sheet=3, password="LS2024")
house5 <-xl.read.file("data/raw/1_admin_data/pcq_wave5/PrisonClimateSurveyParticipantsData_ALL_CHS_20240508.xlsx", xl.sheet=4, password="LS2024")
program5 <-xl.read.file("data/raw/1_admin_data/pcq_wave5/PrisonClimateSurveyParticipantsData_ALL_CHS_20240508.xlsx", xl.sheet=5, password="LS2024")
conduct5 <-xl.read.file("data/raw/1_admin_data/pcq_wave5/PrisonClimateSurveyParticipantsData_ALL_CHS_20240508.xlsx", xl.sheet=6, password="LS2024")
work5 <-xl.read.file("data/raw/1_admin_data/pcq_wave5/PrisonClimateSurveyParticipantsData_ALL_CHS_20240508.xlsx", xl.sheet=7, password="LS2024")
visit5 <-xl.read.file("data/raw/1_admin_data/pcq_wave5/PrisonClimateSurveyParticipantsData_ALL_CHS_20240508.xlsx", xl.sheet=8, password="LS2024")

# -- Ensure Compatibility
names(assess5)[which(names(assess5)=="Control_Number")] <- "control_number"
names(program5)[which(names(program5)=="completion_description")] <- "CompletionDesc"

basic5 <- basic5 %>%
  rename(STG = stg,
         `ASCA Category - Ranked` = Ranking,
         MHCode = mh_code) %>%
  mutate(min_expir_date = as.character(ifelse(min_expir_date==0,
                                              00000000, min_expir_date)),
         max_expir_date = as.character(ifelse(max_expir_date==0,
                                              00000000, max_expir_date)),
         date_of_birth = as.character(date_of_birth),
         delete_date = as.character(delete_date),
         RecmpMax_Dt = NA)

basic5 <- as.data.frame(basic5)

# -- Merge in
date_datapull <- 20240508
wave_no <- 5

basic <- add_wave_data(basic, basic5, date_datapull, wave_no)
move <- add_wave_data(move, move5, date_datapull, wave_no)
assess <- add_wave_data(assess, assess5, date_datapull, wave_no)
house <- add_wave_data(house, house5, date_datapull, wave_no)
program <- add_wave_data(program, program5, date_datapull, wave_no)
conduct <- add_wave_data(conduct, conduct5, date_datapull, wave_no)
work <- add_wave_data(work[,which(names(work) %ni% c("WrkAsgnmtStrt_Dt", "WrkAsgnmtEnd_Dt"))], work5[,which(names(work5) %ni% c("WrkAsgnmtStrt_Dt", "WrkAsgnmtEnd_Dt"))], date_datapull, wave_no)
visit <- add_wave_data(visit, visit5, date_datapull, wave_no)

# Wave 6 ####
# -- Read in Admin Data
basic6 <-xl.read.file("data/raw/1_admin_data/pcq_wave6/PrisonClimateSurveyParticipantsData_20241104.xlsx", xl.sheet=1, password="LS2024")
move6 <-xl.read.file("data/raw/1_admin_data/pcq_wave6/PrisonClimateSurveyParticipantsData_20241104.xlsx", xl.sheet=2, password="LS2024")
assess6 <-xl.read.file("data/raw/1_admin_data/pcq_wave6/PrisonClimateSurveyParticipantsData_20241104.xlsx", xl.sheet=3, password="LS2024")
house6 <-xl.read.file("data/raw/1_admin_data/pcq_wave6/PrisonClimateSurveyParticipantsData_20241104.xlsx", xl.sheet=4, password="LS2024")
program6 <-xl.read.file("data/raw/1_admin_data/pcq_wave6/PrisonClimateSurveyParticipantsData_20241104.xlsx", xl.sheet=5, password="LS2024")
conduct6 <-xl.read.file("data/raw/1_admin_data/pcq_wave6/PrisonClimateSurveyParticipantsData_20241104.xlsx", xl.sheet=6, password="LS2024")
work6 <-xl.read.file("data/raw/1_admin_data/pcq_wave6/PrisonClimateSurveyParticipantsData_20241104.xlsx", xl.sheet=7, password="LS2024")
visit6 <-xl.read.file("data/raw/1_admin_data/pcq_wave6/PrisonClimateSurveyParticipantsData_20241104.xlsx", xl.sheet=8, password="LS2024") # TO FIX: this includes just 1 row for a date way after the datapull

# -- Ensure Compatibility
names(assess6)[which(names(assess6)=="Control_Number")] <- "control_number"
names(program6)[which(names(program6)=="completion_description")] <- "CompletionDesc"

basic6 <- basic6 %>%
  rename(STG = stg,
         `ASCA Category - Ranked` = Ranking,
         MHCode = mh_code) %>%
  mutate(min_expir_date = as.character(ifelse(min_expir_date==0,
                                              00000000, min_expir_date)),
         max_expir_date = as.character(ifelse(max_expir_date==0,
                                              00000000, max_expir_date)),
         date_of_birth = as.character(date_of_birth),
         delete_date = as.character(delete_date),
         RecmpMax_Dt = NA)

basic6 <- as.data.frame(basic6)

# -- Merge in
date_datapull <- 20241104
wave_no <- 6

basic <- add_wave_data(basic, basic6, date_datapull, wave_no)
move <- add_wave_data(move, move6, date_datapull, wave_no)
assess <- add_wave_data(assess, assess6, date_datapull, wave_no)
house <- add_wave_data(house, house6, date_datapull, wave_no)
program <- add_wave_data(program, program6, date_datapull, wave_no)
conduct <- add_wave_data(conduct, conduct6, date_datapull, wave_no)
work <- add_wave_data(work[,which(names(work) %ni% c("WrkAsgnmtStrt_Dt", "WrkAsgnmtEnd_Dt"))],
                                    work6[,which(names(work6) %ni% c("WrkAsgnmtStrt_Dt", "WrkAsgnmtEnd_Dt"))],
                                    date_datapull, wave_no)
visit <- add_wave_data(visit, visit6, date_datapull, wave_no)

# Standardize ID column name ####
names(basic)[which(names(basic)=="inmate_number")] <- "original_id"
names(move)[which(names(move)=="mov_cur_inmt_num")] <- "original_id"
names(assess)[which(names(assess)=="Inmate_number")] <- "original_id"

# Control numbers ####
# Some datasets only have control numbers and not inmate numbers - house, program, conduct, work, and visit
# Find unique combinations of IDs and control numbers
control_nos <- unique(basic[,c("original_id", "control_number")])

# Sometimes there are two versions of the control number, one with 6 digits and one with 5 digits where the only difference is a leading zero. In those cases only, add a leading zero.
control_nos <- control_nos %>%
  group_by(original_id) %>%
  mutate(
    # Check if there's a 6-digit control number where the last 5 digits match with a 5-digit control number
    has_leading_zero = any(nchar(control_number) == 6 &
                             substr(control_number, 2, 6) == control_number[nchar(control_number) == 5]),

    # Add the leading zero only if there's a match
    control_number = if_else(has_leading_zero & nchar(control_number) == 5,
                             paste0("0", control_number),
                             control_number)
  ) %>%
  ungroup() %>%
  select(-has_leading_zero) %>%
  distinct()

# Some control numbers are associated with multiple IDs (n=54 as of wave 6)
# Associate each control number with the accompanying original inmate IDs
control_nos_wide <- control_nos %>%
  group_by(control_number) %>%
  mutate(research_order = row_number()) %>%
  pivot_wider(
    names_from = research_order,
    values_from = original_id,
    names_prefix = "original_id_"
  ) %>%
  ungroup()

# Mask IDs ####
# Basic
basic <- basic[,-which(names(basic)=="state_id_num")]
basic$original_id <- tolower(basic$original_id)
i <- unique(basic$original_id)
id.link <- mask_ids(i) # Generate masked IDs
basic <- left_join(basic, id.link, by="original_id") # Merge basic on original ID
basic <- basic[,-which(names(basic)=="original_id")] # delete original ID
basic <- basic[,c(ncol(basic),1:ncol(basic)-1)] # reorder columns

# Move
move$original_id <- tolower(move$original_id)
i <- unique(move$original_id)
move <- left_join(move, id.link, by="original_id")
move <- move[,-which(names(move)=="original_id")]
move <- move[,c(ncol(move),1:ncol(move)-1)]

# Assess
assess$original_id <- tolower(assess$original_id)
i <- unique(assess$original_id)
i <- tolower(unique(assess$original_id))
id.link <- mask_ids(i)
assess <- left_join(assess, id.link, by="original_id")
assess <- assess[,-which(names(assess)=="original_id")]
assess <- assess[,c(ncol(assess),1:ncol(assess)-1)]

# Control Numbers ####




# Retain the first research_id associated with each control_number and drop the rest. This is an issue to prioritize fixing asap. Reserve half a day! ####
# See control number function I started working on above. But have to figure out how this works across datasets (basic, house, etc.)

control_nos <- control_nos %>%
  group_by(control_number) %>%
  slice(1) %>%  # Retain the first occurrence of each control number
  ungroup()

stopifnot(length(unique(control_nos$research_id))==length(unique(control_nos$control_number)))

# -- house ####
house <- left_join(house, control_nos)
house <- house[,c(ncol(house),1:ncol(house)-1)]
house <- house[,-which(names(house)=="control_number")]

# -- program ####
program <- left_join(program, control_nos)
program <- program[,c(ncol(program),1:ncol(program)-1)]
program <- program[,-which(names(program)=="control_number")]

# -- conduct ####
conduct <- left_join(conduct, control_nos)
conduct <- conduct[,c(ncol(conduct),1:ncol(conduct)-1)]
conduct <- conduct[,-which(names(conduct)=="control_number")]

# -- work ####
work <- left_join(work, control_nos)
work <- work[,c(ncol(work),1:ncol(work)-1)]
work <- work[,-which(names(work)=="control_number")]

# -- visit ####
visit <- left_join(visit, control_nos)
visit <- visit[,c(ncol(visit),1:ncol(visit)-1)]
visit <- visit[,-which(names(visit)=="control_number")]

# Delete control number from the basic, assess and move dataframes, too
basic <- basic[,-which(names(basic)=="control_number")]
assess <- assess[,-which(names(assess)=="control_number")]
move <- move[,-which(names(move)=="control_number")]

# Save as dataframes
basic <- as.data.frame(basic)
move <- as.data.frame(move)
assess <- as.data.frame(assess)
house <- as.data.frame(house)
conduct <- as.data.frame(conduct)
work <- as.data.frame(work)
visit <- as.data.frame(visit)


# Save files ####
saveRDS(basic, file="data/processed/processing_layer_1/basic.Rds")
saveRDS(move, file="data/processed/processing_layer_1/move.Rds")
saveRDS(assess, file="data/processed/processing_layer_1/assess.Rds")
saveRDS(house, file="data/processed/processing_layer_1/house.Rds")
saveRDS(program, file="data/processed/processing_layer_1/program.Rds")
saveRDS(conduct, file="data/processed/processing_layer_1/conduct.Rds")
saveRDS(work, file="data/processed/processing_layer_1/work.Rds")
saveRDS(visit, file="data/processed/processing_layer_1/visit.Rds")
