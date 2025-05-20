# ---------- Data set-up ---------- ---- 
rm(list=ls())
rm(list=ls())
library(readxl)
library(dplyr)
library(lubridate)
library("excel.link")
source("scripts/0_id_masking_function.R")
`%ni%` = Negate(`%in%`)

# Admin Data wave 1 
basic <- read_xlsx("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsData_20220625.xlsx", sheet=1)
basic_cor <- xl.read.file("data/raw/1_admin_data/pcq_wave1/PrisonClimateSurveyParticipantsDataCorrectedIDs_20220903.xlsx", xl.sheet=1, password="LS_2022")
basic <- rbind(basic, basic_cor)
basic2 <- xl.read.file("data/raw/1_admin_data/pcq_wave2/PrisonClimateSurveyCHSData_20220912.xlsx", xl.sheet=1, password="LS2022")
basic <- rbind(basic, basic2)

i <- data.frame(inmate_id = unique(basic[basic$sentence_class %in% c("LIFE", "COMMUTED LIFE"),"inmate_number"]))
write.csv(i, file = "data/processed/dedicated_analyses/lifer_ids_for_admit_date.csv")
