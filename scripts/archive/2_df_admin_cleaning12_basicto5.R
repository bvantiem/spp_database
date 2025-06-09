# Notes ####
# Several parts of this script should be updated with every new wave.

# ---------- Data Set Up ---------- ####
rm(list=ls())
## Libraries ####
library(lubridate)
source("scripts/0_utils.R")

## Data ####
basic <- readRDS("data/processed/processing_layer_1/basic.Rds")
house <- readRDS("data/processed/processing_layer_1/house.Rds")
conduct <- readRDS("data/processed/processing_layer_1/conduct.Rds")
assess <- readRDS("data/processed/processing_layer_1/assess.Rds")
work <- readRDS("data/processed/processing_layer_1/work.Rds")
lifers <- readRDS("data/processed/processing_layer_1/230224_lifer_admit_dates.Rds")
randassign <- readRDS("data/processed/randassign.Rds")


# Link basic and treatment status
basic <- left_join(basic, randassign, by="research_id")


# ---------- DF BASIC SAVED W WAVE 5 ---------- ####
## Data Cleaning  ####
### Renaming
names(basic)[which(names(basic)=="ASCA Category - Ranked")] <- "asca"
names(basic)[which(names(basic)=="class_of_sent")] <- "sentence_class_code"
names(basic)[which(names(basic)=="RecmpMax_Dt")] <- "recomp_max_date"
names(basic)[which(names(basic)=="MHCode")] <- "mh_code"

### Factoring only
var.names <- c("temp_custody", "location_permanent", "sentence_status", "commit_cnty", "offense_code",  "mh_code")
for(i in var.names){
  basic[,i] <- as.factor(basic[,i])
}

### Factoring + levels
basic$race_code <- factor(basic$race_code, levels=c("W", "B", "A"))
basic$marital_status_code <- factor(basic$marital_status_code, levels=c("SIN", "MAR", "SEP", "DIV", "WID", "UNK")) # not clear on diff unk and NA
basic$asca <- factor(basic$asca, levels=c("1-Violent","2-Property","3-Drugs","4-Public Order","9-Not An Arrest"))
basic$custody <- factor(basic$custody,levels=c("L2","L3", "L4","L5", "L1")) # Very few ppl in L1
basic$sentence_class_code <- factor(basic$sentence_class_code, levels=c("IN", "LF", "DF", "CL")) # Very few ppl in CL

#### Numeric
var.names <- c("STG","grade_complete",
               "min_cort_sent_yrs","min_cort_sent_mths" ,"min_cort_sent_days",
               "max_cort_sent_yrs","max_cort_sent_mths" ,"max_cort_sent_days")

for(i in var.names){
  basic[,i] <- as.numeric(basic[,i])
}

#### Dates
basic$min_expir_date <- as.numeric(basic$min_expir_date)
basic$min_expir_date <- with(basic, ifelse(min_expir_date %in% c(00000000,0), 99990101, min_expir_date))
basic$min_expir_date <- ymd(basic$min_expir_date)

basic$max_expir_date <- as.numeric(basic$max_expir_date)
basic$max_expir_date <- with(basic, ifelse(max_expir_date==00000000, 99990101, max_expir_date))
basic$max_expir_date <- ymd(basic$max_expir_date)

basic$recomp_max_date <- ymd(basic$recomp_max_date)

basic$date_of_birth <- ymd(basic$date_of_birth)

## New Variables ####

basic$location_permanent_chs <- ifelse(basic$location_permanent=="CHS",1,0)
basic$year_of_birth <- year(basic$date_of_birth)
basic$high_school <- ifelse(basic$grade_complete>=12, 1,0)
basic$age_wave1 <- decimal_date(ymd(20220501))-decimal_date(basic$date_of_birth)
basic$age_wave2 <- decimal_date(ymd(20221115))-decimal_date(basic$date_of_birth)
basic$age_wave3 <- decimal_date(ymd(20230520))-decimal_date(basic$date_of_birth)
basic$age_wave4 <- decimal_date(ymd(20231128))-decimal_date(basic$date_of_birth)
basic$age_wave5 <- decimal_date(ymd(20240606))-decimal_date(basic$date_of_birth)
basic$age_at_treatment <- with(basic, ifelse(treatment_wave==1, age_wave1,
                                             ifelse(treatment_wave==2, age_wave2,
                                                    ifelse(treatment_wave==3, age_wave3,
                                                           ifelse(treatment_wave==4, age_wave4,
                                                                  ifelse(treatment_wave==5, age_wave5,NA))))))

basic$days_to_min_wave1 <- ymd(basic$min_expir_date)-ymd(20220501) # Update with every wave
basic$days_to_min_wave2 <- ymd(basic$min_expir_date)-ymd(20221115)
basic$days_to_min_wave3 <- ymd(basic$min_expir_date)-ymd(20230520)
basic$days_to_min_wave4 <- ymd(basic$min_expir_date)-ymd(20231128)
basic$days_to_min_wave5 <- ymd(basic$min_expir_date)-ymd(20240606)

basic$days_to_min_at_treatment <-   with(basic, ifelse(treatment_wave==1, days_to_min_wave1,
                                                       ifelse(treatment_wave==2, days_to_min_wave2,
                                                              ifelse(treatment_wave==3, days_to_min_wave3,
                                                                     ifelse(treatment_wave==4, days_to_min_wave4,
                                                                            ifelse(treatment_wave==5, days_to_min_wave5,NA))))))
basic$min_sent_days <- with(basic, (min_cort_sent_yrs*365)+(min_cort_sent_mths*31)+(min_cort_sent_days))
basic$max_sent_days <- with(basic, (max_cort_sent_yrs*365)+(max_cort_sent_mths*31)+(max_cort_sent_days))

basic$life <- ifelse(basic$sentence_class %in% c("LIFE"),1,0)

# Calculate estimated admit dates & estimated time served
basic$est_admit_date <- basic$max_expir_date - 2*(days(basic$max_expir_date-basic$min_expir_date)) # estimate!

# Link with admit dates for lifers
basic <- left_join(basic, lifers[,c("admit_date", "research_id")])
basic[which(basic$research_id %in% lifers$research_id),"est_admit_date"] <- basic[which(basic$research_id %in% lifers$research_id),"admit_date"]
basic <- basic[,-which(names(basic)=="admit_date")]

i <- which(basic$sentence_class=="DEFINITE/FLAT")
basic$est_admit_date[i] <- basic$min_expir_date[i]-basic$min_sent_days[1]
i <- which(basic$sentence_class=="INDETERMINATE" & (basic$min_expir_date==basic$max_expir_date))
basic$est_admit_date[i] <- basic$max_expir_date[i]-basic$max_sent_days[i]
i <- which(basic$research_id=="rid_am3704")
basic$est_admit_date[i] <- ymd(20220430) # Manual override. Formula not 100% adequate

# DATA REQUEST: Information still missing for 20 individuals
i <- which(basic$sentence_class=="INDETERMINATE" & (basic$min_expir_date!="9999-01-01"
                                                    & basic$max_expir_date=="9999-01-01"))
basic$est_admit_date[i] <- NA
i <- which(basic$sentence_class=="DETENTION" & (basic$min_expir_date=="9999-01-01"
                                                    & basic$max_expir_date=="9999-01-01"))
basic$est_admit_date[i] <- NA
i <- which(is.na(basic$sentence_class) & (basic$min_expir_date=="9999-01-01"
                                                & basic$max_expir_date=="9999-01-01"))
basic$est_admit_date[i] <- NA

basic$est_days_served_on_20220501 <- ymd(20220501)-basic$est_admit_date # Update with every wave
basic$est_days_served_on_20220501 <- as.numeric(gsub(" days", "", basic$est_days_served_on_20220501))

# Static characteristics
basic$custody_recode <- basic$custody
basic[which(!is.na(basic$temp_custody)),"custody_recode"] <- basic[which(!is.na(basic$temp_custody)),]$temp_custody
basic$race_white <- ifelse(basic$race=="WHITE",1,0)
basic$race_black <- ifelse(basic$race=="BLACK",1,0)
basic$violent_offense <- ifelse(basic$asca=="1-Violent",1,0)
basic$property_offense <- ifelse(basic$asca=="2-Property",1,0)
basic$drugs_offense <- ifelse(basic$asca=="3-Drugs",1,0)
basic$publicorder_offense <- ifelse(basic$asca=="4-Public Order",1,0)
basic$notanarrest_offense <- ifelse(basic$asca=="9-Not An Arrest",1,0)

## Unresolved Issues ####
# delete date not in unambiguous format. Most are 9999999. All dates that are not 9999999 miss one number (e.g. 2022167)

## Save ####
saveRDS(basic, file="data/processed/basic.Rds")

# ---------- DF HOUSE SAVED W WAVE 2 ---------- ####
# House variables ####
house$date_in_recode <- parse_date_time(house$date_in, orders = c("mdy", "dmy", "ymd"))
house$date_out_recode <- parse_date_time(house$date_out, orders = c("mdy", "dmy", "ymd"))
house$date_in_recode <- ymd(house$date_in_recode)
# Resolve issue of dates that do fail to parse! ####
house$date_out_recode <- ymd(house$date_out_recode)
house[which(house$date_out=="00000000"),"date_out_recode"] <- ymd(99990101)
house <- house[which(house$facility=="CHS"),]
house <- house[which(house$date_in!=house$date_out),] #assuming these are errors
ids <- unique(house$research_id)
house$location_at_pcqwave1 <- "unknown"

# Note to self - check the below code. Still valid when adding more waves in? ####
for(i in ids){
  # Issues - there are gaps between moves, so some people are nowhere on April 1st. These people returned an error.
  # The below does not provide an answer for those (7) people
  house <- data.frame(house)
  temp <- ymd(20220501)>=house[house$research_id==i,"date_in_recode"] & ymd(20220501)<house[house$research_id==i,"date_out_recode"]
  proceed <- length(temp[which(temp==FALSE)])<length(temp)

  if(proceed==TRUE){ # not the case for people who are nowhere on April first
    k <- house[house$research_id==i,"date_in_recode"][which(ymd(20220501)>=house[house$research_id==i,"date_in_recode"] & ymd(20220501)<house[house$research_id==i,"date_out_recode"])]
    house[house$research_id==i,"location_at_pcqwave1"] <- ifelse(house[house$research_id==i,]$date_in_recode==k,1,0)
  }
}
# unique(house[house$location_at_pcqwave1=="unknown",]$research_id) # Ids of people who are nowhere

# Link housing units with unit type
house$unit <- tolower(paste0(house$building, house$section))
house <- left_join(house, unit_mapping, by="unit")


# Save ####
# saveRDS(house, file="data/processed/house.Rds")

# ---------- DF CONDUCT SAVED W WAVE 2 ---------- ####
# Data Cleaning ####
conduct$misconduct_date <- ymd(conduct$misconduct_date)

# New Variables ####
conduct$mcdt_g_1m <- as.numeric(conduct$misconduct_date <= ymd(20220501) &
                     conduct$misconduct_date >= ymd(20220401) &
                       conduct$vrdict_guilty!="000")
conduct$mcdt_g_6m <- as.numeric(conduct$misconduct_date <= ymd(20220501) &
                                      conduct$misconduct_date >= ymd(20211201) &
                                     conduct$vrdict_guilty!="000")
# Save ####
# saveRDS(conduct, file="data/processed/conduct.Rds")

# ---------- DF ASSESS SAVED W WAVE 2 ---------- ####
# Notes
# Data relates to multiple intakes - see ID number 10404 for example
# HIQ tests until there until 2010, so left out.
assess$test_date <- gsub("(.*) .*", "\\1", assess$Test_Dt)
assess$test_date <- ymd(assess$test_date)
assess <- assess[,-which(names(assess)=="Test_Dt")]
assess <- unique(assess) # Remove the many duplicate entries
# Identify the date of the last test taken, as well as the last date for each individual type of test ####
# Note that in some cases, two scores are reported for the same test within a few days ####
assess$last_date <- ymd(29991212)
assess$gap <- NA
assess$last_test <- NA
assess$no_tests <- NA
for(i in unique(assess$research_id)){
  index <- which(assess$research_id==i)
  assess[index,"last_date"] <- max(assess[index,]$test_date)
  assess[index,"gap"] <- with(assess[index,], test_date-last_date)
  for(j in c("CSS-M", "RST", "TCU", "LSI-R")){
    index2 <- with(assess, which(research_id==i & Test_Desc==j))
    k <- max(assess[index2,"test_date"], na.rm=TRUE)
    assess[index2,"last_test"] <- ifelse(assess[index2,"test_date"]==k,1,0)
  }
  assess[index,"no_tests"] <- sum(assess[index,]$last_test)
}

# Three individuals have two REST tests on the same day
duplicate_rst <- unique(assess[which(assess$no_tests==5),]$research_id)
for(i in duplicate_rst){
  index <- which(assess$research_id==i & assess$Test_Desc=="RST")
  k <- sample(index)[1]
  assess <- assess[-k,]
}
assess[assess$research_id %in% duplicate_rst,]$no_tests <- 4

# In some cases last test as long as 18 years away, so subset to last test in the last five years
# range(assess[assess$last_test==1,]$gap, na.rm=TRUE)/365
# Most people who have just 3 tests have the RSU, RST, and CSS-M. What to do with this data?
