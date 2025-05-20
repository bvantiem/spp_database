# ---------- Data set-up ---------- ---- 
rm(list=ls())

## Set Seed ----
set.seed(1962)

## Libraries ----
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse)
library(naniar)
library(matrixStats) # for RowSds
library(ltm) # For chronbach's alpha
library(xtable)
library(psychTools)
library(psych) # For factor analysis
library(corrplot) # For factor analysis
library(ggplot2)
library(car) # For factor analysis
library(GPArotation) # For factor analysis
library(splithalf) # # https://cran.r-project.org/web/packages/splithalf/readme/README.html, Alternatives to explore: splithalfr, psych
library(tidyr) # Needed for splithalf/spearman brown analysis 
library(REdaS)
library(estimatr)
library(texreg)

# Source ####
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")

# Notes to self ####
# Example code from splithalf
# https://cran.r-project.org/web/packages/splithalf/readme/README.html
`%ni%` = Negate(`%in%`)

## Load data ----
load("data/processed/pcq.Rda")
load("data/processed/pcq2.Rda")
load("data/processed/pcq_am.Rda")
load("data/processed/basic.Rda")
load("data/processed/house.Rda")

# Subset Data ####
# Subset data to analysis based on wave 1 alone
pcq <- pcq[which(pcq$survey_wave==1),]
pcq2 <- pcq2[which(pcq2$survey_wave==1),]
pcq_am <- pcq_am[which(pcq_am$survey_wave==1),]

# Because this has just 1 individual in there, generates issues with regressions 
pcq <- pcq[pcq$unit_type!="inf",]
pcq2 <- pcq2[pcq2$unit_type!="inf",]
pcq_am <- pcq_am[pcq_am$unit_type!="inf",]
pcq$unit_type <- droplevels(pcq$unit_type) # drop the now empty level for the infirmary
pcq2$unit_type <- droplevels(pcq2$unit_type)

pcq_am$unit_type <- droplevels(pcq_am$unit_type)

# Delete individuals who completed the survey for a second time within wave one.
pcq$survey_no <- 1
pcq2$survey_no <- 1
pcq_am$survey_no <- 1

temp <- data.frame(table(pcq$research_id))
index <- temp[temp$Freq==2,"Var1"]
pcq[pcq$research_id %in% index & pcq$block!="a","survey_no"] <- 2 # all these individuals moved from an a block to another unit, where they completed a second survey
pcq2[pcq2$research_id %in% index & pcq2$block!="a","survey_no"] <- 2
pcq_am[pcq_am$research_id %in% index & pcq_am$block!="a","survey_no"] <- 2

pcq <- pcq[pcq$survey_no==1,] # Drops the second survey of these 3 individuals.
pcq2 <- pcq2[pcq2$survey_no==1,]
pcq_am <- pcq_am[pcq_am$survey_no==1,]

# Save in case needed 
pcq_full <- pcq
pcq2_full <- pcq2
pcq_am_full <- pcq_am

# Retain only questions needed for psychometrics analysis
retain <- pcq_lookup[which(pcq_lookup$include_comparative_psych_analysis=="yes"),]$question_qno
retain.other <- c("research_id", "date", "unit_type", 
                  "maq_prisoners", "maq_staff", "maq_care", "maq_autonomy", "maq_procedure",
                  "maq_safety","maq_actsat","maq_actav","maq_reint","maq_sleep","maq_shop",
                  "maq_overall","maq_severity","maq_contact","maq_visits",
                  "mip_prisoners", "mip_staff", "mip_care", "mip_autonomy", "mip_procedure",
                  "mip_safety","mip_actsat","mip_actav","mip_reint","mip_sleep","mip_shop",
                  "mip_overall","mip_severity","mip_contact","mip_visits",
                  "children", "cell", "foreign_born", "partner")
pcq <- pcq[,c(retain, retain.other)]
pcq2 <- pcq2[,c(retain, retain.other)]

retain.other <- c("research_id", "date", "unit_type", 
                  "mam_prisoners", "mam_staff", "mam_care", "mam_autonomy", "mam_procedure",
                  "mam_safety","mam_actsat","mam_actav","mam_reint","mam_sleep","mam_shop",
                  "mam_overall","mam_severity","mam_contact","mam_visits")
pcq_am <- pcq_am[,c(retain, retain.other)]

# Variables to add ####
pcq_lookup$scale_id <- NA # Code generalizability - just keep it!

pcq[which(is.na(pcq$research_id)), "research_id"] <- paste0("rid_na", c(1:length(pcq[which(is.na(pcq$research_id)), "research_id"])))
pcq2[which(is.na(pcq2$research_id)), "research_id"] <- paste0("rid_na", c(1:length(pcq2[which(is.na(pcq2$research_id)), "research_id"])))
pcq_am[which(is.na(pcq_am$research_id)), "research_id"] <- paste0("rid_na", c(1:length(pcq_am[which(is.na(pcq_am$research_id)), "research_id"])))

## Handy code ####
# pcq_lookup[grep("safe", pcq_lookup$question),c("question_no","question")]
# pcq_lookup[grep("TCU", pcq_lookup$answer_scale),c("question_no","answer_scale")]

# ISSUES TO RESOLVE IN THIS SCRIPT ####
# Now generate basic_temp several times. Better to create it once - with a population column variable indicating whether someone is a respondent.
# Individual figures quoted in the paper #####
# length(which(complete.cases(pcq2[,1:64]))) # number of individuals with complete cases 
# lapply(pcq, function(x) length(which(x %in% c(111,99)))) # number of No Opinion responses 
table(pcq_lookup[pcq_lookup$question_qno %in% names(pcq),"answer_scale_pa"])


# ---------- Population and Sample Characteristics ---------- ####
# Functions for descriptives table ####
# Function to calculate n, range, mean and sd
rownames.table <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Time Served*",
                    "Time Served (inc lifers)",
                    "Life",
                    "Age",
                    "Education: High School or Higher",
                    "White",
                    "Black",
                    "Violent",
                    "Property",
                    "Drugs",
                    "Public Order",
                    "Sex",
                    "Children",
                    "Double Cell",
                    "Country of Birth",
                    "Has Partner",
                    "General Population",
                    "Therapeutic Community",
                    "Transitional Housing Unit",
                    "Honor Block",
                    "Little Scandinavia",
                    "Restricted Housing Unit",
                    "Recovery Unit")
f.descriptives <- function(data){
  lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}
f.tab <- function(basic_temp, house_temp, pcq_temp){
  # Calculate characteristics for selected variables 
  vars <- c("min_sent_days", "max_sent_days", "est_days_served_on_20220501")
  data <- basic_temp[basic_temp$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
  data$min_sent_days <- data$min_sent_days/365
  data$max_sent_days <- data$max_sent_days/365
  data$est_days_served_on_20220501 <- data$est_days_served_on_20220501/365
  tab <- t(as.data.frame(f.descriptives(data)))
  
  vars <- c("est_days_served_on_20220501","life","age_on_20220501","high_school", "race_white", "race_black")
  data <- basic_temp[vars]
  data$est_days_served_on_20220501 <- data$est_days_served_on_20220501/365
  names(data)[which(names(data)=="est_days_served_on_20220501")] <- "est_days_served_on_20220501_inc_lifer"
  tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
  
  vars <- c("violent_offense", "property_offense", "drugs_offense", "publicorder_offense", "sex_offense")
  i <- grep("sex|rape", basic_temp$offense, ignore.case=T)
  basic_temp$violent_offense[i] <- 0
  basic_temp$property_offense[i] <- 0
  basic_temp$drugs_offense[i] <- 0
  basic_temp$publicorder_offense[i] <- 0
  basic_temp$sex_offense <- 0
  basic_temp$sex_offense[i] <- 1
  data <- basic_temp[vars]
  tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
  
  vars <- c("children", "cell", "foreign_born", "partner")
  data <- pcq_temp[vars]
  tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
  
  vars <- c("unit_type_gp", "unit_type_gp-tc", "unit_type_thu", "unit_type_hons", "unit_type_ls", "unit_type_rhu", "unit_type_rec")
  data <- fastDummies::dummy_cols(data.frame(unit_type = house_temp$unit_type))
  data <- data[,vars]
  tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
  tab <- as.data.frame(tab)
  
  # Build table 
  tab <- as.data.frame(tab)
  names(tab) <- c("n","min", "max","mean","SD")
  row.names(tab) <- rownames.table
  tab$range <- paste0("(",round(tab$min,2), ",", round(tab$max,2), ")")
  tab <- tab %>% dplyr::select(-min, -max)
  
  if(population.exclude == TRUE){
    i <- which(row.names(tab) %in% c("Children", "Double Cell", "Country of Birth", "Has Partner"))
    tab[i,] <- NA
  }
  tab <- tab[,c("n","range","mean", "SD")]
  return(tab)
}
# Population characteristics ####
# Data for population
basic_temp <- basic[which(basic$date_datapull %in% c(ymd(20220625), ymd(20220903))),]

house_temp <- house[which(house$date_datapull %in% c(ymd(20220625), ymd(20220903)) &
                            house$location_at_pcqwave1==1),]
pcq_temp <- pcq_full[which(pcq_full$research_id %in% unique(basic_temp$research_id) &
                             pcq_full$survey_wave==1),]
population.exclude <- TRUE # If set to True, excludes variables for which we only have data for survey respondents 

# Deduplicate for individuals for whom I have data from both datapulls 
basic_temp$pull_no <- 1
temp <- data.frame(table(basic_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
basic_temp[basic_temp$research_id %in% index & basic_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
basic_temp <- basic_temp[which(basic_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 

house_temp$pull_no <- 1
temp <- data.frame(table(house_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
house_temp[house_temp$research_id %in% index & house_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
house_temp <- house_temp[which(house_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 

tab <- f.tab(basic_temp, house_temp, pcq_temp)
tab1 <- tab # save for combined table later


print(xtable(tab, digits=c(0,0,0,2,2), caption="Surveyed Population Characteristics"),include.rownames=TRUE,
      file="output/tables/characteristics_surveyed_population.txt") 

# Survey respondents characteristics ####
# Data
basic_temp <- basic[which(basic$research_id %in% unique(pcq$research_id) &
                            basic$date_datapull %in% c(ymd(20220625), ymd(20220903))),]

house_temp <- house[which(house$research_id %in% unique(pcq$research_id) &
                            house$date_datapull %in% c(ymd(20220625), ymd(20220903)) &
                            house$location_at_pcqwave1==1),]

population.exclude <- FALSE # If set to True, excludes variables for which we only have data for survey respondents

# Deduplicate for individuals for whom I have data from both datapulls 
basic_temp$pull_no <- 1
temp <- data.frame(table(basic_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
basic_temp[basic_temp$research_id %in% index & basic_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
basic_temp <- basic_temp[which(basic_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 

house_temp$pull_no <- 1
temp <- data.frame(table(house_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
house_temp[house_temp$research_id %in% index & house_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
house_temp <- house_temp[which(house_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 


tab <- f.tab(basic_temp, house_temp, pcq_temp)
tab2 <- tab # save for combined table later

print(xtable(tab, digits=c(0,0,0,2,2), caption="Survey Respondent Characteristics"),include.rownames=TRUE,
      file="output/tables/characteristics_survey_respondents.txt") 

# Pa pop and respondents 
tab <- cbind(tab1$mean, tab1$n, tab2$mean, tab2$n)
tab <- as.data.frame(tab)
names(tab) <- c("pa_population", "pa_n_total", "pa_respondent", "pa_n_respondent")
row.names(tab) <- rownames.table
print(xtable(tab, digits=c(0,2,0,2,0), caption="Characteristics of surveyed population & respondents"),include.rownames=TRUE,
      file="output/tables/characteristics_population_and_respondents.txt") 


# NL/PA Sample Descriptives Table ####
tab$variable <- rownames(tab)
vars <- c("Age",
          "Male",
          "Education: High School or Higher",
          "Children",
          "Has Partner",
          "White",
          "Black",
          "Country of Birth",
          "Violent",
          "Property",
          "Drugs",
          "Sex",
          "Public Order",
          "Other",
          "Time Served*",
          "Time Served (inc lifers)",
          "Double Cell",
          "Pre-trial detention",
          "Prison", 
          "Minimum Security",
          "Extra care",
          "Police detainees",
          "Persistent offenders")
nl_tab1 <- data.frame(variable = vars, 
                      nl_n_respondent = rep(NA, length(vars)),
                      nl_n_nonrespondent = rep(NA, length(vars)),
                      nl_respondent = rep(NA, length(vars)),
                      nl_nonrespondent = rep(NA, length(vars)),
                      nl_n_total = rep(NA, length(vars)),
                      nl_population = rep(NA, length(vars)))
nl_tab1 <- left_join(nl_tab1, tab)
nl_tab1$nl_n_respondent <- c(4538,
                        4288+246,
                        2297+1272+529,
                        1746+2574,
                        2492+1752,
                        NA,
                        NA,
                        2821+312+188+154+94+79+674,
                        1636,
                        1189,
                        715,
                        183,
                        NA,
                        219,
                        4536,
                        NA,
                        3353+910,
                        1728,
                        1605,
                        217,
                        274,
                        492,
                        220)

nl_tab1$nl_n_nonrespondent <- c(2284,
                             2179+104,
                             NA,
                             NA,
                             NA,
                             NA,
                             NA,
                             1177+138+113+113+51+90+521,
                             762,
                             655,
                             263,
                             73,
                             NA,
                             107,
                             2247,
                             NA,
                             NA,
                             714,
                             940,
                             128,
                             114,
                             227,
                             112)

nl_tab1$nl_respondent <- c(38.84,
                           .946,
                           .31+.129,
                           .596,
                           .587,
                           NA,
                           NA,
                           .653,
                           .415,
                           .302,
                           .181,
                           .046,
                           NA,
                           .056,
                           11.91,
                           NA,
                           .213,
                           .381,
                           .354,
                           .048,
                           .060,
                           .108,
                           .049)

nl_tab1$nl_nonrespondent <- c(36.59,
                           .954,
                           NA,
                           NA,
                           NA,
                           NA,
                           NA,
                           .534,
                           .41,
                           .352,
                           .141,
                           .039,
                           NA,
                           .058,
                           12.11,
                           NA,
                           NA,
                           .318,
                           .419,
                           .057,
                           .051,
                           .101,
                           .054)

nl_tab1$nl_n_total <- with(nl_tab1, nl_n_nonrespondent+nl_n_respondent)
nl_tab1$nl_population <- with(nl_tab1, ((nl_n_respondent*nl_respondent+nl_n_nonrespondent*nl_nonrespondent)/(nl_n_total)))
nl_tab1[nl_tab1$variable %in% c("Male"),c("pa_n_total", "pa_population","pa_n_respondent","pa_respondent")] <- c(max(nl_tab1$pa_n_total, na.rm=T),1,max(nl_tab1$pa_n_respondent,na.rm=T),1)
nl_tab1[nl_tab1$variable %in% c("Prison"),c("pa_n_total", "pa_population","pa_n_respondent","pa_respondent")] <- c(max(nl_tab1$pa_n_total, na.rm=T),1,max(nl_tab1$pa_n_respondent,na.rm=T),1)
nl_tab1[nl_tab1$variable %in% c("Time Served*"),c("nl_population","nl_respondent")] <- nl_tab1[nl_tab1$variable %in% c("Time Served*"),c("nl_population","nl_respondent")]/12
nl_tab1 <- nl_tab1[,c("variable", "nl_n_respondent", "nl_respondent", "nl_n_total", "nl_population",
                      "pa_n_respondent", "pa_respondent", "pa_n_total", "pa_population")]
nl_tab1[,c("nl_respondent", "nl_population", "pa_respondent", "pa_population")] <- round(nl_tab1[,c("nl_respondent", "nl_population", "pa_respondent", "pa_population")],2)

nl_tab1a <- nl_tab1
names(nl_tab1a) <- c("Variable",
                    rep(c("n", "Respondents","n","Population"),2))
print(xtable(nl_tab1, digits=c(0,0,0,2,0,2,0,2,0,2), caption="Population and Survey Respondent Characteristics - NL and PA"),include.rownames=FALSE, file="output/tables/characteristics_nl_pa.txt")

nl_tab1b <- nl_tab1
nl_tab1b <- nl_tab1b[,c("variable", "pa_n_respondent", "pa_respondent", "nl_n_respondent", "nl_respondent")]
names(nl_tab1b) <- c("Variable",
                     rep(c("n", "Mean"),2))
print(xtable(nl_tab1b, digits=c(0,0,0,2,0,2), caption="Survey Respondent Characteristics - NL and PA"),include.rownames=FALSE, file="output/tables/characteristics_nl_pa_respondents.txt")

nl_tab1c <- nl_tab1
nl_tab1c <- nl_tab1c[,c("variable", "pa_n_total", "pa_population", "nl_n_total", "nl_population")]
names(nl_tab1c) <- c("Variable",
                     rep(c("n", "Mean"),2))
print(xtable(nl_tab1c, digits=c(0,0,0,2,0,2), caption="Population Characteristics - NL and PA"),include.rownames=FALSE, file="output/tables/characteristics_nl_pa_population.txt")





# Compare PA respondent means with PA population means - Statistical tests ####
# Data for population
basic_temp <- basic
basic_temp$population_wave1 <- as.numeric(basic$date_datapull %in% c(ymd(20220625), ymd(20220903)))
basic_temp$respondent_wave1 <- as.numeric(basic$date_datapull %in% c(ymd(20220625), ymd(20220903)) &
                                            basic$research_id %in% unique(pcq$research_id))
basic_temp <- basic_temp[basic_temp$population_wave1==1,]

# Override offense categories so that they more closely match the Dutch categorization
i <- grep("sex|rape", basic_temp$offense, ignore.case=T)
basic_temp$violent_offense[i] <- 0
basic_temp$property_offense[i] <- 0
basic_temp$drugs_offense[i] <- 0
basic_temp$publicorder_offense[i] <- 0
basic_temp$sex_offense <- 0
basic_temp$sex_offense[i] <- 1
basic_temp$offense_type <- with(basic_temp, ifelse(violent_offense==1,"violent",
                                                   ifelse(property_offense==1, "property",
                                                          ifelse(drugs_offense==1, "drugs",
                                                                 ifelse(publicorder_offense==1, "publicorder",
                                                                        ifelse(sex_offense==1, "sex", "other"))))))

# Deduplicate for individuals for whom I have data from both datapulls 
basic_temp$pull_no <- 1
temp <- data.frame(table(basic_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
basic_temp[basic_temp$research_id %in% index & basic_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
basic_temp <- basic_temp[which(basic_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 

with(basic_temp, table(respondent_wave1, population_wave1))

data <- basic_temp[which(basic_temp$population_wave1==1),]
summary(lm(age_on_20220501 ~ respondent_wave1, data=data))$coefficients
summary(lm(high_school ~ respondent_wave1, data=data))$coefficients
summary(lm(race_white ~ respondent_wave1, data=data))$coefficients
summary(lm(race_black ~ respondent_wave1, data=data))$coefficients
summary(lm(sex_offense ~ respondent_wave1, data=data))$coefficients
summary(lm(publicorder_offense ~ respondent_wave1, data=data))$coefficients
summary(lm(drugs_offense ~ respondent_wave1, data=data))$coefficients
summary(lm(property_offense ~ respondent_wave1, data=data))$coefficients
summary(lm(violent_offense ~ respondent_wave1, data=data))$coefficients
summary(lm(est_days_served_on_20220501 ~ respondent_wave1, data=data))$coefficients

# ---------- PsychoMetric Descriptives by Scale ---------- ####
## Internal consistency table ####
# CB - all answered, spearman brown, complete cases 
want.to.rerun.internal.consistency <- FALSE
if(want.to.rerun.internal.consistency == TRUE){
scales.list <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain & 
                                   pcq_lookup$scale_theory_long %ni% c('Overall Question', "Subjective Severity of Imprisonment"),c("scale_theory", "pc_theory")])
keep <- data.frame(mean = rep("boo", nrow(scales.list)),
                   sd = rep("boo", nrow(scales.list)),
                   scale = rep("boo", nrow(scales.list)),
                   pc = rep("boo", nrow(scales.list)),
                   cr.alpha = rep("boo", nrow(scales.list)),
                   sb.stat = rep("boo", nrow(scales.list)),
                   sb_low = rep("boo", nrow(scales.list)),
                   sb_high = rep("boo", nrow(scales.list)),
                   items = rep("boo", nrow(scales.list)),
                   complete_responses = rep("boo", nrow(scales.list)),
                   complete_responses_pcq2 = rep("boo", nrow(scales.list)))

for(i in scales.list$scale_theory){
  for(k in scales.list[scales.list$scale_theory==i,]$pc_theory){ # redundant in this case - relevant if we want to use this code for non-prison climate questions only
    
    # Means and SDs
    ## Identify scales and accompanying question numbers 
    qnos <- data.frame(no = unique(pcq_lookup[pcq_lookup$scale_theory==i & 
                                                pcq_lookup$pc_theory==k &
                                                pcq_lookup$include_comparative_psych_analysis=="yes",
                                              "question_no_pa_2022a"]),                    
                       qno = NA)
    qnos$qno <- paste0("q",qnos$no)
    
    rows.without.nas <- which(complete.cases(pcq[,qnos$qno])==TRUE) # Complete cases include those that have no opinion or not applicable answers, but exclude questions left blank
    
    # Calculate mean of identified questions that were answered for each individual 
    pcq$ind.mean <- NA
    pcq[rows.without.nas,]$ind.mean <- rowSums(pcq2[rows.without.nas,qnos$qno], na.rm=TRUE) #summing answered questions
    pcq$no_completed_qs <- NA #Calulate number of answered questions that were not 'no opinion' (cannot count these for the average)
    for(j in 1:nrow(pcq)){
      pcq[j,]$no_completed_qs <- length(qnos$qno)-length(which(pcq[j,qnos$qno]==111)) 
    }
    pcq$ind.mean <- ifelse(!is.na(pcq$ind.mean),pcq$ind.mean/pcq$no_completed_qs, pcq$ind.mean)
    
    ## Calculate mean and sd of individual means 
    temp <- pcq %>% 
      summarize(mean = mean(ind.mean, na.rm = TRUE),
                sd = sd(ind.mean, na.rm = TRUE)) %>% 
      mutate(scale_theory = i,pc_theory = k) %>%
      `colnames<-`(c("mean","sd","scale_theory","pc_theory"))
    
    
    keep[which(scales.list$scale_theory==i & scales.list$pc_theory==k),c("mean", "sd", "scale","pc")] <- temp
    
    # Calculate Crownbach's Alpha and number of items per question
    # This uses dataframe pcq2 (where no opinion and not applicable cases have been set to NA), but uses pcq to calculate complete cases, and thus includes all cases with no opinion/not applicable cases in the calculation of the cronbach coefficient.
    # Not calculated if only 1 question in a scale_theory (e.g. 'comments')
    if(i != "overall"){
    keep[keep$scale==i & keep$pc==k,"cr.alpha"] <- ifelse(ncol(pcq[,qnos$qno])>1,cronbach.alpha(pcq2[complete.cases(pcq[,qnos$qno]),qnos$qno], na.rm=TRUE)[1],NA)
    keep[keep$scale==i & keep$pc==k,"complete_responses"] <- ifelse(ncol(pcq[,qnos$qno])>1,cronbach.alpha(pcq2[complete.cases(pcq[,qnos$qno]),qnos$qno], na.rm=TRUE)[2],NA)
    keep[keep$scale==i & keep$pc==k,"items"] <- ifelse(ncol(pcq[,qnos$qno])>1,cronbach.alpha(pcq2[complete.cases(pcq[,qnos$qno]),qnos$qno], na.rm=TRUE)[3],NA)
    }
    
    # Spearman-Brown, split-half reliability
    # The spearman-brown corrected reliability estimates are calculated over all individuals who gave numerical answers to all questions in a given scale_theory. This is inconsistent with the calculation of the Chronbach's Alpha coefficient but I failed to figure out how to include the NA values in the SB calculations. 
    rows.without.nas <- which(complete.cases(pcq2[,qnos$qno])==TRUE) 
    keep[keep$scale==i & keep$pc==k,"complete_responses_pcq2"] <- length(rows.without.nas)
    if(k=="prison climate" & nrow(qnos)>3){
      temp <- pcq[rows.without.nas,c("research_id",qnos$qno)] 
      temp <- as.data.frame(temp)
      temp <- reshape(data=temp, idvar="research_id",
                      varying = qnos$qno,
                      v.name=c("RT"),
                      times = qnos$qno,
                      direction="long")
      names(temp) <- c("research_id", "trial_number", "RT")
      
      sb.stat <- splithalf(data = temp,
                           outcome = "RT",
                           score = "average", 
                           halftype = "random", 
                           permutations = 1000, 
                           var.RT = "RT",
                           var.participant = "research_id",
                           average = "mean",
                           plot = FALSE)
      keep[keep$scale==i & keep$pc==k,c("sb.stat", "sb_low", "sb_high")] <- sb.stat$final_estimates[,c("spearmanbrown", "SB_low", "SB_high")]
    }
  }
}
keep$sb_range <- paste0("(",keep$sb_low,", ",keep$sb_high,")")



# Generate table 
tab <- keep[keep$pc=="prison climate",c("scale", "mean","sd", "cr.alpha","sb_range", "items", "complete_responses","complete_responses_pcq2")]
data <- pcq_lookup[pcq_lookup$pc_theory=="prison climate" & pcq_lookup$in_which_survey =="NL & PA",c("scale_theory", "scale_theory_long", "domain_name", "domain_no_bosma")]
names(data) <- c("scale", "scale_theory_long", "domain_name", "domain_no_bosma")
tab <- left_join(tab,unique(data))
tab <- tab[,-which(names(tab)=="scale")]
tab <- tab[order(tab$domain_no_bosma), ] # Sort by domain
tab <- tab[,c(10,9,8,6,7,5,1,2,3,4)]   #tab[,c(9,1,7,8,6,2,3,4,5)]
names(tab) <- c("No","Domain","Scale","n1", "n2", "#","M","SD", "CA","SB")
for(i in c("n1", "n2", "#", "M", "SD", "CA")){
  tab[,i] <- as.numeric(tab[,i])
}
print(xtable(tab, digits=c(0,0,0,0,0,0,0,2,2,2,0), caption="Descriptive Statistics for Prison Climate Scales"),include.rownames=FALSE, file="output/tables/robustness_internal_consistency.txt") 
}

# Run internal consistency table for imputed data
# Note that this is unnecessarily clumsy code; adapted from old code. Simpify when cleaning code
want.to.rerun.internal.consistency <- TRUE
if(want.to.rerun.internal.consistency == TRUE){
  scales.list <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain & 
                                     pcq_lookup$scale_theory_long %ni% c('Overall Question', "Subjective Severity of Imprisonment"),c("scale_theory", "pc_theory")])
  keep <- data.frame(mean = rep("boo", nrow(scales.list)),
                     sd = rep("boo", nrow(scales.list)),
                     scale = rep("boo", nrow(scales.list)),
                     pc = rep("boo", nrow(scales.list)),
                     cr.alpha = rep("boo", nrow(scales.list)),
                     sb.stat = rep("boo", nrow(scales.list)),
                     sb_low = rep("boo", nrow(scales.list)),
                     sb_high = rep("boo", nrow(scales.list)),
                     items = rep("boo", nrow(scales.list)),
                     complete_responses = rep("boo", nrow(scales.list)),
                     complete_responses_pcq2 = rep("boo", nrow(scales.list)))
  
  for(i in scales.list$scale_theory){
    for(k in scales.list[scales.list$scale_theory==i,]$pc_theory){ # redundant in this case - relevant if we want to use this code for non-prison climate questions only
      
      # Means and SDs
      ## Identify scales and accompanying question numbers 
      qnos <- data.frame(no = unique(pcq_lookup[pcq_lookup$scale_theory==i & 
                                                  pcq_lookup$pc_theory==k &
                                                  pcq_lookup$include_comparative_psych_analysis=="yes",
                                                "question_no_pa_2022a"]),                    
                         qno = NA)
      qnos$qno <- paste0("q",qnos$no)
      
      rows.without.nas <- which(complete.cases(pcq_am[,qnos$qno])==TRUE) # Should be everyone except those who had only NA answers 
      
      # Calculate mean of identified questions that were answered for each individual 
      pcq_am$ind.mean <- NA
      pcq_am[rows.without.nas,]$ind.mean <- rowSums(pcq_am[rows.without.nas,qnos$qno], na.rm=TRUE) # should be every
      pcq_am$no_completed_qs <- NA #Calulate number of answered questions that were not 'no opinion' (cannot count these for the average)
      for(j in 1:nrow(pcq_am)){
        pcq_am[j,]$no_completed_qs <- length(qnos$qno)-length(which(pcq_am[j,qnos$qno]==111)) 
      }
      pcq_am$ind.mean <- ifelse(!is.na(pcq_am$ind.mean),pcq_am$ind.mean/pcq_am$no_completed_qs, pcq_am$ind.mean)
      
      ## Calculate mean and sd of individual means 
      temp <- pcq_am %>% 
        summarize(mean = mean(ind.mean, na.rm = TRUE),
                  sd = sd(ind.mean, na.rm = TRUE)) %>% 
        mutate(scale_theory = i,pc_theory = k) %>%
        `colnames<-`(c("mean","sd","scale_theory","pc_theory"))
      
      
      keep[which(scales.list$scale_theory==i & scales.list$pc_theory==k),c("mean", "sd", "scale","pc")] <- temp
      
      # Calculate Crownbach's Alpha and number of items per question
      if(i != "overall"){
        keep[keep$scale==i & keep$pc==k,"cr.alpha"] <- ifelse(ncol(pcq_am[,qnos$qno])>1,cronbach.alpha(pcq_am[complete.cases(pcq_am[,qnos$qno]),qnos$qno], na.rm=TRUE)[1],NA)
        keep[keep$scale==i & keep$pc==k,"complete_responses"] <- ifelse(ncol(pcq_am[,qnos$qno])>1,cronbach.alpha(pcq_am[complete.cases(pcq_am[,qnos$qno]),qnos$qno], na.rm=TRUE)[2],NA)
        keep[keep$scale==i & keep$pc==k,"items"] <- ifelse(ncol(pcq_am[,qnos$qno])>1,cronbach.alpha(pcq_am[complete.cases(pcq_am[,qnos$qno]),qnos$qno], na.rm=TRUE)[3],NA)
      }
      
      # Spearman-Brown, split-half reliability
      rows.without.nas <- which(complete.cases(pcq_am[,qnos$qno])==TRUE) 
      keep[keep$scale==i & keep$pc==k,"complete_responses_pcq2"] <- length(rows.without.nas)
      if(k=="prison climate" & nrow(qnos)>3){
        temp <- pcq_am[rows.without.nas,c("research_id",qnos$qno)] 
        temp <- as.data.frame(temp)
        temp <- reshape(data=temp, idvar="research_id",
                        varying = qnos$qno,
                        v.name=c("RT"),
                        times = qnos$qno,
                        direction="long")
        names(temp) <- c("research_id", "trial_number", "RT")
        
        sb.stat <- splithalf(data = temp,
                             outcome = "RT",
                             score = "average", 
                             halftype = "random", 
                             permutations = 1000, 
                             var.RT = "RT",
                             var.participant = "research_id",
                             average = "mean",
                             plot = FALSE)
        keep[keep$scale==i & keep$pc==k,c("sb.stat", "sb_low", "sb_high")] <- sb.stat$final_estimates[,c("spearmanbrown", "SB_low", "SB_high")]
      }
    }
  }
  keep$sb_range <- paste0("(",keep$sb_low,", ",keep$sb_high,")")
  
  # Generate table 
  tab <- keep[keep$pc=="prison climate",c("scale", "mean","sd", "cr.alpha","sb_range", "items", "complete_responses","complete_responses_pcq2")]
  data <- pcq_lookup[pcq_lookup$pc_theory=="prison climate" & pcq_lookup$in_which_survey =="NL & PA",c("scale_theory", "scale_theory_long", "domain_name", "domain_no_bosma")]
  names(data) <- c("scale", "scale_theory_long", "domain_name", "domain_no_bosma")
  tab <- left_join(tab,unique(data))
  tab <- tab[,-which(names(tab)=="scale")]
  tab <- tab[order(tab$domain_no_bosma), ] # Sort by domain
  tab <- tab[,c(10,9,8,5,1,2,3,4)]   #tab[,c(9,1,7,8,6,2,3,4,5)]
  names(tab) <- c("No","Domain","Scale","#","M","SD", "CA","SB")
  for(i in c("#", "M", "SD", "CA")){
    tab[,i] <- as.numeric(tab[,i])
  }
  print(xtable(tab, digits=c(0,0,0,0,0,2,2,2,0), caption="Descriptive Statistics for Prison Climate Scales"),include.rownames=FALSE, file="output/tables/imputed_internal_consistency.txt") 
}


## Plot Dutch means against PA means ####
# Note runs against latest tab run above 
temp <- tab[,c("Scale", "M")]
temp2 <- c("Prisoner relationships", 3.44, 
          "Staff-prisoner relationships", 3.32,
          "Procedural Justice", 3.30,                    
          "Safety", 4,                               
          "Satisfaction With Frequency of Contact", 2.84,
          "Satisfaction with Visits", 2.94,              
          "Quality of care", 3.30,                      
          "Sleep Quality", 2.77,                
          "Shop Quality", 2.39,                  
          "Satisfaction with Activities", 3.12,      
          "Availability of Meaningful Activities", 2.27,
          "Reintegration", 2.49,                         
          "Autonomy", 2.71,                  
          "Overall Question", 2.92,                    
          "Subjective Severity of Imprisonment", 3.48)
temp2 <- as.data.frame(matrix(temp2, nrow=length(temp2)/2, ncol=2, byrow=TRUE))
names(temp2) <- c("Scale", "M.NL")
temp2$M.NL <- as.numeric(temp2$M.NL)
temp <- left_join(temp, temp2)
data <- temp[temp$Scale %ni% c("Overall Question", "Subjective Severity of Imprisonment"),]

library(ggrepel)
p <- ggplot(data,
       aes(x=M, y=M.NL)) +
  geom_point() +
  geom_text_repel(aes(label=Scale), point.padding = .8, size=2.5)+
  geom_abline(intercept = 0, slope=1, linetype="dotted") + 
  xlim(min(c(data$M, data$M.NL))-.1,max(c(data$M, data$M.NL))+.1) + 
  ylim(min(c(data$M, data$M.NL))-.1,max(c(data$M, data$M.NL))+.1) + 
  ggtitle("Prison Climate Scale Means", 
          subtitle = "Means from SCI Chester and Dutch Prisons") +
  ylab("The Netherlands") + 
  xlab("SCI Chester") + 
  theme_bw()

ggsave("output/figures/means_PA_NL.jpg", plot = p, width = 6, height = 6, dpi = 300)

# ---------- Factor analysis NL PA  ---------- ----
# Resources
# https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html
# https://www.statmethods.net/advstats/factor.html
# https://towardsdatascience.com/exploratory-factor-analysis-in-r-e31b0015f224

## Set up ####
# Identify questions to use 
# Do not include subjective severity or overall satisfaction
qnos.qs <- unique(pcq_lookup[pcq_lookup$pc=="prison climate",c("question_qno", "domain_name", "scale_theory", "question_no_pa_2022a", "factor_no_bosma", "factor_loading_bosma")])

qnos <- unique(with(pcq_lookup, pcq_lookup[which(pc_theory=="prison climate" & 
                                                   in_which_survey =="NL & PA" & 
                                                   include_comparative_psych_analysis=="yes" & 
                                                   question_qno %ni% c("q1", "q7", "q8", "q9")),"question_qno"])) 

## Kaiser-Meyer-Olkin (KMO) 
# Using two different packages to confirm result 
KMO(r=cor(pcq2[complete.cases(pcq2[,qnos]),qnos]))
KMOS(pcq2[complete.cases(pcq2[,qnos]),qnos], use = "pairwise.complete.obs")

KMO(r=cor(pcq_am[complete.cases(pcq_am[,qnos]),qnos]))
KMOS(pcq_am[complete.cases(pcq_am[,qnos]),qnos], use = "pairwise.complete.obs")

## Bartlett’s Test of Sphericity
# Method 1
cor_matrix <- cor(pcq2[complete.cases(pcq2[,qnos]),qnos])
cortest.bartlett(cor_matrix, n = nrow(pcq2[complete.cases(pcq2[,qnos]),qnos]))

cor_matrix <- cor(pcq_am[complete.cases(pcq_am[,qnos]),qnos])
cortest.bartlett(cor_matrix, n = nrow(pcq_am[complete.cases(pcq_am[,qnos]),qnos]))

# Method 2 
cortest.bartlett(pcq2[complete.cases(pcq2[,qnos]),qnos],n = nrow(pcq2[complete.cases(pcq2[,qnos]),qnos])) #Small values of the significance level indicate that a factor analysis may be useful with our data.
det(cor(pcq2[complete.cases(pcq2[,qnos]),qnos])) #We have a positive determinant, which means the factor analysis will probably run.

## Parallel analysis ####
# parallel <- fa.parallel(pcq2[complete.cases(pcq[,qnos]),qnos], fm="ml", fa="both", sim=FALSE)
parallel <- fa.parallel(pcq_am[,qnos], fm="pa", fa="both", sim=FALSE)
parallel$fa.values # Eigenvalues for factors 
parallel$pc.values # Eigenvalues for components 
parallel$nfact
parallel$ncomp

## Factor analysis based on 12 factors as identified by parallel analysis ####
# Set number of factors

# Code for factor analysis
# Update object name if no longer doing ml! 
# fa.pooled option could work with several imputed datasets!!
fa.ml.oblimin <- fa(r=pcq_am[,qnos],  # pcq2[,qnos]
                    nfactors = parallel$nfact, 
                    # covar = FALSE, SMC = TRUE,
                    fm="pa", # ml for max likelihood (consider paf, used in Malandrone)
                    rotate="oblimin",
                    warnings=TRUE,
                    missing=TRUE,
                    impute="mean") # none rotation
fa.diagram(fa.ml.oblimin)
# fa.ml.oblimin$Phi # Interfactor correlations 
# fa.ml.oblimin$RMSEA
# fa.ml.oblimin$crms
print(fa.ml.oblimin)


# Link factors to PCQ scales
# Using the scales of the question with the highest loading for this
temp <- data.frame(unclass(fa.ml.oblimin$loadings)) # Extract factor loadings
temp$question_qno <- rownames(temp)
temp <- left_join(temp, pcq_lookup[,c("question_qno", "question_no_pa_2022a","question_pa_2022a", "scale_theory")])
for(i in 1:n_factors){
  names(temp)[i] <- temp[which.max(abs(temp[,i])),"scale_theory"]
}
temp <- temp[,c((n_factors+1):ncol(temp),1:n_factors)] # Shuffle columns 

# Build Table 2 in Bosma 
# Retain only those values with more than .265 (Bosma uses .4, in our case, I feel safe in this institution is .29, and satisfaction with religious services loads at .269)

# Set up dataframe keep based on first factor
temp2 <- temp[,4+1] # Hard coded 
k <- which(abs(temp2)>.25)
keep <- data.frame(loading.value=temp[k,4+1],
                   factor=rep(names(temp)[4+1], length(k)),
                   question_no_pa_2022a=temp[k,"question_no_pa_2022a"],
                   question_pa_2022a=temp[k,"question_pa_2022a"])

# repeat for all other factors 
for(i in 2:n_factors){
  temp2 <- temp[,4+i] 
  k <- which(abs(temp2)>.265)
  temp3 <- data.frame(loading.value=temp[k,4+i],
                      factor=rep(names(temp)[4+i], length(k)),
                      question_no_pa_2022a=temp[k,"question_no_pa_2022a"],
                      question_pa_2022a=temp[k,"question_pa_2022a"])
  keep <- rbind(keep, temp3)
}
length(qnos)==nrow(keep) # 58,61. False because Keep includes three questions twice 

# Check that a question loads onto the anticipated factor 
tab <- left_join(qnos.qs[qnos.qs$question_qno %in% qnos,], keep) 
tab$check <- tab$scale_theory==tab$factor
tab$loading.value <- as.numeric(tab$loading.value)
unique(tab$scale_theory)[which(unique(tab$scale_theory) %ni% unique(tab$factor))]
tab$question_pa_2022a[which(duplicated(tab$question_pa_2022a))]
tab <- tab[,c("domain_name", "scale_theory", "question_pa_2022a", "loading.value","factor_loading_bosma", "factor", "factor_no_bosma","check")] # For visual inspection

# Format table for output
tab <- tab[,c("question_pa_2022a", "factor_no_bosma", "loading.value","factor_loading_bosma")] 
tab <- tab[order(tab$factor_no_bosma),]
names(tab) <- c("Item", "Factor", "Loading PA", "Loading NL")
print(xtable(tab, digits=c(0,0,0,2,2)), include.rownames = FALSE, file="output/tables/imputed_factor_analysis.txt")

# Factor analysis using principal axis factoring
fa.parallel(pcq_am[,qnos], fm="pa", fa="both", sim=FALSE)

fa.ml.oblimin <- fa(r=pcq_am[,qnos],  # pcq2[,qnos]
                    nfactors = 13, 
                    # covar = FALSE, SMC = TRUE,
                    fm="pa", # ml for max likelihood (consider paf, used in Malandrone)
                    rotate="oblimin",
                    warnings=TRUE,
                    missing=TRUE,
                    impute="mean") # none rotation
fa.diagram(fa.ml.oblimin)

# Link factors to PCQ scales
# Using the scales of the question with the highest loading for this
temp <- data.frame(unclass(fa.ml.oblimin$loadings)) # Extract factor loadings
temp$question_qno <- rownames(temp)
temp <- left_join(temp, pcq_lookup[,c("question_qno", "question_no_pa_2022a","question_pa_2022a", "scale_theory")])
for(i in 1:n_factors){
  names(temp)[i] <- temp[which.max(abs(temp[,i])),"scale_theory"]
}
temp <- temp[,c((n_factors+1):ncol(temp),1:n_factors)] # Shuffle columns 

# Build Table 2 in Bosma 
# Retain only those values with more than .265 (Bosma uses .4, in our case, I feel safe in this institution is .29, and satisfaction with religious services loads at .269)

# Set up dataframe keep based on first factor
temp2 <- temp[,4+1] # Hard coded 
k <- which(abs(temp2)>.25)
keep <- data.frame(loading.value=temp[k,4+1],
                   factor=rep(names(temp)[4+1], length(k)),
                   question_no_pa_2022a=temp[k,"question_no_pa_2022a"],
                   question_pa_2022a=temp[k,"question_pa_2022a"])

# repeat for all other factors 
for(i in 2:n_factors){
  temp2 <- temp[,4+i] 
  k <- which(abs(temp2)>.265)
  temp3 <- data.frame(loading.value=temp[k,4+i],
                      factor=rep(names(temp)[4+i], length(k)),
                      question_no_pa_2022a=temp[k,"question_no_pa_2022a"],
                      question_pa_2022a=temp[k,"question_pa_2022a"])
  keep <- rbind(keep, temp3)
}
length(qnos)==nrow(keep) # 58,61. False because Keep includes three questions twice 

# Check that a question loads onto the anticipated factor 
tab <- left_join(qnos.qs[qnos.qs$question_qno %in% qnos,], keep) 
tab$check <- tab$scale_theory==tab$factor
tab$loading.value <- as.numeric(tab$loading.value)
unique(tab$scale_theory)[which(unique(tab$scale_theory) %ni% unique(tab$factor))]
tab$question_pa_2022a[which(duplicated(tab$question_pa_2022a))]
tab$factor_no_pa <- ifelse(tab$check==TRUE, tab$factor_no_bosma, tab$factor)
tab$factor_no_pa <- ifelse(tab$factor=="procedure", 2,
                           ifelse(tab$factor=="actav", 12,
                                  ifelse(tab$factor=="visits.1", 5, tab$factor_no_pa)))
tab <- tab[,c("domain_name", "scale_theory", "question_pa_2022a", "loading.value","factor_loading_bosma", "factor","factor_no_pa", "factor_no_bosma","check")] # For visual inspection

# Format table for output
tab <- tab[,c("question_pa_2022a", "factor_no_pa", "loading.value","factor_no_bosma","factor_loading_bosma")] 
tab <- tab[order(tab$factor_no_bosma),]
names(tab) <- c("Item", "Factor PA", "Loading PA","Factor NL","Loading NL")
print(xtable(tab, digits=c(0,0,0,2,0,2)), include.rownames = FALSE, file="output/tables/imputed_factor_analysis_principal_axis.txt")

# ---------- Interscale correlation matrix ---------- ####
# Using scoreItems. See old code section for approach using cluster.cor based on a correlation matrix
key.list <- list(pr=c("q10", "q11", "q12", "q13", "q14"), # prisoner relationships
                 sr=c("q15", "q16", "q17", "q18"), # staff relationships 
                 pj=c("q19", "q20", "q21", "q22"), # procedural justice 
                 sf=c("q26", "q29", "q30", "q31", "q32"), # safety
                 sv=c("q139", "q140", "q141", "q142", "q143", "q144", "q145", "q146"), #sat with visits
                 fc=c("q135", "q136", "q137"), # frequency of contact
                 sq=c("q94", "q95", "q96"), # sleep quality 
                 qc=c("q111", "q112", "q119", "q120", "q121", "q122"), # quality of care
                 sq=c("q99","q100", "q101"), # shop quality 
                 as=c("q62", "q63", "q64", "q65", "q66", "q67", "q68"), # satisfaction with activities 
                 aa=c("q69", "q70", "q71", "q72"), # availability of activities
                 re=c("q73", "q74", "q75", "q76"), # reintegration
                 au=c("q82", "q83", "q84", "q85"))# autonomy 

qnos <- unique(pcq_lookup[pcq_lookup$pc_theory=="prison climate" & 
                            pcq_lookup$in_which_survey =="NL & PA" &
                            pcq_lookup$scale_theory %ni% c("severity", "overall") &
                            pcq_lookup$include_comparative_psych_analysis=="yes",
                          c("question_no_pa_2022a","question_qno","scale_theory","scale_theory_long")]) 
scale_names <- data.frame(scale_abbreviation = names(key.list),
                          scale_theory_long = c("Prisoner relationships",
                                                "Staff-prisoner relationships",
                                                "Procedural Justice",
                                                "Safety",
                                                "Satisfaction with Visits",
                                                "Satisfaction With Frequency of Contact",
                                                "Sleep Quality",
                                                "Quality of care",
                                                "Shop Quality",
                                                "Satisfaction with Activities",
                                                "Availability of Meaningful Activities",
                                                "Reintegration",
                                                "Autonomy"))

n_scales <- length(unique(qnos$scale_theory))
my.scores <- scoreItems(key.list,
                        pcq_am[,qnos$question_qno],
                        missing=TRUE,
                        impute="median") 
print(my.scores, short=FALSE)

# Generate all four possible interscale correlation matrices
# Retain only the corrected correlations (above the diagonal)
# Choose which matrix to use 
matrix.choice <- list(MIMS = my.scores$MIMS,
                      MIMT = my.scores$MIMT,
                      matrix.cor = my.scores$cor,
                      matrix.corrected = my.scores$corrected) # Used corrected one in current version of text 
for(k in 1:length(matrix.choice)){
  file.name <- paste0("output/tables/imputed_interscale_correlations_",names(matrix.choice)[k],".txt")
  keep <- t(as.vector(c(rep(0,1),rep(1,n_scales-1))))
  for(i in 2:n_scales){
    keep <- rbind(keep,t(as.vector(c(rep(0,i), rep(1,n_scales-i)))))
  }
  tab <- t(as.data.frame(matrix.choice[k][[1]]*keep))
  tab[which(tab==0)] <- ""
  tab <- as.data.frame(tab)
  tab <- tab[1:12]
  tab <- as.data.frame(lapply(tab,as.numeric)) # convert all columns to numeric
  names(tab) <- seq(1,n_scales-1,1)
  row.names(tab) <- scale_names$scale_theory_long
  print(xtable(tab, digits=c(0,rep(3,12)), caption=paste0("Interscale Correlation Matrix: ",names(matrix.choice)[k])),
        include.rownames=TRUE, 
        file = file.name)
}

# ----------  Table with overall means by Unit  ----------  ####
# Make baseline table based on means of answered questions - for robustness 
data <- pcq2[,grep("maq_", names(pcq2))]
tab <- describeBy(data, group = pcq2$unit_type, digits=2, mat=TRUE)
tab$scale_theory <- gsub("maq_", "",rownames(tab)) # get scale names 
tab$scale_theory <- substr(tab$scale_theory, 1, nchar(tab$scale)-1)
tab <- tab[,c("mean", "group1", "scale_theory")]
tab <- reshape(tab, idvar = "scale_theory", timevar = "group1", direction = "wide", times = )
names(tab) <- gsub("mean.", "", names(tab))
tab <- left_join(tab,unique(pcq_lookup[which(pcq_lookup$pc_theory=="prison climate" &
                                               pcq_lookup$include_comparative_psych_analysis=="yes"),c("scale_theory_long","scale_theory", "scale_no_bosma")]))
tab <- tab %>%  arrange(scale_no_bosma) %>%
  relocate(scale_theory_long, .before=gp) %>%
  dplyr::select(-scale_theory, -scale_no_bosma, -ls) %>%
  filter(scale_theory_long %ni% c("Overall Question", "Subjective Severity of Imprisonment"))
names(tab) <- c("Scale", "General", "Therapeutic", "Honor", "Recovery", "Restrictive", "Transitional")

print(xtable(tab, digits = c(0,0,rep(2,6))), include.rownames = FALSE, file = "output/tables/maq_unit_differences.txt")

# Calculate significant differences for superscript
data <- pcq2[which(pcq2$unit_type != "ls"),]
names.list <- names(pcq2)[grep("maq_", names(pcq2))]
names.list <- names.list[names.list != "maq_overall"]
for(i in names.list){
  print(i)
  lminput <- as.formula(paste(i, "~ unit_type")) 
  print(TukeyHSD(aov(lminput, data = data)))
  print("---------------------------------")
}

# Heat map of means 
data <- tab %>%
  pivot_longer(!Scale, names_to = "Unit", values_to = "Mean")
p <- ggplot(data, aes(x = Unit, y = Scale, fill = Mean)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Mean), color = "black", size = 3) +
  scale_fill_gradient(low = "blue", high = "white") +
  coord_fixed(ratio = .3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
ggsave("output/figures/maq_unit_means.jpg", plot = p, width = 9, height = 6, dpi = 300)

# Based on imputed data
data <- pcq_am[,grep("mam_", names(pcq_am))]
tab <- describeBy(data, group = pcq2$unit_type, digits=2, mat=TRUE)
tab$scale_theory <- gsub("mam_", "",rownames(tab)) # get scale names 
tab$scale_theory <- substr(tab$scale_theory, 1, nchar(tab$scale)-1)
tab <- tab[,c("mean", "group1", "scale_theory")]
tab <- reshape(tab, idvar = "scale_theory", timevar = "group1", direction = "wide", times = )
names(tab) <- gsub("mean.", "", names(tab))
tab <- left_join(tab,unique(pcq_lookup[which(pcq_lookup$pc_theory=="prison climate" &
                                               pcq_lookup$include_comparative_psych_analysis=="yes"),c("scale_theory_long","scale_theory", "scale_no_bosma")]))
tab <- tab %>%  arrange(scale_no_bosma) %>%
  relocate(scale_theory_long, .before=gp) %>%
  dplyr::select(-scale_theory, -scale_no_bosma, -ls) %>%
  filter(scale_theory_long %ni% c("Overall Question", "Subjective Severity of Imprisonment"))
names(tab) <- c("Scale", "General", "Therapeutic", "Honor", "Recovery", "Restrictive", "Transitional")

print(xtable(tab, digits = c(0,0,rep(2,6))), include.rownames = FALSE, file = "output/tables/imputed_unit_differences.txt")

# Calculate significant differences for superscript
data <- pcq_am[which(pcq_am$unit_type != "ls"),]
names.list <- names(pcq_am)[grep("mam_", names(pcq_am))]
for(i in names.list){
  print(i)
  lminput <- as.formula(paste(i, "~ unit_type")) 
  print(TukeyHSD(aov(lminput, data = data)))
  print("---------------------------------")
}

# Heat map of means 
data <- tab %>%
  pivot_longer(!Scale, names_to = "Unit", values_to = "Mean")
p <- ggplot(data, aes(x = Unit, y = Scale, fill = Mean)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Mean), color = "black", size = 3) +
  scale_fill_gradient(low = "blue", high = "white") +
  coord_fixed(ratio = .3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
ggsave("output/figures/imputed_unit_means.jpg", plot = p, width = 9, height = 6, dpi = 300)

# ---------- Table 4 Regressions ----------  ####
# Code for regressions for Table 4 in Bosma. Based on simple scale averages. 
# Data
basic_temp <- basic[which(basic$research_id %in% unique(pcq$research_id) &
                            basic$date_datapull %in% c(ymd(20220625), ymd(20220903))),]

# Deduplicate for individuals for whom I have data from both datapulls 
basic_temp$pull_no <- 1
temp <- data.frame(table(basic_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
basic_temp[basic_temp$research_id %in% index & basic_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
basic_temp <- basic_temp[which(basic_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 

# Imputed data
plot.name <- "output/tables/imputed_criterion_validity.txt"
data.reg <- left_join(pcq_am_full,basic_temp) # pcq2_full
data.reg <- data.reg[which(data.reg$race_code != "A"),] # just two individuals, cause regression not to run 
data.reg$est_months_served_on_20220501 <- data.reg$est_days_served_on_20220501/30.5

# Set independent variables for both regressions
regressors <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain &
                                  pcq_lookup$pc_theory=="prison climate" &
                                  pcq_lookup$scale_theory %ni% c("overall", "severity"),
                                c("scale_theory")])
regressors <- paste0("mam_", regressors)

indvars.controls <- data.reg %>% dplyr::select(age_on_20220501, foreign_born, high_school, partner, children, est_months_served_on_20220501, race_code, cell, asca)
indvars.controls.scales <- data.reg %>% dplyr::select(all_of(regressors), age_on_20220501, foreign_born, high_school, partner, children, est_months_served_on_20220501, race_code, cell, asca)

# Depvar: I am satisfied with this insitutiton 
fit.controls.inst <- lm(data.reg$q1 ~ ., data=indvars.controls)
fit.controls.scales.inst <- lm(data.reg$q1 ~ ., data=indvars.controls.scales)
summary(fit.controls.inst)$adj.r.squared
summary(fit.controls.scales.inst)$adj.r.squared

# Depvar: I am satisfied with this unit 
fit.controls.unit <- lm(data.reg$q4 ~ ., data=indvars.controls)
fit.controls.scales.unit <- lm(data.reg$q4 ~ ., data=indvars.controls.scales)
summary(fit.controls.unit)$adj.r.squared
summary(fit.controls.scales.unit)$adj.r.squared

# Depvar: Severity Scale 
fit.controls.sev <- lm(data.reg$mam_severity ~ ., data=indvars.controls)
fit.controls.scales.sev <- lm(data.reg$mam_severity ~ ., data=indvars.controls.scales)
summary(fit.controls.sev)$adj.r.squared
summary(fit.controls.scales.sev)$adj.r.squared

# Table
texreg(list(fit.controls.scales.inst, fit.controls.scales.unit,fit.controls.scales.sev),
       include.ci = FALSE,
       caption="Criterion Validity",
       caption.above=TRUE,
       custom.coef.map = list("mam_prisoners" = "Prisoner Relationships",
                              "mam_staff" = "Staff-Prisoner Relationships",
                              "mam_procedure" = "Procedural Justice",
                              "mam_safety" = "Safety",
                              "mam_visits" = "Satisfaction with Visits",
                              "mam_contact" = "Frequency of Contact",
                              "mam_sleep" = "Sleep quality",
                              "mam_care" = "Quality of Care",
                              "mam_shop" = "Shop quality",
                              "mam_actsat" = "Satisfaction with Activities",
                              "mam_actav" = "Availability of Activities",
                              "mam_reint" = "Reintegration",
                              "mam_autonomy" = "Autonomy",
                              "age_on_20220501" = "Age",
                              "foreign_born" = "Foreign Born",
                              "high_school" = "Finished High School",
                              "partner" = "Has Partner",
                              "children" = "Has Children",
                              "est_months_served_on_20220501" = "Time Served (months)",
                              "race_codeB" = "Black",
                              "cell" = "Shares a cell",
                              "asca2-Property" = "Property Offense (ref = violent)",
                              "asca3-Drugs" = "Drugs Offense (ref = violent)",
                              "asca4-Public Order" = "Public Order Offense (ref = violent)"),
       custom.header=list("Institution" = 1,
                          "Unit" = 2,
                          "Subjective Severity" = 3),  
       custom.model.names = c("(1)", "(2)", "(3)"),
       digits=3,
       stars = c(0.001, 0.01, 0.05),
       column.spacing = 0,
       custom.note = "Notes: Standard errors in parentheses. %stars ",
       file=plot.name)

# Means of answered questions 
plot.name <- "output/tables/maq_criterion_validity.txt"
data.reg <- left_join(pcq2_full,basic_temp) 
data.reg <- data.reg[which(data.reg$race_code != "A"),] # just two individuals, cause regression not to run 
data.reg$est_months_served_on_20220501 <- data.reg$est_days_served_on_20220501/30.5

# Set independent variables for both regressions
regressors <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain &
                                  pcq_lookup$pc_theory=="prison climate" &
                                  pcq_lookup$scale_theory %ni% c("overall", "severity"),
                                c("scale_theory")])
regressors <- paste0("maq_", regressors)

indvars.controls <- data.reg %>% dplyr::select(age_on_20220501, foreign_born, high_school, partner, children, est_months_served_on_20220501, race_code, cell, asca)
indvars.controls.scales <- data.reg %>% dplyr::select(all_of(regressors), age_on_20220501, foreign_born, high_school, partner, children, est_months_served_on_20220501, race_code, cell, asca)

# Depvar: I am satisfied with this insitutiton 
fit.controls.inst <- lm(data.reg$q1 ~ ., data=indvars.controls)
fit.controls.scales.inst <- lm(data.reg$q1 ~ ., data=indvars.controls.scales)
summary(fit.controls.inst)$adj.r.squared
summary(fit.controls.scales.inst)$adj.r.squared

# Depvar: I am satisfied with this unit 
fit.controls.unit <- lm(data.reg$q4 ~ ., data=indvars.controls)
fit.controls.scales.unit <- lm(data.reg$q4 ~ ., data=indvars.controls.scales)
summary(fit.controls.unit)$adj.r.squared
summary(fit.controls.scales.unit)$adj.r.squared

# Depvar: Severity Scale 
fit.controls.sev <- lm(data.reg$maq_severity ~ ., data=indvars.controls)
fit.controls.scales.sev <- lm(data.reg$maq_severity ~ ., data=indvars.controls.scales)
summary(fit.controls.sev)$adj.r.squared
summary(fit.controls.scales.sev)$adj.r.squared

# Table
texreg(list(fit.controls.scales.sev, fit.controls.scales.inst, fit.controls.scales.unit),
       include.ci = FALSE,
       caption="Criterion Validity",
       caption.above=TRUE,
       custom.coef.map = list("maq_prisoners" = "Prisoner Relationships",
                              "maq_staff" = "Staff-Prisoner Relationships",
                              "maq_procedure" = "Procedural Justice",
                              "maq_safety" = "Safety",
                              "maq_visits" = "Satisfaction with Visits",
                              "maq_contact" = "Frequency of Contact",
                              "maq_sleep" = "Sleep quality",
                              "maq_care" = "Quality of Care",
                              "maq_shop" = "Shop quality",
                              "maq_actsat" = "Satisfaction with Activities",
                              "maq_actav" = "Availability of Activities",
                              "maq_reint" = "Reintegration",
                              "maq_autonomy" = "Autonomy",
                              "age_on_20220501" = "Age",
                              "foreign_born" = "Foreign Born",
                              "high_school" = "Finished High School",
                              "partner" = "Has Partner",
                              "children" = "Has Children",
                              "est_months_served_on_20220501" = "Time Served (months)",
                              "race_codeB" = "Black",
                              "cell" = "Shares a cell",
                              "asca2-Property" = "Property Offense (ref = violent)",
                              "asca3-Drugs" = "Drugs Offense (ref = violent)",
                              "asca4-Public Order" = "Public Order Offense (ref = violent)"),
       custom.header=list("Subjective Severity" = 1, 
                          "Institution" = 2,
                          "Unit" = 3),  
       custom.model.names = c("(1)", "(2)", "(3)"),
       digits=3,
       stars = c(0.001, 0.01, 0.05),
       column.spacing = 0,
       custom.note = "Notes: Standard errors in parentheses. %stars ",
       file=plot.name)

# Repeat but with imputed scale means (mip_)
# Set independent variables for both regressions
plot.name <- "output/tables/mip_criterion_validity.txt"
regressors <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain &
                                  pcq_lookup$pc_theory=="prison climate" &
                                  pcq_lookup$scale_theory %ni% c("overall", "severity"),
                                c("scale_theory")])
regressors <- paste0("mip_", regressors)

indvars.controls <- data.reg %>% dplyr::select(age_on_20220501, foreign_born, high_school, partner, children, est_months_served_on_20220501, race_code, cell, asca)
indvars.controls.scales <- data.reg %>% dplyr::select(all_of(regressors), age_on_20220501, foreign_born, high_school, partner, children, est_months_served_on_20220501, race_code, cell, asca)

# Depvar: I am satisfied with this insitutiton 
fit.controls.inst <- lm(data.reg$q1 ~ ., data=indvars.controls)
fit.controls.scales.inst <- lm(data.reg$q1 ~ ., data=indvars.controls.scales)
summary(fit.controls.inst)$adj.r.squared
summary(fit.controls.scales.inst)$adj.r.squared

# Depvar: I am satisfied with this unit 
fit.controls.unit <- lm(data.reg$q4 ~ ., data=indvars.controls)
fit.controls.scales.unit <- lm(data.reg$q4 ~ ., data=indvars.controls.scales)
summary(fit.controls.unit)$adj.r.squared
summary(fit.controls.scales.unit)$adj.r.squared

# Depvar: Severity Scale 
fit.controls.sev <- lm(data.reg$mip_severity ~ ., data=indvars.controls)
fit.controls.scales.sev <- lm(data.reg$mip_severity ~ ., data=indvars.controls.scales)
summary(fit.controls.sev)$adj.r.squared
summary(fit.controls.scales.sev)$adj.r.squared

# 
texreg(list(fit.controls.scales.sev, fit.controls.scales.inst, fit.controls.scales.unit),
       include.ci = FALSE,
       caption="Criterion Validity",
       caption.above=TRUE,
       custom.coef.map = list("mip_prisoners" = "Prisoner Relationships",
                              "mip_staff" = "Staff-Prisoner Relationships",
                              "mip_procedure" = "Procedural Justice",
                              "mip_safety" = "Safety",
                              "mip_visits" = "Satisfaction with Visits",
                              "mip_contact" = "Frequency of Contact",
                              "mip_sleep" = "Sleep quality",
                              "mip_care" = "Quality of Care",
                              "mip_shop" = "Shop quality",
                              "mip_actsat" = "Satisfaction with Activities",
                              "mip_actav" = "Availability of Activities",
                              "mip_reint" = "Reintegration",
                              "mip_autonomy" = "Autonomy",
                              "age_on_20220501" = "Age",
                              "foreign_born" = "Foreign Born",
                              "high_school" = "Finished High School",
                              "partner" = "Has Partner",
                              "children" = "Has Children",
                              "est_months_served_on_20220501" = "Time Served (months)",
                              "race_codeB" = "Black",
                              "cell" = "Shares a cell",
                              "asca2-Property" = "Property Offense (ref = violent)",
                              "asca3-Drugs" = "Drugs Offense (ref = violent)",
                              "asca4-Public Order" = "Public Order Offense (ref = violent)"),
       custom.header=list("Subjective Severity" = 1, 
                          "Institution" = 2,
                          "Unit" = 3),  
       custom.model.names = c("(1)", "(2)", "(3)"),
       digits=3,
       stars = c(0.001, 0.01, 0.05),
       column.spacing = 0,
       custom.note = "Notes: Standard errors in parentheses. %stars ",
       file=plot.name)



### OLD CODE --------------------------------------------- ####
# DUPLICATE: DELETE? Surveyed Population Characteristics ####
# # Subset to unique individuals 
# basic_temp <- basic[which(basic$research_id %in% unique(pcq$research_id) &
#                             basic$date_datapull %in% c(ymd(20220625), ymd(20220903))),]
# 
# house_temp <- house[which(house$research_id %in% unique(pcq$research_id) &
#                             house$date_datapull %in% c(ymd(20220625), ymd(20220903)) &
#                             house$location_at_pcqwave1==1),]
# 
# # Deduplicate for individuals for whom I have data from both datapulls 
# basic_temp$pull_no <- 1
# temp <- data.frame(table(basic_temp$research_id))
# index <- temp[temp$Freq>1,"Var1"]
# basic_temp[basic_temp$research_id %in% index & basic_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
# basic_temp <- basic_temp[which(basic_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 
# 
# house_temp$pull_no <- 1
# temp <- data.frame(table(house_temp$research_id))
# index <- temp[temp$Freq>1,"Var1"]
# house_temp[house_temp$research_id %in% index & house_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
# house_temp <- house_temp[which(house_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 
# 
# # Function to calculate n, range, mean and sd
# f.descriptives <- function(data){
#   lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
# }
# 
# # Calculate characteristics for selected variables 
# vars <- c("min_sent_yrs", "max_sent_yrs")
# data <- basic_temp[basic_temp$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
# tab <- t(as.data.frame(f.descriptives(data)))
# 
# vars <- c("life","age_on_20220501", "high_school", "race_white", "race_black")
# data <- basic_temp[vars]
# tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
# 
# temp <- pcq_full
# temp$children <- ifelse(temp$q167==1, 0, # do you have children?
#                         ifelse(temp$q167==2, 1, NA))
# vars <- c("children")
# data <- temp[vars]
# tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
# 
# vars <- c("unit_type_gp", "unit_type_gp-tc", "unit_type_thu", "unit_type_hons", "unit_type_ls", "unit_type_rhu", "unit_type_rec")
# data <- fastDummies::dummy_cols(data.frame(unit_type = house_temp$unit_type))
# data <- data[,vars]
# tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
# tab <- as.data.frame(tab)
# 
# # Build table 
# tab <- as.data.frame(tab)
# names(tab) <- c("n","min", "max","mean","SD")
# row.names(tab) <- c("Minimum Sentence (in years)*",
#                     "Maximum Sentence (in years)*",
#                     "Life",
#                     "Age",
#                     "Education: High School or Higher",
#                     "Black",
#                     "White",
#                     "Children",
#                     "General Population",
#                     "Therapeutic Community",
#                     "Transitional Housing Unit",
#                     "Honor Block",
#                     "Little Scandinavia",
#                     "Restricted Housing Unit",
#                     "Recovery Unit")
# tab$range <- paste0("(",round(tab$min,2), ",", round(tab$max,2), ")")
# tab <- tab %>% dplyr::select(-min, -max)
# tab <- tab[,c("n","range","mean", "SD")]
# print(xtable(tab, digits=c(0,0,0,2,2), caption="Prison Population Characteristics"),include.rownames=TRUE,
#       file="output/tables/surveyed_population_characteristics.txt") 

# Interscale correlation matrix - Cluster.Cor ####
# Example here: https://www.rdocumentation.org/packages/psych/versions/1.0-17/topics/cluster.cor
# Note to self - this also produces Alpha! 

# Data: pcq.interscale
# Data I need: all complete cases (no missing data) for all prison climate questions
qnos <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain &
                            pcq_lookup$pc_theory=="prison climate" &
                            pcq_lookup$scale_theory %ni% c("severity", "overall"),
                          c("question_no_pa_2022a", "scale_theory", "scale_theory_long")]) 
qnos$qno <- paste0("q",qnos$question_no_pa_2022a)
rows.without.nas <- which(complete.cases(pcq2[,qnos$qno])==TRUE)
pcq.interscale <- pcq[rows.without.nas,qnos$qno] # my data
n_scales <- length(unique(qnos$scale_theory))

# Key
key_scales <- data.frame(cbind(scale_theory = unique(qnos$scale_theory),
                               scale_theory_long = unique(qnos$scale_theory_long),
                               scale_no = seq(1,n_scales,1)))
key_scales$scale_no <- as.numeric(key_scales$scale_no)
qnos <- left_join(qnos, key_scales)

for(i in 1:n_scales){ #create column for each scale
  qnos[,ncol(qnos)+1] <- NA
  names(qnos)[ncol(qnos)] <- paste0("scale_",i)
}

index <- 5 # hard coded - to indicate the col before start of scale columns 
for(i in 1:length(unique(qnos$scale_theory))){
  qnos[,index+i] <- ifelse(qnos$scale_no==i,1,0) #hard coded 
}
key_scales_matrix <- as.matrix(qnos[,(index+1):ncol(qnos)])

# Produce the interscale correlation matrix
r.mat <- cor(pcq.interscale)
interscale.matrix <- cluster.cor(key_scales_matrix,r.mat)

# Tables
# Retain only the corrected correlations (above the diagonal)
keep <- t(as.vector(c(rep(0,1),rep(1,n_scales-1))))
for(i in 2:n_scales){
  keep <- rbind(keep,t(as.vector(c(rep(0,i), rep(1,n_scales-i)))))
}
tab <- t(as.data.frame(interscale.matrix$corrected*keep))
key_scales$name <- paste0(key_scales$scale_no,". ",key_scales$scale_long)
row.names(tab) <- key_scales$scale_theory_long
tab <- as.data.frame(tab)
names(tab) <- seq(1,n_scales,1)
print(xtable(tab, digits=c(0,rep(3,13)), caption="Corrected Interscale Correlation Matrix"),
      include.rownames=TRUE, 
      file = "output/tables/interscale_correlations.txt")