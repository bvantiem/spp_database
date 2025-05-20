######### ---------- Data set-up ---------- ---- 
## Notes ####
# This script was built using only the factors as identified in the factor analysis 
## Set Seed ----
set.seed(1962)

# Set surveywave ####
wave <- 1
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
library(tools)
library("excel.link")


# Example code from splithalf
# https://cran.r-project.org/web/packages/splithalf/readme/README.html
`%ni%` = Negate(`%in%`)

## Load data ----
rm(list=ls())
source("scripts/0_utils.R")
load("data/processed/pcq.Rda")
load("data/processed/pcq2.Rda")
load("data/processed/basic.Rda")

## Add variables ####
# unit_type
pcq$unit_type <- with(pcq, ifelse(unit %in% c("aa", "ab", "ac", "ad", "eb", "da", "db"), "gp",
                                  ifelse(unit %in% c("bb", "bc", "bd"), "tc",
                                         ifelse(unit=="ba", "rec",
                                                ifelse(unit=="ca", "ls",
                                                       ifelse(unit=="cb", "hons",
                                                              ifelse(unit=="ea", "thu",
                                                                     ifelse(unit=="rhu", "rhu",
                                                                            ifelse(unit=="inf", "inf",NA)))))))))
pcq$unit_type <- as.factor(pcq$unit_type)

pcq2$unit_type <- with(pcq2, ifelse(unit %in% c("aa", "ab", "ac", "ad", "eb", "da", "db"), "gp",
                                    ifelse(unit %in% c("bb", "bc", "bd"), "tc",
                                           ifelse(unit=="ba", "rec",
                                                  ifelse(unit=="ca", "ls",
                                                         ifelse(unit=="cb", "hons",
                                                                ifelse(unit=="ea", "thu",
                                                                       ifelse(unit=="rhu", "rhu",
                                                                              ifelse(unit=="inf", "inf",NA)))))))))
pcq2$unit_type <- as.factor(pcq2$unit_type)

pcq_lookup$scale_id <- NA # Code generalizability - just keep it!

# issues to fix  ####
# Because this has just 1 individual in there, generates issues with regressions 
pcq <- pcq[pcq$unit_type!="inf",]
pcq2 <- pcq2[pcq2$unit_type!="inf",]

# Subset data ####
wave <- 1
pcq <- pcq[which(pcq$survey_wave==wave),]
pcq2 <- pcq2[which(pcq2$survey_wave==wave),]
## Variables to add/fix  ####
# Delete individuals who completed the survey for a second time within wave one.
if(wave==1){
  pcq$survey_no <- 1
  pcq2$survey_no <- 1
  temp <- data.frame(table(pcq$research_id))
  index <- temp[temp$Freq==2,"Var1"]
  pcq[pcq$research_id %in% index & pcq$block!="a","survey_no"] <- 2 # all these individuals moved from an a block to another unit, where they completed a second survey
  pcq2[pcq2$research_id %in% index & pcq2$block!="a","survey_no"] <- 2
  pcq <- pcq[pcq$survey_no==1,] # Drops the second survey of these 3 individuals.
  pcq2 <- pcq2[pcq2$survey_no==1,]
  
  # delete anonymous ids 
  index <- which(pcq$research_id==99999|pcq$unit_type=="ls")
  pcq <- pcq[-index,]
  pcq2 <- pcq2[-index,]
}else{}

## Variables to add/fix - Public Health ####
pcq2[which(pcq2$q147==2),"q147"] <- 0
pcq2[which(pcq2$q148==2),"q148"] <- 0


# Subset for public health variables ####
ph_lookup_all <- pcq_lookup[pcq_lookup$scale_theory %in% c("safety", "conduct", "violence", "sleep", "health", "care", "discrimination"),]
ph_lookup_all <- ph_lookup_all[ph_lookup_all$question_no_pa_2022a %ni% c(33,34,40,41),]

######### ---------- Tables ---------- ---- 
# Original question set ####
options(xtable.comment = FALSE)

# Create as list
questions.list <- list(conduct = c(48:54), 
                       victimization = c(43:47),
                       sleep = c(94:96),
                       overall_care = c(111:112),
                       physical_health = c(113:114),
                       service_use_frequency = c(115:118),
                       service_use_satisfaction = c(119:122),
                       discrimination = c(147:148))

for(i in 1:length(questions.list)){ 
  print(i)
  questions <- questions.list[[i]]
  # Subset data 
  question_ch <- ph_lookup_all[ph_lookup_all$question_no_pa_2022a %in% questions,]$question_pa_2022a
  ph_lookup <- ph_lookup_all[ph_lookup_all$question_no_pa_2022a %in% questions,]
  qnos <- ph_lookup$question_qno
  pcq_temp <- pcq2[,c("research_id", "block", "unit_type", qnos)]
  if(names(questions.list)[i] %ni% c("sleep", "physical_health", "overall_care", "service_use_satisfaction")){
    pcq_temp[,qnos] <- ifelse(pcq_temp[,qnos]==1, 0, 1)
  }
  
  # Aggregate results
  df <- pcq_temp[,qnos]
  results <- describe(df)
  results.aggregate <- data.frame(Question = question_ch,
                                  Mean = results$mean,
                                  Respondents = results$n,
                                  SD = results$sd,
                                  Min = results$min,
                                  Max = results$max)
  
  caption.name <- toTitleCase(gsub("_"," ",names(questions.list)[i]))
  file.name <- "output/tables/public_health/public_health_tables.txt"
  print(xtable(results.aggregate, 
               digits = c(0,0,2,0,2,0,0), 
               caption = caption.name,
               caption.placement="top",
               latex.environments = "flushleft"),
        file = file.name, 
        append=TRUE)
  
  # Results by unit
  pcq_temp <- as.data.frame(pcq_temp)
  df <- pcq_temp[,c("unit_type",qnos)]
  results <- describeBy(df ~ unit_type)
  results <- data.frame(question = c(question_ch,"respondents"),
                        gp = with(results, c(gp$mean[-1], gp$n[1])),
                        tc = with(results, c(tc$mean[-1], tc$n[1])),
                        rec = with(results, c(rec$mean[-1], rec$n[1])),
                        hons = with(results, c(hons$mean[-1], hons$n[1])),
                        thu = with(results, c(thu$mean[-1], thu$n[1])),
                        rhu = with(results, c(rhu$mean[-1], rhu$n[1])))
  rm(pcq_temp, df, qnos, ph_lookup, question_ch)
  print(xtable(results, 
               digits=c(0,0,2,2,2,2,2,2),
               caption=caption.name,
               caption.placement="top",
               timestamp=NULL,
               latex.environments = "flushleft"), 
        file = file.name,
        append=TRUE)
}

##### Unit Specific Response Rates Table ####
df <- xl.read.file("data/raw/3_surveys/participation_lists/pcq_wave1_participation.xlsx", password = "chester")

df$unit <- tolower(df$unit)
df$unit_type <- with(df, ifelse(unit %in% c("aa", "ab", "ac", "ad", "eb", "da", "db"), "gp",
                                ifelse(unit %in% c("bb", "bc", "bd"), "tc",
                                       ifelse(unit=="ba", "rec",
                                              ifelse(unit=="ca", "ls",
                                                     ifelse(unit=="cb", "hons",
                                                            ifelse(unit=="ea", "thu",
                                                                   ifelse(unit=="fa", "rhu", # ASSUMING this is the RHU
                                                                          ifelse(unit=="inf", "inf",NA)))))))))

rr.table <- as.data.frame(table(df$unit_type))
rr.table <- rr.table[-which(rr.table$Var1 %in% c("inf", "ls")),]
rr.table2 <- as.data.frame(table(pcq2$unit_type))
rr.table2 <- rr.table2[-which(rr.table2$Var1 %in% c("inf", "ls")),]
rr.table <- left_join(rr.table, rr.table2, by="Var1")
names(rr.table) <- c("Unit Type", "Population", "Surveys")
rr.table$`Response Rate` <- rr.table$Surveys/rr.table$Population
for(i in 2:4){
  rr.table[,i] <- as.numeric(rr.table[,i])
}
rr.table[,1] <- as.character(rr.table[,1])
rr.table[nrow(rr.table)+1,] <- c("all",sum(rr.table$Population), sum(rr.table$Surveys), sum(rr.table$Surveys)/sum(rr.table$Population))
for(i in 2:4){
  rr.table[,i] <- as.numeric(rr.table[,i])
}
print(xtable(rr.table, 
             digits = c(0,0,0,0,2),
             caption = "Response Rates Wave 1",
             timestemp = NULL,
             latex.environments = "flushleft"),
      file = "output/tables/public_health/response_rates_wave1.txt")


# New question set ####
options(xtable.comment = FALSE)

# Create as list
questions.list <- list(physical_health_sleep_and_exercise = c(55,56,58),
                       mental_health_and_stress = c(123:130),
                       personal_relationships_and_support_in_prison = c(10,14,15,18,21),
                       support_system = c(131,132),
                       contact_with_the_outside = c(133, 134, 138),
                       safety_and_discrimination = c(26,27,28,30,31,27,35,36,37,42))
                       

for(i in 1:length(questions.list)){ 
  print(i)
  questions <- questions.list[[i]]
  # Subset data 
  question_ch <- pcq_lookup[pcq_lookup$question_no_pa_2022a %in% questions,]$question_pa_2022a
  ph_lookup <- pcq_lookup[pcq_lookup$question_no_pa_2022a %in% questions,]
  qnos <- ph_lookup$question_qno
  pcq_temp <- pcq2[,c("research_id", "block", "unit_type", qnos)]
  if(names(questions.list)[i] %ni% c("personal_relationships_and_support_in_prison",
                                     "support_system",
                                     "safety_and_discrimination")){
    pcq_temp[,qnos] <- ifelse(pcq_temp[,qnos]==1, 0, 1)
  }
  
  # Aggregate results
  df <- pcq_temp[,qnos]
  results <- describe(df)
  results.aggregate <- data.frame(Question = question_ch,
                                  Mean = results$mean,
                                  Respondents = results$n,
                                  SD = results$sd,
                                  Min = results$min,
                                  Max = results$max)
  
  caption.name <- toTitleCase(gsub("_"," ",names(questions.list)[i]))
  file.name <- "output/tables/public_health/public_health_tables_extra_qs.txt"
  print(xtable(results.aggregate, 
               digits = c(0,0,2,0,2,0,0), 
               caption = caption.name,
               caption.placement="top",
               latex.environments = "flushleft"),
        file = file.name, 
        append=TRUE)
  
  # Results by unit
  pcq_temp <- as.data.frame(pcq_temp)
  df <- pcq_temp[,c("unit_type",qnos)]
  results <- describeBy(df ~ unit_type)
  results <- data.frame(question = c(question_ch,"respondents"),
                        gp = with(results, c(gp$mean[-1], gp$n[1])),
                        tc = with(results, c(tc$mean[-1], tc$n[1])),
                        rec = with(results, c(rec$mean[-1], rec$n[1])),
                        hons = with(results, c(hons$mean[-1], hons$n[1])),
                        thu = with(results, c(thu$mean[-1], thu$n[1])),
                        rhu = with(results, c(rhu$mean[-1], rhu$n[1])))
  rm(pcq_temp, df, qnos, ph_lookup, question_ch)
  print(xtable(results, 
               digits=c(0,0,2,2,2,2,2,2),
               caption=caption.name,
               caption.placement="top",
               timestamp=NULL,
               latex.environments = "flushleft"), 
        file = file.name, 
        append=TRUE)
}

##### Unit Specific Response Rates Table ####
library("excel.link")
df <- xl.read.file("data/raw/3_surveys/participation_lists/pcq_wave1_participation.xlsx", password = "chester")

df$unit <- tolower(df$unit)
df$unit_type <- with(df, ifelse(unit %in% c("aa", "ab", "ac", "ad", "eb", "da", "db"), "gp",
                                ifelse(unit %in% c("bb", "bc", "bd"), "tc",
                                       ifelse(unit=="ba", "rec",
                                              ifelse(unit=="ca", "ls",
                                                     ifelse(unit=="cb", "hons",
                                                            ifelse(unit=="ea", "thu",
                                                                   ifelse(unit=="fa", "rhu", # ASSUMING this is the RHU
                                                                          ifelse(unit=="inf", "inf",NA)))))))))

rr.table <- as.data.frame(table(df$unit_type))
rr.table <- rr.table[-which(rr.table$Var1 %in% c("inf", "ls")),]
rr.table2 <- as.data.frame(table(pcq2$unit_type))
rr.table2 <- rr.table2[-which(rr.table2$Var1 %in% c("inf", "ls")),]
rr.table <- left_join(rr.table, rr.table2, by="Var1")
names(rr.table) <- c("Unit Type", "Population", "Surveys")
rr.table$`Response Rate` <- rr.table$Surveys/rr.table$Population
for(i in 2:4){
  rr.table[,i] <- as.numeric(rr.table[,i])
}
rr.table[,1] <- as.character(rr.table[,1])
rr.table[nrow(rr.table)+1,] <- c("all",sum(rr.table$Population), sum(rr.table$Surveys), sum(rr.table$Surveys)/sum(rr.table$Population))
for(i in 2:4){
  rr.table[,i] <- as.numeric(rr.table[,i])
}
print(xtable(rr.table, 
             digits = c(0,0,0,0,2),
             caption = "Response Rates Wave 1",
             timestemp = NULL,
             latex.environments = "flushleft"),
      file = "output/tables/public_health/response_rates_wave1.txt")
