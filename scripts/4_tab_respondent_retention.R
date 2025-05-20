# Data set-up ----
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")

# Data ####
pcq <- readRDS("data/processed/processing_layer_2/pcq.Rds")
research_participants <- readRDS("data/processed/research_participants.Rds")

# Respondent Retention table ####
# This table displays the number of new respondents in each survey wave on the diagonal
# Columns to the left of the diagonal display the number of participants who continued to participate in subsequent waves

tab <- data.frame(wave1 = c(NA, NA, NA, NA, NA),
                  wave2 = c(NA, NA, NA, NA, NA),
                  wave3 = c(NA, NA, NA, NA, NA),
                  wave4 = c(NA, NA, NA, NA, NA),
                  wave5 = c(NA, NA, NA, NA, NA))
rownames(tab) <- c("wave1", "wave2", "wave3", "wave4", "wave5")

tab[1,1] <- with(research_participants, length(which(pcq_wave1==1)))
tab[1,2] <- with(research_participants, length(which(pcq_wave1==1 & (pcq_wave2==1|pcq_wave2_5==1))))
tab[1,3] <- with(research_participants, length(which(pcq_wave1==1 & (pcq_wave2==1|pcq_wave2_5==1) & pcq_wave3==1)))
tab[1,4] <- with(research_participants, length(which(pcq_wave1==1 & (pcq_wave2==1|pcq_wave2_5==1) & pcq_wave3==1 & pcq_wave4==1)))
tab[1,5] <- with(research_participants, length(which(pcq_wave1==1 & (pcq_wave2==1|pcq_wave2_5==1) & pcq_wave3==1 & pcq_wave4==1 & pcq_wave5==1)))
tab[2,2] <- with(research_participants, length(which(pcq_wave1==0 & (pcq_wave2==1|pcq_wave2_5==1))))
tab[2,3] <- with(research_participants, length(which(pcq_wave1==0 & (pcq_wave2==1|pcq_wave2_5==1) & pcq_wave3==1)))
tab[2,4] <- with(research_participants, length(which(pcq_wave1==0 & (pcq_wave2==1|pcq_wave2_5==1) & pcq_wave3==1 & pcq_wave4==1)))
tab[2,5] <- with(research_participants, length(which(pcq_wave1==0 & (pcq_wave2==1|pcq_wave2_5==1) & pcq_wave3==1 & pcq_wave4==1 & pcq_wave5==1)))
tab[3,3] <- with(research_participants, length(which(pcq_wave1==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==1)))
tab[3,4] <- with(research_participants, length(which(pcq_wave1==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==1 & pcq_wave4==1)))
tab[3,5] <- with(research_participants, length(which(pcq_wave1==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==1 & pcq_wave4==1 & pcq_wave5==1)))
tab[4,4] <- with(research_participants, length(which(pcq_wave1==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==0 & pcq_wave4==1)))
tab[4,5] <- with(research_participants, length(which(pcq_wave1==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==0 & pcq_wave4==1 & pcq_wave5==1)))
tab[5,5] <- with(research_participants, length(which(pcq_wave1==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==0 & pcq_wave4==0 & pcq_wave5==1)))
respondent_retention <- tab

# Population 'retention' table  & Response rates ####
# NOTE: these are not really reliable because we did survey pushes for which we did not gather new population data.
# This pushes up participation rates because new arrivals between the initial data collection and the survey push are not represented in population data
tab <- data.frame(wave1 = c(NA, NA, NA, NA, NA),
                  wave2 = c(NA, NA, NA, NA, NA),
                  wave3 = c(NA, NA, NA, NA, NA),
                  wave4 = c(NA, NA, NA, NA, NA),
                  wave5 = c(NA, NA, NA, NA, NA))
rownames(tab) <- c("wave1", "wave2", "wave3", "wave4", "wave5")

tab[1,1] <- with(research_participants, length(which((pcq_wave1==1|adm_wave1==1))))
tab[1,2] <- with(research_participants, length(which((pcq_wave1==1|adm_wave1==1) & (pcq_wave2==1|pcq_wave2_5==1|pcq_callsheets_wave2==1))))
tab[1,3] <- with(research_participants, length(which((pcq_wave1==1|adm_wave1==1) & (pcq_wave2==1|pcq_wave2_5==1|pcq_callsheets_wave2==1) & (pcq_wave3==1|pcq_callsheets_wave3==1))))
tab[1,4] <- with(research_participants, length(which((pcq_wave1==1|adm_wave1==1) & (pcq_wave2==1|pcq_wave2_5==1|pcq_callsheets_wave2==1) & (pcq_wave3==1|pcq_callsheets_wave3==1 & (pcq_wave4==1|pcq_callsheets_wave4==1)))))
tab[1,5] <- with(research_participants, length(which((pcq_wave1==1|adm_wave1==1) & (pcq_wave2==1|pcq_wave2_5==1|pcq_callsheets_wave2==1) & (pcq_wave3==1|pcq_callsheets_wave3==1) & (pcq_wave4==1|pcq_callsheets_wave4==1) & (pcq_wave5==1|pcq_callsheets_wave5==1))))
tab[2,2] <- with(research_participants, length(which(adm_wave1==0 & (pcq_wave2==1|pcq_wave2_5==1|pcq_callsheets_wave2==1))))
tab[2,3] <- with(research_participants, length(which(adm_wave1==0 & (pcq_wave2==1|pcq_wave2_5==1|pcq_callsheets_wave2==1) & (pcq_wave3==1|pcq_callsheets_wave3==1))))
tab[2,4] <- with(research_participants, length(which(adm_wave1==0 & (pcq_wave2==1|pcq_wave2_5==1|pcq_callsheets_wave2==1) & (pcq_wave3==1|pcq_callsheets_wave3==1) & (pcq_wave4==1|pcq_callsheets_wave4==1))))
tab[2,5] <- with(research_participants, length(which(adm_wave1==0 & (pcq_wave2==1|pcq_wave2_5==1|pcq_callsheets_wave2==1) & (pcq_wave3==1|pcq_callsheets_wave3==1 & (pcq_wave4==1|pcq_callsheets_wave4==1) & (pcq_wave5==1|pcq_callsheets_wave5==1)))))
tab[3,3] <- with(research_participants, length(which(adm_wave1==0 & pcq_wave1==0 & pcq_callsheets_wave2==0 & pcq_wave2==0 & pcq_wave2_5==0 & (pcq_wave3==1|pcq_callsheets_wave3==1))))
tab[3,4] <- with(research_participants, length(which(adm_wave1==0 & pcq_wave1==0 & pcq_callsheets_wave2==0 & pcq_wave2==0 & pcq_wave2_5==0 & (pcq_wave3==1|pcq_callsheets_wave3==1) & (pcq_wave4==1|pcq_callsheets_wave4==1))))
tab[3,5] <- with(research_participants, length(which(adm_wave1==0 & pcq_wave1==0 & pcq_callsheets_wave2==0 & pcq_wave2==0 & pcq_wave2_5==0 & (pcq_wave3==1|pcq_callsheets_wave3==1 & (pcq_wave4==1|pcq_callsheets_wave4==1) & (pcq_wave5==1|pcq_callsheets_wave5==1)))))
tab[4,4] <- with(research_participants, length(which(adm_wave1==0 & pcq_wave1==0 & pcq_callsheets_wave2==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==0 & pcq_callsheets_wave3==0 & (pcq_wave4==1|pcq_callsheets_wave4==1))))
tab[4,5] <- with(research_participants, length(which(adm_wave1==0 & pcq_wave1==0 & pcq_callsheets_wave2==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==0 & pcq_callsheets_wave3==0 & (pcq_wave4==1|pcq_callsheets_wave4==1) & (pcq_wave5==1|pcq_callsheets_wave5==1))))
tab[5,5] <- with(research_participants, length(which(adm_wave1==0 & pcq_wave1==0 & pcq_callsheets_wave2==0 & pcq_wave2==0 & pcq_wave2_5==0 & pcq_wave3==0 & pcq_callsheets_wave3==0 & pcq_wave4==0 & pcq_callsheets_wave4==0 & (pcq_wave5==1|pcq_callsheets_wave5==1))))

tab[1,1]+tab[2,2]+tab[3,3]+tab[4,4]+tab[5,5] # Unique incarcerated individuals
population_retention <- tab

# Response rates
round(respondent_retention/population_retention,2)
with(research_participants, length(which((pcq_wave3==1))))/with(research_participants, length(which((adm_wave3==1))))


