# ---------- Data set-up ---------- ---- 
rm(list=ls())

## Set Seed ----
set.seed(1962)

# Source ####
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")

library(readxl)
library(dplyr)
library("excel.link")
source("scripts/0_id_masking_function.R")
`%ni%` = Negate(`%in%`)

# Prepare data ####
round3 <- xl.read.file("data/raw/4_random_assignment/20230424_eligibility_round3.xlsx", xl.sheet=1, password="LS2023")
load("data/processed/randassign.Rda")

# How many empty spaces to fill?
# There are 49 filled spaces and 15 empty spaces in the 64-bed unit. The 15 empty spaces include the two people who are due to be released next week (so there are currently 13 empty spaces.)
temp <- randassign[which(is.na(randassign$release_type) & randassign$treated==1),]
unmasked_ids <- unmask_ids(temp$research_id)
temp$id <- unmasked_ids$original_id
write.csv(temp[,c("id", "treated", "stratum", "treatment_wave")], "data/temp/individuals_on_ca_as_of_230512.csv")


# Since the unit started, 27 individuals have left from the unit (including people who were moved to other prisons or units). Of those, there was one lifer, 14 individuals who were treated in wave 1 and 12 individuals who were treated in wave 2. 
temp.released <- randassign[which(!is.na(randassign$release_type) & randassign$treated==1),]
table(temp.released$treatment_wave, temp.released$stratum)

# Of the 15 individuals who were released last (not counting Bowman, who has already been replaced) 11 were from the 0-6 month stratum and 4 were from the 6-12 month to min stratum. Therefore, we should randomly select 33 individuals (11 treated, 22 controls) from the 0-6 month stratum and 12 (4 treated, 8 controls) from the 6-12 month to min stratum. 
temp.released %>% filter(stratum!="lifer") %>% arrange(release_date) %>% select(stratum) %>% head(n=14) %>% table()
temp.released %>% filter(stratum!="lifer") %>% arrange(release_date) %>% select(stratum) %>% head(n=16) %>% table()

# For the avoidance of doubt, the people who chose not to stay / were removed from the unit will continue to count as 'treated' in our analyses. We will however fill the empty spaces. 
