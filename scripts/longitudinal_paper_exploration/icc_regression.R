# Notes ####
# Data set-up ----
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")


pcq_mf <- readRDS("data/processed/pcq2_missforest.rds")
summary(lm(q21 ~ unit_type + race_black + violent_offense + high_school + as.factor(survey_wave) + STG, data = pcq_mf))
summary(lm(q94 ~ unit_type + race_black + violent_offense + high_school + as.factor(survey_wave) + STG, data = pcq_mf))

# Having trouble with the lme4 package, can't figure out how to fix it.
library(lme4)
library(performance)

# Null model (random intercept only)
model_null <- lmer(q10 ~ 1 + (1 | unit_type), data = pcq_mf)

# Calculate ICC
icc(model_null)

# Random intercept model with individual characteristics
model_conditional <- lmer(q10 ~ race_black + violent_offense + (1 | unit_type), data = your_data)

# Calculate conditional ICC
icc(model_conditional)
