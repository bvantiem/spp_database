# ================================================================= ####
# Notes to Script ####
# -- Objective ####

# -- Readme ####

# -- To do (delete if none) ####


# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions (delete if none) ####


# -- Read in Data ####
pcq <- readRDS("data/processed/processing_layer_3/pcq_masked_clean.Rds")
survey_population <- readRDS("data/processed/processing_layer_2/survey_population_masked.Rds")

# ================================================================= ####
# Table 