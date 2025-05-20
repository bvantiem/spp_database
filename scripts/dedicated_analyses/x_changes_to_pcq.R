# ---------- Data set-up ---------- ---- 
## Notes ####
# This script was built using only the factors as identified in the factor analysis 
## Set Seed ----
set.seed(1962)

## Libraries ----
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse)
library(xtable)
library(ggplot2)
library(tidyr) # Needed for splithalf/spearman brown analysis 
library(estimatr)
library(texreg)

#
rm(list=ls())
source("scripts/0_utils.R")

# Tabulate differences with original pcq, for prison climate scales ####
data <- with(pcq_lookup, pcq_lookup[which(pc_theory == "prison climate" &
                                            include_comparative_psych_analysis == "yes"),])
data$nature_of_change2 <- ifelse(data$nature_of_change_unit=="to unit","to unit",
                                 ifelse(data$nature_of_change_unit=="to institution","to institution","no change"))

table.changes <- data %>% group_by(scale_theory_long, nature_of_change2) %>% summarize(n=n()) %>%
  spread(key= nature_of_change2, n, fill=0)
table.changes <- as.data.frame(table.changes)
table.changes

data[which(data$nature_of_change_unit=="to institution"),]
