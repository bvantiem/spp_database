# ---------- Data set-up ---------- ----
rm(list=ls())

## Set Seed ----
set.seed(1962)

# Source ####
source("scripts/00_packages.R")
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")

randassign <- readRDS("data/processed/randassign.Rds")
stopifnot(length(unique(randassign$research_id))==nrow(randassign))

caroster <- randassign[which(randassign$treated==1 & is.na(randassign$release_date) & is.na(randassign$release_type)),]
carelease <- randassign[which(randassign$treated==1),c("research_id", "treatment_date", "release_type", "release_date")]

unmasked_ids <- unmask_ids(carelease$research_id)
carelease$original_id <- unmasked_ids$original_id
carelease <- carelease[,c("original_id", "treatment_date", "release_type", "release_date")]
write_xlsx(carelease, "data/raw/4_random_assignment/release_dates/temp/241015_release_dates.xlsx")
write_xlsx(randassign, "data/raw/4_random_assignment/release_dates/temp/241015_random_assignment.xlsx")

# Check on individuals whose treatment status is contested
carelease[which(carelease$original_id %in% c("bj0536", "md0142", "qd7527", "qm2808", "ay7639", "ba3244")),]
randassign$original_id <- unmask_ids(randassign$research_id)$original_id
randassign[which(randassign$original_id %in% c("bj0536", "md0142", "qd7527", "qm2808", "ay7639", "ba3244")),]
randassign[which(randassign$original_id %in% c("ql6173")),]
with(randassign[which(randassign$treatment_wave==4),], table(randassign$treated, randassign$stratum))
