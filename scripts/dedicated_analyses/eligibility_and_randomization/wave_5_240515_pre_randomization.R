# ---------- Data set-up ---------- ----
rm(list=ls())

## Set Seed ----
set.seed(1962)

# Source ####
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")
`%ni%` = Negate(`%in%`)

randassign <- readRDS("data/processed/randassign.Rds")
stopifnot(length(unique(randassign$research_id))==nrow(randassign))

caroster <- randassign[which(randassign$treated==1 & is.na(randassign$release_date) & is.na(randassign$release_type)),]
forjordan <- randassign[which(randassign$treated==1),c("research_id", "treatment_date", "release_type", "release_date")]

unmasked_ids <- unmask_ids(forjordan$research_id)
forjordan$original_id <- unmasked_ids$original_id
forjordan <- forjordan[,c("original_id", "treatment_date", "release_type", "release_date")]
write_xlsx(forjordan, "data/raw/4_random_assignment/release_dates/temp/240515_release_dates.xlsx")
