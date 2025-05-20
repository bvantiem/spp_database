# ---------- Data set-up ---------- ----
rm(list=ls())

## Set Seed ----
set.seed(1962)

# Source ####
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")
`%ni%` = Negate(`%in%`)

randassign <- readRDS("data/processed/randassign.Rds")
ca_roster <- xl.read.file("data/raw/4_random_assignment/assignment/CA Master Roster - Num ONLY 2023-10-23 for bvt.xlsx", xl.sheet=1, password="LS2023")

# Unmask randassign and subset to treated ####
unmasked_ids <- unmask_ids(randassign$research_id)
randassign$original_id <- unmasked_ids$original_id
randassign <- randassign %>% relocate(original_id, .before = research_id)
randassign <- randassign[which(randassign$treated==1),]

# Mask ca_roster ####
ca_roster$original_id <- tolower(ca_roster$original_id)
i <- unique(ca_roster$original_id)
id.link <- mask_ids(i) # Generate masked IDs
ca_roster <- left_join(ca_roster, id.link, by="original_id") # Merge basic on original ID

# Assess the status of things
ca_ids <- ca_roster$original_id[which(ca_roster$original_id %in% randassign$original_id)]
ca_rogue_ids <- ca_roster$original_id[which(ca_roster$original_id %ni% randassign$original_id)]


cat("Based on the CA roster, there are", nrow(ca_roster), "filled spaces, and", 64-nrow(ca_roster), "empty spaces")
cat("Of the", nrow(ca_roster), "filled spaces, our data indicates that", length(ca_ids), "were indeed treated.") # I can see from the data I have that this individual is a lifer.

randassign$ca_ids <- 0
randassign$ca_ids[which(randassign$original_id %in% ca_ids)] <- 1

randassign$assumed_released_since_rand3 <- 0
randassign$assumed_released_since_rand3[which(randassign$release_date>=ymd(20230519) & randassign$treated==1 & randassign$ca_ids==0)] <- 1
ids_assumed_released_since_rand3 <- randassign$original_id[which(randassign$assumed_released_since_rand3==1)]
ids_assumed_not_released <- randassign$original_id[which(randassign$assumed_released_since_rand3==0 & is.na(randassign$release_type))]

with(randassign[which(randassign$assumed_released_since_rand3==1),], table(stratum))

cat("Data suggests that", nrow(randassign), "individuals have been assigned to CA. Of those individuals,", length(which(!is.na(randassign$release_type))),"were previously paroled, transferred to another prison, removed from the unit, or never spent time on CA. Our data suggests that another,",length(ids_assumed_released_since_rand3), "are not currently on CA, suggesting they have been released and need to be replaced. Data suggests that", length(ids_assumed_not_released), "are currently on CA. This is", 64-(length(ids_assumed_released_since_rand3)+length(ids_assumed_not_released)), "too few people for a unit with 64 beds.")
with(randassign[which(randassign$assumed_released_since_rand3==1),], table(stratum))
nrow(randassign[which(randassign$assumed_released_since_rand3==1),])

randassign <- randassign[,-which(names(randassign)=="research_id")]

write_xlsx(randassign, "data/raw/4_random_assignment/assignment/temp/231030_randomization_prep.xlsx")
