# ---------- Data set-up ---------- ----
rm(list=ls())

## Set Seed ----
set.seed(0630231106)

## Load data ----
load("data/processed/basic.Rda") #3248
load("data/processed/pcq.Rda")
load("data/processed/pcq2.Rda")
`%ni%` = Negate(`%in%`)

# Delete individuals for whom I have identical data from different datapulls
# This keeps data from the first available datapull
basic <- basic[-which(duplicated(basic[,-which(names(basic)=="date_datapull")])),] # Note that I have admin data from the second datapull for individuals who did not complete a survey in the second round.
length(unique(basic$research_id))

# Prepare basic for match with pcq. Match on id + survey wave
basic$survey_wave <- with(basic, ifelse(date_datapull == "2022-06-25", 1,
                                        ifelse(date_datapull == "2022-09-03", 1,
                                        ifelse(date_datapull == "2022-09-12", 2,
                                        ifelse(date_datapull == "2023-04-10", 3, NA)))))
basic$obs_id <- with(basic, paste0(research_id, "_",  survey_wave))

# Create obs_id variable in other dataframes
pcq$obs_id <- with(pcq, paste0(research_id, "_",survey_wave))
pcq2$obs_id <- with(pcq2, paste0(research_id, "_",survey_wave))
pcq_am$obs_id <- with(pcq_am, paste0(research_id, "_",survey_wave))

# Merge dataframes
pcq <- left_join(pcq, basic[,-which(names(basic) %in% c("research_id", "survey_wave"))], by="obs_id")
pcq2 <- left_join(pcq2, basic[,-which(names(basic) %in% c("research_id", "survey_wave"))], by="obs_id")
pcq_am <- left_join(pcq_am, basic[,-which(names(basic) %in% c("research_id", "survey_wave"))], by="obs_id")




