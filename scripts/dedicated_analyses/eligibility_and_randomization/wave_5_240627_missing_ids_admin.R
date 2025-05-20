# Missing admin data request to Jordan on 7/27/2023
research_participants <- readRDS("data/processed/research_participants.Rds")
basic <- readRDS("data/processed/processing_layer_1/basic.Rds")

ids_in_survey_or_population <- research_participants$research_id
ids_in_survey_or_population <- ids_in_survey_or_population[-grep("_na", ids_in_survey_or_population)]

i <- which(ids_in_survey_or_population %ni% basic$research_id)
missing_ids <- unmask_ids(ids_in_survey_or_population[i])

temp <- research_participants[which(research_participants$research_id %in% unique(paste0("rid_", missing_ids$research_id))),]
# missing_ids <- missing_ids[,c("original_id")]
write.xlsx(missing_ids, "data/processed/dedicated_analyses/240627_missing_ids_admin.xlsx")
