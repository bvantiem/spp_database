# Missing admin data request to Jordan on 7/27/2023
ids_in_survey_or_population <- unique(c(pcq$research_id,
                                        randassign$research_id,
                                        wave2_callsheets$research_id,
                                        wave3_callsheets$research_id,
                                        pcqwave2_5$research_id))
ids_in_survey_or_population <- ids_in_survey_or_population[-grep("_na", ids_in_survey_or_population)]

i <- which(ids_in_survey_or_population %ni% basic$research_id)
missing_ids <- unmask_ids(ids_in_survey_or_population[i])

write.xlsx(missing_ids, "data/processed/dedicated_analyses/230727_missing_ids_admin.xlsx")
