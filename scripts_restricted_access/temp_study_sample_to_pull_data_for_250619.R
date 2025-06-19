# -- Read in Data ####
basic <- readRDS("data_restricted_access/processed/identified/1a_basic.Rds")
randassign <- readRDS("data_restricted_access/processed/identified/1b_randassign.Rds")
survey_population <- readRDS("data_restricted_access/processed/identified/2a_survey_population.Rds")
pcq <- readRDS("data_restricted_access/processed/identified/2a_pcq.Rds")
pcq <- as.data.frame(pcq)

# ================================================================= ####


# Unique IDS ####
unique_ids <- c(pcq$inmate_id,
                       randassign$inmate_id,
                       basic$inmate_id,
                       survey_population$inmate_id)
unique_ids <- tolower(unique_ids)
unique_ids <- unique(unique_ids)
unique_ids <- as.data.frame(unique_ids)
write_xlsx(unique_ids, "data_restricted_access/processed/identified/99_unique_ids_as_of_20250619.xlsx")

nrow(unique_ids)
length(unique(survey_population$inmate_id))
length(unique(pcq$inmate_id))
length(unique(basic$inmate_id))
length(unique(randassign$inmate_id))
