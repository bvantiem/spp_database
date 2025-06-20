# Function ####
# Read in data ####
respart <- readRDS("data/processed/de_identified/3_research_participants_masked.Rds")
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds") # Need to update release dates 

# Treatment compliance table ####
# -- Prepare data ####
treatment_compliance <- randassign %>%
  filter(treated == 1) %>%
  filter(stratum != "lifer") %>%
  mutate(onunit = ifelse(is.na(release_from_unit_date), 1, 0)) 
# -- Prepare table ####
tab <- cbind(prepare_summary_column(treatment_compliance),
             prepare_summary_column(treatment_compliance %>% filter(!is.na(release_from_unit_date))),
             prepare_summary_column(treatment_compliance %>% filter(is.na(release_from_unit_date)))) 

 # setNames(c("Variable","All","LSU","Control"))

tab
