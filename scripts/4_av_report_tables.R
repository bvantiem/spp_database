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

# Generate basic table with means ####
# prepare_table <- function(data){
#   # Note: numbering the rows because tidyr::gather puts variables in alphabetical order
#   tab <- data %>%
#     transmute(
#       # Sentence Characteristics
#       minimum_sentence = release_type_removed,
#       maximum_sentence = sent_max_cort_days/365)
#   # life = life)
#   
#   tab <- data.balance %>%
#     transmute(
#       # Sentence Characteristics
#       r01a_minimum_sentence = sent_min_cort_days/365,
#       r01b_maximum_sentence = sent_max_cort_days/365,
#       # r01c_life = life,
#       # Offense Characteristics
#       r02a_violent_offense = violent_offense,
#       r02b_property_offense = property_offense,
#       r02c_drugs_offense = drugs_offense,
#       r02d_publicorder_offense = publicorder_offense,
#       # Demographics
#       r03a_black = race_black,
#       r03b_white = race_white,
#       # r03c_age_at_treatment = age_at_treatment,
#       r03d_married = married,
#       r03e_high_school_degree = high_school)
#   #r03e_security_threat_group = STG)
#   
#   tab <- tab %>%
#     tidyr::gather(variable, value) %>%
#     # Summarize by variable
#     group_by(variable) %>%
#     # summarise all columns
#     summarise(`Mean` = round(mean(value, na.rm=TRUE),3)) %>% # Check number of NA values after getting full data.balance
#     mutate(Mean = format(Mean, digits = 3))
#   
#   tab_2 <- data.balance %>%
#     select(
#       r99a_N = research_id) %>%
#     tidyr::gather(variable, value) %>%
#     # Summarize by variable
#     group_by(variable) %>%
#     # summarise all columns
#     summarise(`Mean` = length(unique(value)))
#   
#   tab <- rbind(tab,
#                tab_2)
#   
#   variable_names <- gsub("(....)(_)(.*)", "\\3", tab$variable)
#   variable_names <- str_to_title(gsub("_", " ", variable_names))
#   variable_names[which(variable_names=="Publicorder Offense")] <- "Public Order Offense"
#   tab$variable <- variable_names
#   names(tab) <- c("Variable", "Share/Mean")
#   rownames(tab) <- NULL
#   return(tab)
# }


# Read in data ####
respart <- readRDS("data/processed/de_identified/3_research_participants_masked.Rds")
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds") # Need to update release dates 

# Treatment compliance table ####
# -- Prepare data ####
treatment_compliance <- randassign %>%
  filter(rct == 1) %>%
  filter(rct_stratum != "lifer") %>%
  mutate(onunit = ifelse(is.na(rct_release_dt), 1, 0)) 

# Percents ####
on_unit <- treatment_compliance %>% 
  group_by(rct_treat_wave) %>%
  summarise(currently_on_the_lsu = mean(onunit)*100)

released <- treatment_compliance %>% 
  filter(onunit==0) %>%
  group_by(rct_treat_wave) %>%
  summarise(released = mean(rct_release_community)*100,
            removed = mean(rct_release_removed)*100,
            transferred = mean(rct_release_transferred)*100,
            refused_treatment = mean(rct_release_refused)*100)

tab_percents <- left_join(on_unit, released)
tab_percents <- tab_percents %>%
  mutate(row_type = "percent")
tab_percents

# Numbers ####
on_unit_numbers <- treatment_compliance %>% 
  group_by(rct_treat_wave) %>%
  summarise(currently_on_the_lsu = sum(onunit))

released_numbers <- treatment_compliance %>% 
  filter(onunit==0) %>%
  group_by(rct_treat_wave) %>%
  summarise(released = sum(rct_release_community),
            removed = sum(rct_release_removed),
            transferred = sum(rct_release_transferred),
            refused_treatment = sum(rct_release_refused))

tab_numbers <- left_join(on_unit_numbers, released_numbers)
tab_numbers <- tab_numbers %>%
  mutate(row_type = "numbers")
tab_numbers

# Combine dataframes
tab_combined <- bind_rows(tab_numbers, tab_percents) |>
  arrange(rct_treat_wave, row_type) %>%
  select(-row_type) %>%
  rename(treatment_wave = rct_treat_wave)

# Compute column sums and add as last row ####
totals <- tab_numbers |>
  summarise(
    treatment_wave = "Total",
    currently_on_the_lsu = sum(currently_on_the_lsu, na.rm = TRUE),
    released = sum(released, na.rm = TRUE),
    removed = sum(removed, na.rm = TRUE),
    transferred = sum(transferred, na.rm = TRUE),
    refused_treatment = sum(refused_treatment, na.rm = TRUE)
  )

tab_combined_with_totals <- rbind(tab_combined, totals)

# Latex table #### o
tab_latex <- tab_combined_with_totals %>%
  kbl(caption = "Treatment Compliance",
      align = c("lrrrrr"),
      row.names = FALSE) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  row_spec(c(14,15),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") 

# -- Save ####
# tab_latex <- save_kable(file = "output/tables/tabx_balance_table_waves123456.pdf", self_contained = T,density = 200)


