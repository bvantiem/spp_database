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

# -- Functions ####

# Read in data ####
respart <- readRDS("data/processed/de_identified/3_research_participants_masked.Rds")
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds") # Need to update release dates 

# Treatment compliance table ####
# -- Prepare data ####
treatment_compliance <- randassign %>%
  filter(rct == 1) %>%
  filter(rct_stratum %ni% c("lifer", "commuted death")) %>%
  mutate(onunit = ifelse(is.na(rct_release_dt), 1, 0)) 

# Percents ####
tab_percents <- treatment_compliance %>% 
  group_by(rct_treat_wave) %>%
  summarise(currently_on_unit = mean(onunit)*100,
            released = mean(rct_release_community)*100,
            transferred = mean(rct_release_transferred)*100,
            removed = mean(rct_release_removed)*100,
            refused_treatment = mean(rct_release_refused)*100) %>%
  mutate(currently_on_unit = paste0(format(currently_on_unit, digits = 2), "%"),
         released = paste0(format(released, digits = 2), "%"),
         removed = paste0(format(removed, digits = 2), "%"),
         transferred = paste0(format(transferred, digits = 2), "%"),
         refused_treatment = paste0(format(refused_treatment, digits = 2), "%")) %>%
  mutate(row_type = "percent")


# Numbers ####
on_unit_numbers <- treatment_compliance %>% 
  group_by(rct_treat_wave) %>%
  summarise(currently_on_unit = sum(onunit)) %>%
  mutate(currently_on_unit = as.character(round(currently_on_unit)))

tab_numbers <- treatment_compliance %>% 
  group_by(rct_treat_wave) %>%
  summarise(currently_on_unit = sum(onunit),
            released = sum(rct_release_community),
            transferred = sum(rct_release_transferred),
            removed = sum(rct_release_removed),
            refused_treatment = sum(rct_release_refused)) %>%
  mutate(currently_on_unit = as.character(round(currently_on_unit)),
         released = as.character(round(released)),
         removed = as.character(round(removed)),
         transferred = as.character(round(transferred)),
         refused_treatment = as.character(round(refused_treatment))) %>%
  mutate(row_type = "numbers")


# Combine dataframes
tab_combined <- bind_rows(tab_numbers, tab_percents) |>
  arrange(rct_treat_wave, row_type) %>%
  select(-row_type) %>%
  rename(treatment_wave = rct_treat_wave)

# Compute column sums and add as last row ####
totals <- tab_numbers |>
  summarise(
    treatment_wave = "Total",
    currently_on_unit = sum(as.numeric(currently_on_unit, na.rm = TRUE)),
    released = sum(as.numeric(released), na.rm = TRUE),
    removed = sum(as.numeric(removed), na.rm = TRUE),
    transferred = sum(as.numeric(transferred), na.rm = TRUE),
    refused_treatment = sum(as.numeric(refused_treatment), na.rm = TRUE)
  )

tab_combined_with_totals <- rbind(tab_combined, totals)
names(tab_combined_with_totals) <- str_to_title(gsub("_", " ", names(tab_combined_with_totals)))

# PDF table #### 
tab_pdf<- tab_combined_with_totals %>%
  kbl(caption = "Treatment Compliance",
      align = c("lrrrrr"),
      row.names = FALSE) %>%
  add_header_above(c(" " = 2, "Not on Unit" = 4)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  row_spec(c(14,15),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid")
  

tab_pdf

# Latex Table ####
tab_combined_with_totals %>%
  kbl(caption = "Treatment Compliance",
      align = c("lrrrrr"),
      row.names = FALSE,
      format = "latex", 
      booktabs = TRUE) %>%
  add_header_above(c(" " = 2, "Not on Unit" = 4)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  row_spec(c(14,15),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  save_kable(file = "output/tables/tabx_treatment_compliance.tex", self_contained = T)




