# ================================================================= ####
# Notes to Script ####
# -- Objective ####

# -- Readme ####

# -- To do (delete if none) ####
# Jonas: to make descriptive tables for the sample
# unassigned: fix proportion table that filled out survey in wave j conditional on still being in the prison.
# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions (delete if none) ####


# -- Read in Data ####
pcq <- readRDS("data/processed/de_identified/3_pcq_cleaned.Rds")
survey_population <- readRDS("data/processed/de_identified/2b_survey_population_masked.Rds")
research_participants <- readRDS("data/processed/de_identified/3_research_participants_masked.Rds")
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds")
# ================================================================= ####
# Survey Population by Wave ####
survey_population_by_wave <- survey_population %>%
  count(survey_wave, name = "survey_population") %>%
  rename(wave = survey_wave)

pcq <- pcq %>%
  left_join(survey_population_by_wave,
            by = "wave", 
            relationship = "many-to-one") 

# PCQ Survey Data Overview ####
pcq <- pcq %>%
  group_by(research_id) %>%
  mutate(survey_first = min(wave), .groups = "drop") %>%
  ungroup() %>%
  mutate(survey_repeat = ifelse(wave > survey_first, 1, 0)) %>%
  relocate(survey_first, survey_repeat, .after = n_wave_max)

pcq_surveys <- pcq %>%
  group_by(wave) %>%
  summarise(
    surveys = n(),
    respondents = n_distinct(research_id),
    repeat_respondents = n_distinct(research_id[survey_repeat == 1]),
    survey_population = unique(survey_population)
  ) %>% 
  mutate(wave = as.integer(wave)) %>%
  ungroup() %>%
  mutate(response_rate = round((respondents / survey_population) * 100, 1)) %>%
  mutate(cumsum_surveys = cumsum(surveys))

# Add number of unique respondents and number unique repeat respondents 
cumsum_unique_respondents <- pcq %>%
  summarise(
    wave1 = n_distinct(research_id[wave %in% c(1)]),
    wave2 = n_distinct(research_id[wave %in% c(1:2)]),
    wave3 = n_distinct(research_id[wave %in% c(1:3)]),
    wave4 = n_distinct(research_id[wave %in% c(1:4)]),
    wave5 = n_distinct(research_id[wave %in% c(1:5)]),
    wave6 = n_distinct(research_id[wave %in% c(1:6)]),
    wave7 = n_distinct(research_id[wave %in% c(1:7)])
  ) %>%
  t() %>%
  as.data.frame() %>%
  setNames("cumsum_unique_respondents")

cumsum_unique_repeat_respondents <- pcq %>%
  summarise(
    wave1 = n_distinct(research_id[wave %in% c(1) & survey_repeat == 1]),
    wave2 = n_distinct(research_id[wave %in% c(1:2) & survey_repeat == 1]),
    wave3 = n_distinct(research_id[wave %in% c(1:3) & survey_repeat == 1]),
    wave4 = n_distinct(research_id[wave %in% c(1:4) & survey_repeat == 1]),
    wave5 = n_distinct(research_id[wave %in% c(1:5) & survey_repeat == 1]),
    wave6 = n_distinct(research_id[wave %in% c(1:6) & survey_repeat == 1]),
    wave7 = n_distinct(research_id[wave %in% c(1:7) & survey_repeat == 1])
  ) %>%
  t() %>%
  as.data.frame() %>%
  setNames("cumsum_unique_repeat_respondents")

pcq_surveys$cumsum_respondents <- cumsum_unique_respondents$cumsum_unique_respondents
pcq_surveys$cumsum_repeat_respondents <- cumsum_unique_repeat_respondents$cumsum_unique_repeat_respondents

## Fix ordering of columns 
pcq_surveys <- pcq_surveys %>% 
  select(wave,
         response_rate,
         surveys,
         respondents,
         repeat_respondents,
         cumsum_surveys,
         cumsum_respondents,
         cumsum_repeat_respondents)


names(pcq_surveys) <- c(
  "Wave",
  "Response rate",
  "Surveys",
  "Unique Respondents",
  "Unique Repeat Respondents",
  "Surveys",
  "Unique Respondents",
  "Unique Repeat Respondents")

# Table ####
file_name <- "tabx_pcq_survey_data_overview"
pcq_surveys %>%
  kbl(caption = "PCQ Survey Data Overview",
      align = c("lrrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 2, "By Wave" = 3, "Cumulative, Including All Prior Waves" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  # column_spec(7, border_left=T, extra_css = "border-left: 1px solid black;")  %>% 
  row_spec(c(nrow(pcq_surveys)),
           hline_after=FALSE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("This table describes the survey data collected in the Scandinavian Prison Project. Repeat respondents are defined as respondents who have completed more than one survey.")) %>% 
  landscape() %>%
  # kable_styling(latex_options = c("scale_down")) %>% 
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)


# PCQ Survey Data for RCT Sample ####
# -- Data prep ####
randassign <- randassign %>%
  filter(rct_stratum %ni% c("lifer", "commuted death"),
         rct_treat_wave != max(rct_treat_wave)) # Always drop the last wave as we can't have pre and post data for this group

pcq_with_cohort <- pcq %>%
  left_join(randassign,
            by = c("research_id"),
            relationship = "many-to-one") 

pcq_rct_overview <- pcq %>%
  left_join(randassign,
            by = "research_id",
            relationship = "many-to-one") %>%
  filter(!is.na(rct),
         rct_stratum %ni% c("lifer", "commuted death")) %>%
  select(research_id, 
         wave,
         rct,
         rct_stratum,
         rct_treat_wave) %>%
  mutate(survey_pre = ifelse(wave == rct_treat_wave, 1, 0),
         survey_post = ifelse(wave == rct_treat_wave+1, 1, 0),
         survey_pre_expanded = ifelse(wave <= rct_treat_wave, 1, 0),
         survey_post_expanded = ifelse(wave > rct_treat_wave, 1, 0)) %>%
  group_by(research_id) %>%
  mutate(survey_pre = max(survey_pre),
         survey_post = max(survey_post),
         survey_pre_expanded = max(survey_pre_expanded),
         survey_post_expanded = max(survey_post_expanded)) %>%
  mutate(surveys_pre_and_post = min(survey_pre, survey_post),
         surveys_pre_and_post_expanded = min(survey_pre_expanded, survey_post_expanded)) %>%
  distinct(research_id, rct, rct_treat_wave, surveys_pre_and_post, surveys_pre_and_post_expanded, rct_stratum) %>%
  ungroup() 

# -- By cohort ####
pcq_rct_overview_by_cohort <- pcq_rct_overview %>%
  group_by(rct_treat_wave, rct) %>%
  summarize(surveys_pre_and_post = sum(surveys_pre_and_post),
            surveys_pre_and_post_expanded = sum(surveys_pre_and_post_expanded)) %>%
  ungroup() %>%
  left_join(randassign %>%
              group_by(rct, rct_treat_wave) %>%
              summarize(cohort_size = n()),
            by = c("rct", "rct_treat_wave"),
            relationship = "many-to-one") %>%
  relocate(cohort_size, .after = rct_treat_wave) %>%
  relocate(rct)

pcq_rct_overview_by_cohort

pcq_rct_overview_overall <- pcq_rct_overview %>%
  group_by(rct) %>%
  summarize(rct_treat_wave = 99,
    surveys_pre_and_post = sum(surveys_pre_and_post),
            surveys_pre_and_post_expanded = sum(surveys_pre_and_post_expanded)) %>%
  ungroup() %>%
  left_join(randassign %>%
              group_by(rct) %>%
              summarize(cohort_size = n()),
            by = c("rct"),
            relationship = "many-to-one") %>%
  relocate(cohort_size, .after = rct_treat_wave) %>%
  relocate(rct)

pcq_rct_complete <- rbind(pcq_rct_overview_by_cohort, pcq_rct_overview_overall)

pcq_rct_overview_side_by_side <- cbind(pcq_rct_complete %>%
                            filter(rct==1) %>%
                            select(rct_treat_wave, cohort_size, surveys_pre_and_post, surveys_pre_and_post_expanded),
                            pcq_rct_complete %>%
                            filter(rct==0) %>%
                            select(cohort_size, surveys_pre_and_post, surveys_pre_and_post_expanded) %>%
                            rename_with(~ paste0(., "_control"), .cols = everything())) # To avoid duplicate names 

pcq_rct_overview_prop <- pcq_rct_overview_side_by_side %>%
  mutate(pct_surveys_pre_and_post = round(surveys_pre_and_post/cohort_size*100, 2),
         pct_surveys_pre_and_post_expanded = round(surveys_pre_and_post_expanded/cohort_size*100,2),
         pct_surveys_pre_and_post_control = round(surveys_pre_and_post_control/cohort_size_control*100, 2),
         pct_surveys_pre_and_post_expanded_control = round(surveys_pre_and_post_expanded_control/cohort_size_control*100,2)) 

tab_combined <- pcq_rct_overview_side_by_side

for (col in names(pcq_rct_overview_side_by_side)[c(3,4,6,7)]) {
  tab_combined[[col]] <- paste0(
    pcq_rct_overview_side_by_side[[col]], " (", trimws(pcq_rct_overview_prop[[paste0("pct_", col)]]), "%)")
}

# Formatting 
tab_formatted <- tab_combined %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NaN"), "-", .))) %>%
  # Correctly name cohort columns in format Cohort X (yyyy-mm-dd)
  mutate(rct_treat_wave = case_when(
    rct_treat_wave == "1" ~ "Cohort 1 (2022-05-02)",
    rct_treat_wave == "2" ~ "Cohort 2 (2022-11-14)",
    rct_treat_wave == "3" ~ "Cohort 3 (2023-05-19)",
    rct_treat_wave == "4" ~ "Cohort 4 (2023-11-27)",
    rct_treat_wave == "5" ~ "Cohort 5 (2024-06-05)",
    rct_treat_wave == "6" ~ "Cohort 6 (2024-11-06)",
    rct_treat_wave == "99" ~ "Mean")) %>%
  rename(cohort = rct_treat_wave)

names(tab_formatted) <- c("Cohort",
                          "Treated",
                          "Pre & Post",
                          "Any Pre & Post",
                          "Control",
                          "Pre & Post",
                          "Any Pre & Post")

# -- -- Table ####
file_name <- "tabx_pcq_survey_data_overview_rct"
tab_formatted %>%
  kbl(caption = "PCQ Survey Data, by Cohort",
      align = c("lrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Treated" = 3, "Control" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  # column_spec(7, border_left=T, extra_css = "border-left: 1px solid black;")  %>% 
  row_spec(c(nrow(pcq_surveys)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("The Pre and Post column counts the individuals in the treated and control groups who filled out a PCQ survey in the last survey wave prior to, and in the first wave after enrollment. The Any Pre and Post column counts the individuals in the treated and control groups who filled out a PCQ survey in any survey wave prior to, and in any wave after enrollment. The table does not include the last cohort as they did not yet have the opportunity to fill out a post survey.")) %>% 
  landscape() %>%
  # kable_styling(latex_options = c("scale_down")) %>% 
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)

# -- By stratum ####
pcq_rct_overview_by_stratum <- pcq_rct_overview %>%
  group_by(rct_stratum, rct) %>%
  summarize(surveys_pre_and_post = sum(surveys_pre_and_post),
            surveys_pre_and_post_expanded = sum(surveys_pre_and_post_expanded)) %>%
  ungroup() %>%
  left_join(randassign %>%
              group_by(rct, rct_stratum) %>%
              summarize(statum_size = n()),
            by = c("rct", "rct_stratum"),
            relationship = "many-to-one") %>%
  relocate(statum_size, .after = rct_stratum) %>%
  relocate(rct)

pcq_rct_overview_by_stratum

pcq_rct_overview_overall <- pcq_rct_overview_by_stratum %>%
  group_by(rct) %>%
  summarize(rct_stratum = "Mean",
            surveys_pre_and_post = sum(surveys_pre_and_post),
            surveys_pre_and_post_expanded = sum(surveys_pre_and_post_expanded)) %>%
  ungroup() %>%
  left_join(randassign %>%
              group_by(rct) %>%
              summarize(statum_size = n()),
            by = c("rct"),
            relationship = "many-to-one") %>%
  relocate(statum_size, .after = rct_stratum) %>%
  relocate(rct)

pcq_rct_complete <- rbind(pcq_rct_overview_by_stratum, pcq_rct_overview_overall)

pcq_rct_overview_side_by_side <- cbind(pcq_rct_complete %>%
                                         filter(rct==1) %>%
                                         select(rct_stratum, statum_size, surveys_pre_and_post, surveys_pre_and_post_expanded),
                                       pcq_rct_complete %>%
                                         filter(rct==0) %>%
                                         select(statum_size, surveys_pre_and_post, surveys_pre_and_post_expanded) %>%
                                         rename_with(~ paste0(., "_control"), .cols = everything())) # To avoid duplicate names 

pcq_rct_overview_prop <- pcq_rct_overview_side_by_side %>%
  mutate(pct_surveys_pre_and_post = round(surveys_pre_and_post/statum_size*100, 2),
         pct_surveys_pre_and_post_expanded = round(surveys_pre_and_post_expanded/statum_size*100,2),
         pct_surveys_pre_and_post_control = round(surveys_pre_and_post_control/statum_size_control*100, 2),
         pct_surveys_pre_and_post_expanded_control = round(surveys_pre_and_post_expanded_control/statum_size_control*100,2)) 

tab_combined <- pcq_rct_overview_side_by_side

for (col in names(pcq_rct_overview_side_by_side)[c(3,4,6,7)]) {
  tab_combined[[col]] <- paste0(
    pcq_rct_overview_side_by_side[[col]], " (", trimws(pcq_rct_overview_prop[[paste0("pct_", col)]]), "%)")
}

# Formatting 
tab_formatted <- tab_combined %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NaN"), "-", .))) %>%
  # Correctly name cohort columns in format Cohort X (yyyy-mm-dd)
  mutate(rct_stratum = case_when(
    rct_stratum  == "00_06_m" ~ "0-6 Months",
    rct_stratum  == "06_12_m" ~ "6-12 Months",
    rct_stratum  == "12_60_m" ~ "12-60 Months",
    rct_stratum  == "60_pl_m" ~ "60+ Months",
    rct_stratum == "Mean" ~ "Mean"))

names(tab_formatted) <- c("Stratum",
                          "Treated",
                          "Pre & Post",
                          "Any Pre & Post",
                          "Control",
                          "Pre & Post",
                          "Any Pre & Post")

tab_formatted

# -- -- Table ####
file_name <- "tabx_pcq_survey_data_overview_rct_stratum"
tab_formatted %>%
  kbl(caption = "PCQ Survey Data, by Stratum",
      align = c("lrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Treated" = 3, "Control" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  # column_spec(7, border_left=T, extra_css = "border-left: 1px solid black;")  %>% 
  row_spec(c(nrow(pcq_surveys)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("The Pre and Post column counts the individuals in the treated and control groups who filled out a PCQ survey in the last survey wave prior to, and in the first wave after enrollment. The Any Pre and Post column counts the individuals in the treated and control groups who filled out a PCQ survey in any survey wave prior to, and in any wave after enrollment.")) %>% 
  landscape() %>%
  # kable_styling(latex_options = c("scale_down")) %>% 
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)

# ================================================================= ####
# # JONAS CODE - COOL SAMPLE MOVEMENT TABLE, TO BE CLEANED ####
# # Table 2a:   sample_movement (counts)
# ## How to read output: each row show respondents who participated in subsequent waves. Diagonal is new respondents, off-diagonal are continued participation.
# ## Each column sum to total number of surveys a given wave (e.g in the first wave, there were 645 surveys)
# ## Column two show how many from wave one, also participated in wave two (369). There next row is new entries for wave two. Total of column two are total number of surveys wave 2.
# unique_respondents <- pcq %>% select(research_id, wave, first_wave) %>% distinct()
# 
# sample_movement_count <- table(unique_respondents$first_wave, unique_respondents$wave, useNA = "ifany")
# 
# tab_sample_movement_count <- xtable(sample_movement_count, 
#                               caption = "Sample movement (count)",
#                               label = "tab:sample_movement_count")
# 
# tab_sample_movement_count <- autoformat(tab_sample_movement_count)
# 
# print(tab_sample_movement_count,
#       include.rownames = F,
#       file = "output/tables/sample_movement_count.txt")
# 
# # Table 2b: sample_movement (proportion)
# ## How to read output: The proportions are rowwise, e. g. who had first survey in x, and were new in wave y? 
# ## TO-DO: filter by still in prison.
# sample_movement_prop <- round(prop.table(sample_movement_count, margin=2)*100,1)
# 
# tab_sample_movement_prop <- xtable(sample_movement_prop, 
#                                     caption = "Sample movement (prop)",
#                                     label = "tab:sample_movement_prop")
# 
# tab_sample_movement_prop <- autoformat(tab_sample_movement_prop)
# 
# print(tab_sample_movement_prop,
#       include.rownames = F,
#       file = "output/tables/sample_movement_prop.txt")
# 
# fig_sample_origin <- ggplot(as.data.frame(sample_movement_prop), aes(x = factor(Var1), y = fct_rev(factor(Var2)), fill = Freq)) +
#   geom_tile() +
#   scale_fill_gradient(low = "white", high = "steelblue", name = "Sample origin (%)") +
#   geom_text(aes(label = ifelse(Freq > 0, paste0(round(Freq, 1), "%"), "")), size = 2.5) +
#   labs(
#     title = "Recruitment of sample by first and subsequent wave",
#     subtitle = "Each column shows the proportion of respondents by the survey wave they first entered.\n Diagonal values reflect new participants; off-diagonal values show returning respondents from earlier \n waves. E. g., in Wave 3, 41.5% joined in wave 1, 21.4% in wave 2, while 37.2% were new in wave 3.",
#     x = "Subsequent wave",
#     y = "First wave"
#   ) +
#   theme(legend.position = "bottom",
#         plot.background = element_rect(fill="white"),
#         panel.background = element_rect(fill="white"),
#         panel.grid = element_blank())
# 
# ggsave(plot=fig_sample_origin, "output/figures/fig_sample_origin.png", dpi = 300)
# 
