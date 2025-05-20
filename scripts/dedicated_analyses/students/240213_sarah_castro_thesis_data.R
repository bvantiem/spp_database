`%ni%` = Negate(`%in%`)
library(dplyr)
library(readxl)

pcq_p1 <- readRDS("data/processed/processing_layer_1/pcq.Rds")
pcq_p2 <- readRDS("data/processed/processing_layer_2/pcq.Rds")
pcq_lookup <- read_xlsx("data/raw/5_pcq_survey_questions/230725_pcq_survey_questions_NL_PA.xlsx")

# Subset to pcq_lookup variables she will want
pcq_lookup_sarah <- pcq_lookup %>%
  mutate(question_no_pa_2022a = as.numeric(question_no_pa_2022a)) %>%
  filter(in_which_survey %in% c("NL & PA", "PA only")) %>%
  select(pc_theory, domain_name, scale_theory_long,
         question_no_pa_2022a,
         answer_scale_pa,
         recode_pa_2022a)

# From Sarah: The questions from the PCQ I'd like to use are 7-9, 61, 66, 69-72, 123-136, 138, and 167. The administrative data variables I'd like to have access to are commitment crime, sentence length, age, and race.
survey_vars <- c("research_id", "survey_wave","date","unit","unit_type", "survey_no")
question_vars <- c("q7", "q8", "q9",
                      "q61", "q66",
                      paste0("q", 69:72),
                      paste0("q", 123:136),
                      "q138", "q167")

demo_vars <- c("min_sent_days",
               "max_sent_days",
               "life",
               "violent_offense",
               "property_offense",
               "drugs_offense",
               "publicorder_offense",
               "notanarrest_offense",
               "race_white",
               "race_black",
               "age_on_20220501",
               "est_days_served_on_20220501")

pcq_sarah <- left_join(pcq_p2[,c(survey_vars, demo_vars)],
                       pcq_p1[,c("research_id", "date", question_vars)],
                       by = c("research_id", "date"))
pcq_sarah$age_on_20220501 <- round(pcq_sarah$age_on_20220501)

saveRDS(pcq_sarah, file="data/processed/dedicated_analyses/240221_pcq_sarah_castro_thesis.Rds")
saveRDS(pcq_lookup_sarah, file="data/processed/dedicated_analyses/240213_pcq_lookup_sarah_castro_thesis.Rds")
