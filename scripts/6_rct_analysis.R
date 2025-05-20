# Set up ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
treated.sample <- readRDS("data/processed/treated_sample_with_pre_and_post_survey.rds")

# Drop lifers
treated.sample <- treated.sample[-which(treated.sample$life==1),]




# Analysis

experiences <- gsub("(post_)(.*)(_dummy)","\\2\\3",grep("post_q", names(treated.sample), value=TRUE))
i <- match(gsub("_dummy", "", experiences), pcq_lookup$question_qno)
experience.names <- pcq_lookup[i,c("question_pa_2022a")]
experience.names <- gsub("In the past month, (how often )?", "", experience.names)
experience.names <- gsub("(did )?(have )?you (been )?", "", experience.names)
experience.names <- gsub("I have been ", "", experience.names)
experience.names <- gsub("on this unit", "on this unit?", experience.names)

# Function - list of regressions for each prison climate scale
regression_output_list <- lapply(as.list(c(scale_names$scale_abbreviation, experiences)),
                               function(pc.scale) {
  main_formula <- as.formula(paste0("post_", pc.scale, " ~  treatment_wave + treated + pre_", pc.scale, " +
                           time_between_pre_and_post_survey + pre_survey_wave + post_survey_wave +
                           pre_survey_no + post_survey_no +
                           stratum + asca +
                           est_days_served_on_20220501 + days_to_min_at_treatment +
                           race + marital_status_code + mh_code + STG + grade_complete +
                           age_on_20220501 + children + foreign_born + partner"))

  feols(fml = main_formula,
        data = treated.sample) # Note - I don't have enough degrees of freedom to cluster! My p-values become very large when I do so.
        # cluster = ~ treatment_wave

  })

# Function to generate tidy models output
coefs_from_list <- function(regression_output_list) {
  data.table::rbindlist(
    lapply(regression_output_list, broom::tidy),
    idcol = "model_id")
}

# Run and build table
tidy_models_output <- coefs_from_list(regression_output_list)
scale.names.nonumber <- gsub("([0123456789][0123]?. )","" ,scale_names$scale_theory_long)
var.names <- c(scale.names.nonumber,experience.names)
tidy_models_output <- left_join(tidy_models_output,
                                   data.frame(var = var.names,
                                              model_id = 1:length(var.names)))
coefs_treated <- tidy_models_output %>%
  filter(term == "treated")


tab <- coefs_treated %>%
  select(var, estimate, std.error, p.value) %>%
  mutate(estimate = round(estimate,2),
         std.error = round(std.error,2),
         p.value = round(p.value,2)) %>%
  # pivot_longer(!var) %>%
  mutate(Dataset = "Imputed") %>%
  select(Dataset, everything())

tab %>%
  kbl(caption = "Estimates by Scale",
      align = c("llrrr"),
      format.args = list(big.mark = ","),
      label = "scale_estimates") %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  save_kable(file = "output/tables/tabx_scale_experience_estimates.pdf")

tab %>%
  kbl(caption = "Estimates by Scale",
      align = c("llrrr"),
      format.args = list(big.mark = ","),
      label = "dd_estimates",format = "latex", # Do not change label!
      booktabs = T,linesep = "") %>%
  #kable_styling(latex_options="scale_down") %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(3:6, width = "7em") %>%
  save_kable(file = "output/tables/tabx_scale_experience_estimates.tex")
