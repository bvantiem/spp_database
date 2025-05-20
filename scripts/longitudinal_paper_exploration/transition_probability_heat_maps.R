# Notes ####
# Data set-up ----
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")


pcq_mf <- readRDS("data/processed/pcq2_missforest.rds")
pcq_12 <- pcq_mf[which(pcq_mf$survey_wave %in% c(1,2)),]
pcq_12 <- pcq_12 %>%
  group_by(research_id) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  filter(survey_no %in% c(1,2)) %>%
  select(-ends_with(("_dummy")))


question_cols <- names(pcq_12)[str_detect(names(pcq_12), "^q")]

# Loop over each "q" column
for (q in question_cols) {
  cat("\n### Processing:", q, "###\n")  # Print column name

  # Convert to wide format for the current question
  pcq_12_wide <- pcq_12 %>%
    select(research_id, all_of(q), survey_no) %>%
    pivot_wider(names_from = survey_no,
                values_from = all_of(q),
                names_prefix = "survey_no_")

  # Create transition matrix
  transition_matrix <- table(pcq_12_wide$survey_no_1, pcq_12_wide$survey_no_2)

  # Convert to proportion matrix
  prop_matrix <- prop.table(transition_matrix, margin = 1)

  # Convert matrix to long format
  df_heatmap <- as.data.frame(as.table(prop_matrix)) %>%
    rename(From = Var1, To = Var2, Probability = Freq) %>%
    mutate(From = factor(From), To = factor(To))

  # Generate heatmap
  heatmap_plot <- ggplot(df_heatmap, aes(x = To, y = From, fill = Probability)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = paste("Transition Probability Heatmap for", q),
         subtitle = pcq_lookup$question_pa_2022a[which(pcq_lookup$question_qno==q)],
         x = "Survey 2 Response",
         y = "Survey 1 Response",
         fill = "Probability") +
    theme_minimal()

  # Print heatmap
  print(heatmap_plot)
}


#########
pcq_12_wide <- pcq_12[,c("research_id", "q10", "survey_no")] %>%
  pivot_wider(names_from = survey_no,
              values_from = q10,
              names_prefix = "survey_no_")

# Normalize rows to get probabilities
prop_matrix <- prop.table(table(pcq_12_wide$survey_no_1 , pcq_12_wide$survey_no_2), margin = 1)

# Convert matrix to long format correctly
df_heatmap <- as.data.frame(as.table(prop_matrix)) %>%
  rename(From = Var1, To = Var2, Probability = Freq) %>%
  mutate(From = factor(From), To = factor(To))  # Ensure categorical ordering

# Generate heatmap
ggplot(df_heatmap, aes(x = To, y = From, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Transition Probability Heatmap",
       x = "Survey 2 Response",
       y = "Survey 1 Response",
       fill = "Probability") +
  theme_minimal()
