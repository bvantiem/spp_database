research_participants %>%
  filter(treated %in% c(1,0)) %>%
  filter(treatment_wave %in% c(1,2, 2.5, 3, 4)) %>%
  filter(stratum != "lifer") %>%
  mutate(any_surveys_pre = ifelse(nsurveys_pre>0,1,0),
         any_surveys_post = ifelse(nsurveys_post>0,1,0)) %>%
  mutate(surveys_pre_and_post = ifelse(any_surveys_pre==1 & any_surveys_post==1, 1, 0)) %>%
  group_by(treated) %>% # treatment_wave
  summarize(n = n(),
            any_surveys_pre = mean(any_surveys_pre),
            any_surveys_post = mean(any_surveys_post),
            surveys_pre_and_post = mean(surveys_pre_and_post)) %>%
  mutate(n_ind_pre = n*any_surveys_pre,
         n_ind_post = n*any_surveys_post,
         n_ind_pre_post = n*surveys_pre_and_post)


# Unique individuals and total surveys
length(unique(research_participants[which(research_participants$nsurveys>0),"research_id"]))
sum(research_participants$nsurveys)

research_participants %>%
  filter(treated %in% c(1,0)) %>%
  filter(treatment_wave %in% c(1,2, 2.5, 3, 4, 5)) %>%
  filter(stratum != "lifer") %>%
  nrow()
