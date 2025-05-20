# Data set-up ----
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")

# Data ####
pcq <- readRDS("data/processed/processing_layer_1/pcq.rds")
research_participants <- readRDS("data/processed/research_participants.Rds")

# Variables to merge in ####
participant_vars <- c("treated", "treatment_wave", "stratum",
  "pcq_wave1", "pcq_wave2", "pcq_wave2_5", "pcq_wave3", "nsurveys","nsurveys_pre", "nsurveys_post",
  "est_days_served_on_20220501", "min_sent_days", "max_sent_days", "life",
  "violent_offense", "property_offense", "drugs_offense", "publicorder_offense","notanarrest_offense",
  "race_white", "race_black", "marital_status_code", "STG", "grade_complete", "high_school", "age_at_treatment", "partner", "children", "foreign_born")

survey_vars <- c("survey_wave", "unit", "unit_type", "date", "survey_no")

# ---------- Survey-Level dataframes ---------- ####
# Retain only the first survey within each survey wave ####
pcq <- pcq %>%
  group_by(research_id, survey_wave) %>%
  slice(which.min(survey_no)) # This retains only the first survey within a survey wave for each respondent.

# Link survey responses to demographic data ####
pcq <- left_join(pcq, research_participants)

# Drop all questions that we're not going to use in this analysis
questions.to.retain <- unique(unlist(c(survey_vars, scale.key.longitudinal, experience.key, service_use_qs, participant_vars)))
pcq <- pcq[,c("research_id", questions.to.retain)]

# Drop data from individuals with more than 10 missing answers ####
tail(sort(rowSums(is.na(pcq[,unique(unlist(scale.key.longitudinal))]))), n=56) # use pcq because it doesn't count 'no opinion' answers as missing
# Drop individuals with more than 10 missing answers
i <- which(rowSums(is.na(pcq[,unique(unlist(scale.key.longitudinal))]))>=30) # One ind. in treated group had >20 answers missing. Set at 30 to avoid dropping this person. It still drops another person who had more than 30 answers missing.
pcq <- pcq[-i,]
# dropped.ids <- pcq$research_id[i]

# Create pcq2
pcq2 <- pcq
pcq2[pcq2==111] <- NA
pcq2[pcq2==996] <- NA

# Intermediate save ####
saveRDS(pcq, file="data/processed/processing_layer_2/pcq.rds")
saveRDS(pcq2, file="data/processed/processing_layer_2/pcq2.rds")

# Impute missing survey data using Random forest ####
# Warning: Takes a very long time to run!
# https://academic-oup-com.proxy.library.upenn.edu/bioinformatics/article/28/1/112/219101?login=true&token=
# Impute based on service use variables

pcq_mf <- as.data.frame(pcq2[,-which(names(pcq2) %in% c(participant_vars, survey_vars))]) # Because admin data should not be
vars_to_factors <- c(unique(unlist(c(scale.key.longitudinal, experience.key)))) # Treat Likert as Factor for MissForest
for(i in vars_to_factors){
  pcq_mf[,i] <- as.factor(pcq_mf[,i])
}
pcq_imp <- missForest(pcq_mf[,-which(names(pcq_mf)=="research_id")])
pcq_mf <- pcq_imp$ximp
pcq_mf$research_id <- pcq2$research_id
pcq_mf <- pcq_mf %>% relocate(research_id, .before = q10)
vars_to_num <- c(unique(unlist(c(scale.key.longitudinal, experience.key))))
for(i in vars_to_num){
  pcq_mf[,i] <- as.numeric(pcq_mf[,i])
}
pcq_mf <- cbind(pcq2[,which(names(pcq2) %in% survey_vars)], pcq_mf, pcq2[,which(names(pcq2) %in% participant_vars)])

# Intermediate save
# Intermediate save of imputed data ####

saveRDS(pcq_mf, file="data/processed/processing_layer_2/pcq2_missforest.rds")

# Calculate scale scores for imputed dataset ####
# pcq_mf <- readRDS("data/processed/processing_layer_2/pcq2_missforest.rds")
vars <- unique(unlist(scale.key.longitudinal))
my.scores <- scoreItems(scale.key.longitudinal,
                        pcq_mf[,which(names(pcq_mf) %in% vars)],
                        missing=FALSE)
scale.scores <- as.data.frame(my.scores$scores)
names(scale.scores) <- paste0("scale_", names(scale.scores))
pcq_mf <- cbind(pcq_mf, scale.scores)
pcq_mf <- pcq_mf %>% relocate(c(names(scale.scores)), .after = q163)


# Add dummies for specific experiences based on whether they participated at least once

recode <- unique(unlist(experience.key))

for(i in recode){
  pcq_mf[,ncol(pcq_mf)+1] <- NA
  names(pcq_mf)[ncol(pcq_mf)] <- paste0(i,"_dummy")
  for(k in 1:nrow(pcq_mf)){
    pcq_mf[k,ncol(pcq_mf)] <- ifelse(pcq_mf[k,i]==1,0,1)
  }
}

saveRDS(pcq_mf, file="data/processed/pcq2_missforest.rds")

# ---------- Person Level Dataframe - for treated sample with pre and post surveys ----------####
# pcq_mf <- readRDS("data/processed/pcq2_missforest.rds")

# For each survey, mark whether they constituted a pre or post-survey, and mark whether they were the last pre-treatment survey or the first post-treatment survey ####
pcq_mf$survey_pre <- with(pcq_mf, ifelse((treatment_wave %in% c(0,1) & survey_wave==1)|
                                           (treatment_wave==2 & survey_wave %in% c(1,2))|
                                           (treatment_wave==3 & survey_wave %in% c(1:3)), 1, 0))
pcq_mf$survey_post <-  with(pcq_mf, ifelse((treatment_wave %in% c(0,1) & survey_wave %in% c(2,3))|
                                             (treatment_wave==2 & survey_wave %in% c(3)), 1, 0))

# Dummy for last pre survey
pcq_mf$survey_pre_last <- 0
unique.ids.pre <- unique(research_participants[which(research_participants$treated %in% c(0,1) & research_participants$nsurveys_pre>0),"research_id"])
unique.ids.pre <- unique.ids.pre[which(unique.ids.pre %in% pcq$research_id)] # to ensure the ID was not dropped above when dropping individuals with too many missing answers

pcq_mf <- as.data.frame(pcq_mf)
for(i in unique.ids.pre){
  row.index <- with(pcq_mf, which(research_id==i & survey_pre==1))
  if(length(row.index)==1){
    pcq_mf[row.index,"survey_pre_last"] <- 1
  }else{
    pcq_mf[row.index,"survey_pre_last"] <- ifelse(pcq_mf[row.index,"survey_wave"]==max(pcq_mf[row.index, "survey_wave"]),1,0)
  }
}

# Dummy for first post survey
pcq_mf$survey_post_first <- 0
unique.ids.post <- unique(research_participants[which(research_participants$treated %in% c(0,1) & research_participants$nsurveys_post>0),"research_id"])
unique.ids.post <- unique.ids.post[which(unique.ids.post %in% pcq$research_id)] # to ensure the ID was not dropped above when dropping individuals with too many missing answers
unique.ids.post <- unique.ids.post[unique.ids.post!="rid_js0382"] # HANDCODED - this ID was dropped above due to a very large number of missing answers

pcq_mf <- as.data.frame(pcq_mf)
for(i in unique.ids.post){
  row.index <- with(pcq_mf, which(research_id==i & survey_post==1))
  if(length(row.index)==1){
    pcq_mf[row.index,"survey_post_first"] <- 1
  }else{
    pcq_mf[row.index,"survey_post_first"] <- ifelse(pcq_mf[row.index,"survey_wave"]==min(pcq_mf[row.index, "survey_wave"]),1,0)
  }
}

# Table of survey completion for full treated sample ####
# Table to map pre_and_post
f.tab.treated <- function(x){
  research_participants %>% filter(treatment_wave==x,
                                 research_id != "rid_js0382") %>%
  mutate(any_surveys_pre = ifelse(nsurveys_pre>0,1,0),
         any_surveys_post = ifelse(nsurveys_post>0,1,0)) %>%
  mutate(survey_pre_and_post = ifelse(any_surveys_pre==1 & any_surveys_post==1, 1, 0)) %>%
  group_by(treated) %>%
  summarize(treatment_wave=x,
            group_size = n(),
            survey_pre = sum(any_surveys_pre),
            survey_post = sum(any_surveys_post),
            survey_pre_and_post = sum(survey_pre_and_post)) %>%
    relocate(treatment_wave, .before = treated) %>%
    mutate(share_pre_and_post = round(survey_pre_and_post/group_size,2)) %>%
    setNames(c("Cohort", "LSU", "N", "Pre-Survey", "Post-Survey", "Pre- and Post- Survey", "Share Pre- and Post- Survey"))}

tab <- rbind(f.tab.treated(1), f.tab.treated(2))

# Build Table
# PDF
tab %>%
  kbl(caption = "Treated Sample",
      align = rep("l", ncol(tab))) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  column_spec(1:3, width = "4em") %>%
  column_spec(4:7, width = "6em") %>%
  save_kable(file = "output/tables/tabx_treated_sample_surveys.pdf",
             self_contained = T,density = 200)

# Tex
tab %>%
  kbl(caption = "Treated Sample",
      align = rep("l", ncol(tab)),
      format.args = list(big.mark = ","),
      label = "treated_sample",format = "latex", # Do not change label!
      booktabs = T,linesep = "") %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  column_spec(1:3, width = "4em") %>%
  column_spec(4:7, width = "6em") %>%
  save_kable(file = "output/tables/tabx_treated_sample_surveys.tex",
             self_contained = T,density = 200)

# Analysis sample
pcq_mf_sample <- subset(pcq_mf, treated %in% c(0,1) & nsurveys_pre>0 & nsurveys_post>0)
temp <- as.data.frame(table(pcq_mf_sample$research_id))
treated.sample <- subset(research_participants, treated %in% c(0,1) & nsurveys_pre>0 & nsurveys_post>0)
treated.sample <- subset(treated.sample, research_id!="rid_js0382") # HANDCODED - this ID was dropped above due to a very large number of missing answers
vars <- c(grep("scale_",names(pcq_mf_sample), value=TRUE), grep("_dummy",names(pcq_mf_sample), value=TRUE), "date", "survey_no", "survey_wave")

# Merge in scores from pre-survey
i <- match(treated.sample$research_id, pcq_mf_sample[which(pcq_mf_sample$survey_pre_last==1),"research_id"])
treated.sample <- cbind(treated.sample, pcq_mf_sample[which(pcq_mf_sample$survey_pre_last==1)[i],vars])
treated.sample$survey_wave <- as.factor(treated.sample$survey_wave)
names(treated.sample) <- gsub("scale_", "pre_", names(treated.sample))
names(treated.sample) <- gsub("^date", "pre_date", names(treated.sample))
names(treated.sample) <- gsub("^survey_no", "pre_survey_no", names(treated.sample))
names(treated.sample) <- gsub("^survey_wave", "pre_survey_wave", names(treated.sample))
names(treated.sample) <- gsub("^q", "pre_q", names(treated.sample))

# Merge in scores from post-survey
i <- match(treated.sample$research_id, pcq_mf_sample[which(pcq_mf_sample$survey_post_first==1),"research_id"])
treated.sample <- cbind(treated.sample, pcq_mf_sample[which(pcq_mf_sample$survey_post_first==1)[i],vars])
treated.sample$survey_wave <- as.factor(treated.sample$survey_wave)
names(treated.sample) <- gsub("scale_", "post_", names(treated.sample))
names(treated.sample) <- gsub("^date", "post_date", names(treated.sample))
names(treated.sample) <- gsub("^survey_no", "post_survey_no", names(treated.sample))
names(treated.sample) <- gsub("^survey_wave", "post_survey_wave", names(treated.sample))
names(treated.sample) <- gsub("^q", "post_q", names(treated.sample))

# Calculate time between last pre-survey and first post-survey
treated.sample$time_between_pre_and_post_survey <- as.numeric(difftime(treated.sample$post_date, treated.sample$pre_date))

saveRDS(treated.sample, file="data/processed/treated_sample_with_pre_and_post_survey.rds")
