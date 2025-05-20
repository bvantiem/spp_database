# Temp query - survey wave 3 additional push. Delete ####
# Response rates by treatment group
research_participants %>% filter(adm_wave3==1) %>% 
  group_by(treated) %>% 
  summarize(count = n(),
            response = sum(pcq_wave3)) %>%
  mutate(share = response/count)

# 95 control individuals did not participate in wave 3
with(research_participants[which(research_participants$treated==0),],
     table(pcq_wave3))

# 45 of them could not participate because they were already released
with(research_participants[which(research_participants$treated==0),],
     table(pcq_wave3, adm_wave3))

# Make a list of all treated/control individuals we need to follow up with 
ids <- with(research_participants, research_participants[which(treated %in% c(0,1) & adm_wave3==1 & pcq_wave3==0),"research_id"])
ids_oversample <- with(research_participants, research_participants[which(treated %ni% c(0,1) & adm_wave3==1 & pcq_wave3==0),"research_id"])

# For those treated in wave 1 and wave 2, for how many of them do we have a pre- and post-measurement?
# Mark these as priority individuals 
temp <- research_participants[which(research_participants$research_id %in% c(ids, ids_oversample)),]
temp$pre_measurement <- with(temp, ifelse((treatment_wave==1 & pcq_wave1==1)|treatment_wave==2 & (pcq_wave1==1|pcq_wave2==1), 1,0))
temp$post_measurement <- with(temp, ifelse(treatment_wave==1 & (pcq_wave1==1|pcq_wave3==1),1,0))
temp$priority <- with(temp, ifelse(post_measurement==0 & pre_measurement==1,1,
                                   ifelse(research_id %in% ids_oversample,3,2)))

temp <- left_join(temp, wave3_callsheets, by = "research_id")
ids <- unmask_ids(temp$research_id)
temp$original_id <- ids$original_id
temp <- temp[,c("original_id","research_id","unit","location","priority")]
temp$level <- substring(temp$location, 1, 1)
temp$cell <- substring(temp$location, 3, 4)
temp$priority[which(is.na(temp$priority))] <- 3
temp <- temp %>% group_by(unit, priority, level)
temp <- temp %>% arrange(location, .by_group = TRUE)
temp <- temp[-which(temp$unit=="ca" & temp$priority==3),]
write.csv(temp, file = "data/temp/follow_up_individuals_w_oversampling.csv")
table(temp$unit, temp$priority)

# Based on script 0_df_participation_wave23
# Participation rates wave 3
wave3_all <- left_join(wave3_callsheets, wave3_participation[,-which(names(wave3_participation)=="unit")], by = "research_id") # Because some people completed on multiple units
wave3_all$completed_survey[which(is.na(wave3_all$completed_survey))] <- 0
wave3_all %>% group_by(unit) %>% summarize(n = n(), completed_survey=sum(completed_survey)/2) %>%
  mutate(participation_rate = completed_survey/n) %>% arrange(participation_rate)

length(unique(wave3_participation$research_id))/length(unique(wave3_callsheets$research_id))
length(which(wave3_participation$research_id %in% wave3_callsheets$research_id))
length(which(wave3_participation$research_id %ni% wave3_callsheets$research_id))

# Identify individuals who are misclassified to be fixed in data entry sheets 
wave3_callsheets$unit_2 <- wave3_callsheets$unit
temp <- left_join(wave3_participation, wave3_callsheets[,c("original_id", "unit_2")])
temp$mismatch <- ifelse(temp$unit != temp$unit_2, 1, 0)


