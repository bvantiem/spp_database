# Notes ####
# Code from:
# http://rnotr.com/likert/ggplot/barometer/likert-plots/

# Note to self: the order of the stackings is determined by the leveling of the col column!! This has changed since the above tutorial was written - which is why the code no longer works.
# See here: https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2

# Data set-up ----
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")


# Plots by Scale, by Unit, with imputed data ####
# This is done with imputed data for ease - if not I need to recalculate scale scores over answered questions only
# Load data


# Run for each wave ####
for(i_survey_wave_no in list(1, 2, 3, 4, 5, c(1, 2, 3, 4, 5))){
pcq_mf <- readRDS("data/processed/pcq2_missforest.rds") # prepared in 5_rct_dataprep
pcq_mf <- pcq_mf %>% ungroup()

survey_wave_no <- i_survey_wave_no
print(survey_wave_no)

# We want the plots to name the units in full and order them in order of privilege levels
pcq_mf <- pcq_mf %>%
  mutate(
    unit_type = case_when(
      unit_type == "rhu" ~ "1. Restrictive Housing",
      unit_type == "gp" ~ "2. General Population",
      unit_type == "gp-tc" ~ "3. Therapeutic Community",
      unit_type == "rec" ~ "4. Recovery Unit",
      unit_type == "hons" ~ "5. Honor Block",
      unit_type == "gp-epu" ~ "5. Enhanced Privilege Unit",
      unit_type == "gp-senior" ~ "5. Senior Unit",
      unit_type == "ls" ~ "6. Little Scandinavia",
      TRUE ~ NA_character_
    )
  )

# Set up plot parameters
numlevels <- 4
pal<-c(brewer.pal(3,"Reds")[c(3,2)],brewer.pal(4,"Greens")[c(2,4)]) # To get a palette going from dark red to light red, to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Dissatisfied", "Somewhat dissatisfied", "Somewhat satisfied",  "Satisfied")

# Function
f.likert.scale.by.unit.plot <- function(scale_x, plot.title){

  data_for_graph <- pcq_mf %>%
    filter(!is.na(unit_type), survey_wave %in% survey_wave_no) %>%
    select(unit_type, {{scale_x}}) %>% # Wrap in {{}} to ensure we can use column names as arguments
    mutate(dissatisfied = ifelse({{scale_x}} <= 2, 1, 0),
           somewhat_dissatisfied = ifelse({{scale_x}} > 2 & {{scale_x}} <= 3, 1, 0),
           somewhat_satisfied = ifelse({{scale_x}} > 3 & {{scale_x}} <= 4, 1, 0),
           satisfied = ifelse({{scale_x}} > 4 & {{scale_x}} <= 5, 1, 0)) %>%
    group_by(unit_type) %>%
    mutate(nobs = n()) %>%
    summarize(dissatisfied = mean(dissatisfied) * 100,
              somewhat_dissatisfied = mean(somewhat_dissatisfied) * 100,
              somewhat_satisfied = mean(somewhat_satisfied) * 100,
              satisfied = mean(satisfied) * 100,
              nobs = unique(nobs)) %>%
    pivot_longer(cols = !c(unit_type, nobs), names_to = "outcome", values_to = "value")  # Retain nobs in pivot_longer

  # Ensure all unit_type and outcome combinations exist, even if they have zero values
  data_for_graph <- data_for_graph %>%
    complete(unit_type, outcome, fill = list(value = 0))  # Fill missing combinations with value = 0

  # Assign colors based on outcome type
  data_for_graph$col <- NA
  data_for_graph[which(data_for_graph$outcome == "dissatisfied"), "col"] <- pal[1]
  data_for_graph[which(data_for_graph$outcome == "somewhat_dissatisfied"), "col"] <- pal[2]
  data_for_graph[which(data_for_graph$outcome == "somewhat_satisfied"), "col"] <- pal[3]
  data_for_graph[which(data_for_graph$outcome == "satisfied"), "col"] <- pal[4]

  data_for_graph$unit_type <- as.factor(data_for_graph$unit_type) # So that I can reverse row order later

  # Ensure that the unit_type and nobs are treated as character vectors for label creation
  data_for_graph <- data_for_graph %>%
    mutate(unit_type = paste0(unit_type, " (n=", nobs, ")"))

  lows <- data_for_graph[which(data_for_graph$outcome %in% c("dissatisfied", "somewhat_dissatisfied")), ]
  lows <- as.data.frame(lows)
  highs <- data_for_graph[which(data_for_graph$outcome %in% c("somewhat_satisfied", "satisfied")), ]
  highs <- as.data.frame(highs)

  # Plot
  p <- ggplot() +
    geom_bar(data = highs, aes(x = unit_type, y = value, fill = col), position = "stack", stat = "identity") +
    geom_bar(data = lows, aes(x = unit_type, y = -value, fill = col), position = "stack", stat = "identity") +
    geom_hline(yintercept = 0, color = c("white")) +
    scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks = pal, guide = "legend") +
    theme_fivethirtyeight() +
    coord_flip() +
    labs(title = str_to_title(gsub("_", " ", plot.title)), y = "", x = "") +
    theme(plot.title = element_text(size = 14, hjust = 0.5)) +
    theme(axis.text.y = element_text(hjust = 0)) +
    theme(legend.position = "bottom") +
    theme(legend.text = element_text(size = 5)) +
    scale_y_continuous(breaks = seq(-100, 100, 10), limits = c(-100, 100)) +
    scale_x_discrete(limits = rev(levels(factor(data_for_graph$unit_type))))

  ggsave(paste0("output/dedicated_analyses/chs_report/scale_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_", plot.title, ".jpg"), p, width = 10, height = 4, dpi = 300)

  write.xlsx(data_for_graph, file = paste0("output/dedicated_analyses/chs_report/scale_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/data_", plot.title, ".xlsx"))
}

f.likert.scale.by.unit.plot(scale_prisoners, "prisoner_relationships")
f.likert.scale.by.unit.plot(scale_staff, "staff_prisoner_relationships_and_procedural_justice")
f.likert.scale.by.unit.plot(scale_safety, "safety")
f.likert.scale.by.unit.plot(scale_visits, "visits")
f.likert.scale.by.unit.plot(scale_contact, "frequency_of_contact")
f.likert.scale.by.unit.plot(scale_sleep, "sleep_quality")
f.likert.scale.by.unit.plot(scale_care, "quality_of_care")
f.likert.scale.by.unit.plot(scale_shop, "commissary")
f.likert.scale.by.unit.plot(scale_complaints, "grievances")
f.likert.scale.by.unit.plot(scale_actav, "meaningful_activity")
f.likert.scale.by.unit.plot(scale_reint, "reintegration_support")

### Plots by item, by Unit, with normal data ####
# Load and process data ####
pcq <- readRDS("data/processed/processing_layer_1/pcq.rds")

# Data prep
pcq <- pcq %>%
  group_by(research_id, survey_wave) %>%
  slice(which.min(survey_no)) # This retains only the first survey within a survey wave for each respondent.

pcq <- pcq %>%
  mutate(
    unit_type = case_when(
      unit_type == "rhu" ~ "1. Restrictive Housing",
      unit_type == "gp" ~ "2. General Population",
      unit_type == "gp-tc" ~ "3. Therapeutic Community",
      unit_type == "rec" ~ "4. Recovery Unit",
      unit_type == "hons" ~ "5. Honor Block",
      unit_type == "gp-epu" ~ "5. Enhanced Privilege Unit",
      unit_type == "gp-senior" ~ "5. Senior Unit",
      unit_type == "ls" ~ "6. Little Scandinavia",
      TRUE ~ NA_character_
    )
  )

pcq2 <- pcq
pcq2[pcq2==111] <- NA
pcq2[pcq2==996] <- NA
pcq2 <- pcq2 %>% ungroup()

# Likert scale items ####
# Set up plot parameters
numlevels <- 5
pal<-c(brewer.pal(3,"Reds")[c(3,2)],brewer.pal(4,"Greys")[c(2,2)], brewer.pal(4,"Greens")[c(2,4)]) # To get a palette going from dark red to light red, to grey (neutral), to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Strongly Disagree", "Disagree","Neutral", "Agree",  "Strongly Agree")

f.likert.item.by.unit.plot <- function(item_x, plot.title){

  data_for_graph <- pcq2 %>% filter(!is.na(unit_type) &
                                      unit_type!="inf" &
                                      survey_wave %in% survey_wave_no &
                                      !is.na({{item_x}})) %>%
    select(unit_type,{{item_x}}) %>% # Wrap in {{}} to ensure we can use column names as arguments
    mutate(strongly_disagree = ifelse({{item_x}}==1,1,0),
           disagree = ifelse({{item_x}}==2,1,0),
           neutral_low = ifelse({{item_x}}==3,1,0),
           neutral_high = ifelse({{item_x}}==3,1,0),
           agree = ifelse({{item_x}}==4,1,0),
           strongly_agree = ifelse({{item_x}}==5,1,0)) %>%
    group_by(unit_type) %>%
    summarize(strongly_disagree = mean(strongly_disagree)*100,
              disagree = mean(disagree*100),
              neutral_low = mean(neutral_low*100)/2, # because I will include it twice
              neutral_high = mean(neutral_high*100)/2, # because I will include it twice
              agree = mean(agree*100),
              strongly_agree = mean(strongly_agree*100)) %>%
    pivot_longer(!unit_type, names_to = "outcome", values_to = "value")
  # arrange(outcome) %>%
  # mutate(col = rep(pal,each=length.unit.types))

  data_for_graph$col <- NA
  data_for_graph[which(data_for_graph$outcome=="strongly_disagree"), "col"] <- pal[1]
  data_for_graph[which(data_for_graph$outcome=="disagree"), "col"] <- pal[2]
  data_for_graph[which(data_for_graph$outcome=="neutral_low"), "col"] <- pal[3]
  data_for_graph[which(data_for_graph$outcome=="neutral_high"), "col"] <- pal[3]
  data_for_graph[which(data_for_graph$outcome=="agree"), "col"] <- pal[5]
  data_for_graph[which(data_for_graph$outcome=="strongly_agree"), "col"] <- pal[6]
  data_for_graph$unit_type <- as.factor(data_for_graph$unit_type) # So that I can reverse row order later

  lows <- data_for_graph[which(data_for_graph$outcome %in% c("strongly_disagree", "disagree", "neutral_low")),]
  lows$col <- factor(lows$col, levels = c("#DE2D26", "#FC9272", "#CCCCCC"))
  # lows <- lows[rev(rownames(lows)),]
  # lows <- lows %>% arrange(outcome)
  highs <- data_for_graph[which(data_for_graph$outcome %in% c("neutral_high","agree", "strongly_agree")),]
  highs <- as.data.frame(highs)

  # Plot
  # Note to self: the order of the stackings is determined by the leveling of the col column!!
  # See here: https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
  p <- ggplot() + geom_bar(data=highs, aes(x = unit_type, y=value, fill=col), position="stack", stat="identity") +
    geom_bar(data=lows, aes(x = unit_type, y=-value, fill=col), position="stack", stat="identity") +
    geom_hline(yintercept = 0, color =c("white")) +
    scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks=pal[-3], guide="legend") +
    theme_fivethirtyeight() +
    coord_flip() +
    labs(title=str_to_title(gsub("_", " ", plot.title)), y="",x="") +
    theme(plot.title = element_text(size=14, hjust=0.5)) +
    theme(axis.text.y = element_text(hjust=0)) +
    theme(legend.position = "bottom") +
    scale_y_continuous(breaks=seq(-100,100,10), limits=c(-100,100)) +
    scale_x_discrete(limits = rev(levels(data_for_graph$unit_type)))

  ggsave(paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_", plot.title, ".jpg"), p, width = 10, height = 3.5, dpi = 300)
}
f.likert.item.by.unit.plot(q4, "i_am_satisfied_with_this_unit")
f.likert.item.by.unit.plot(q5, "i_am_satisfied_with_the_staff_on_this_unit")
f.likert.item.by.unit.plot(q6, "i_am_satisfied_with_building_and_facilities_on_this_unit")
f.likert.item.by.unit.plot(q62, "i_am_satisfied_with_the_recreation_activities")

# These questions have been recoded
# Confusing as strongly agree now means staff do not use too much force
# Reword titles accordingly
f.likert.item.by.unit.plot(q37, "there_are_not_a_lot_of_drugs_on_this_unit")
f.likert.item.by.unit.plot(q42, "staff_do_not_use_too_much_force_on_this_unit")


# Yes/No items ####
pcq2$q44.d <- ifelse(pcq2$q44>1,1,0)
pcq2$q46.d <- ifelse(pcq2$q46>1,1,0)
pcq2$q147.d <- ifelse(pcq2$q46==2,1,0)

numlevels <- 2
pal<-c(brewer.pal(3,"Reds")[c(3)],brewer.pal(4,"Greens")[c(4)]) # To get a palette going from dark red to light red, to grey (neutral), to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Yes", "No")


f.likert.item.by.unit.plot <- function(item_x, plot.title, plot.name){

  data_for_graph <- pcq2 %>% filter(!is.na(unit_type) &
                                       unit_type!="inf" &
                                       survey_wave %in% survey_wave_no &
                                       !is.na({{item_x}})) %>%
    select(unit_type,{{item_x}}) %>% # Wrap in {{}} to ensure we can use column names as arguments
    group_by(unit_type) %>%
    summarize(yes = mean({{item_x}})*100,
              no = (1-mean({{item_x}}))*100) %>%
    pivot_longer(!unit_type, names_to = "outcome", values_to = "value")

  data_for_graph$col <- NA
  data_for_graph[which(data_for_graph$outcome=="yes"), "col"] <- pal[1]
  data_for_graph[which(data_for_graph$outcome=="no"), "col"] <- pal[2]

  data_for_graph$unit_type <- as.factor(data_for_graph$unit_type)

  lows <- data_for_graph[which(data_for_graph$outcome == "yes"),]
  highs <- data_for_graph[which(data_for_graph$outcome == "no"),]


  p <- ggplot() + geom_bar(data=highs, aes(x = unit_type, y=value, fill=col), position="stack", stat="identity") +
    geom_bar(data=lows, aes(x = unit_type, y=-value, fill=col), position="stack", stat="identity") +
    geom_hline(yintercept = 0, color =c("white")) +
    scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks=pal[-3], guide="legend") +
    theme_fivethirtyeight() +
    coord_flip() +
    labs(title=str_to_title(gsub("_", " ", plot.title)), y="",x="") +
    theme(plot.title = element_text(size=14, hjust=0.5)) +
    theme(axis.text.y = element_text(hjust=0)) +
    theme(legend.position = "bottom") +
    scale_y_continuous(breaks=seq(-100,100,10), limits=c(-100,100)) +
    scale_x_discrete(limits = rev(levels(data_for_graph$unit_type))) +
    theme(plot.title = element_text(size = 11))

  ggsave(paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_", plot.name, ".jpg"), p, width = 10, height = 3.5, dpi = 300)

  write.xlsx(data_for_graph, file = paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/data_", plot.title, ".xlsx"))

}


f.likert.item.by.unit.plot(q44.d,"In the past month, I have been yelled at or threatened by a staff member.",
                           "in_the_past_month_I_have_been_yelled_at_or_threatened_by_a_staff_member")

f.likert.item.by.unit.plot(q46.d,"In the past month, I have been punched, pushed, or kicked by a staff member.",
                           "in_the_past_month_I_have_been_punched_pushed_or_kicked_by_a_staff_member")

f.likert.item.by.unit.plot(q147.d,"In the past month, I have been discriminated by a staff member on this unit.",
                           "in_the_past_month_I_have_been_discriminated_by_a_staff_member")

### Plots with several items, with normal data ####

# Satisfaction with activities ####
numlevels <- 4
pal<-c(brewer.pal(3,"Reds")[c(3,2)], brewer.pal(4,"Greens")[c(2,4)]) # To get a palette going from dark red to light red, to grey (neutral), to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Strongly Disagree", "Disagree", "Agree",  "Strongly Agree")

my.data <- pcq2 %>% filter(survey_wave %in% survey_wave_no)
my.data <- data.frame(service_type = c(rep("q62", nrow(my.data)),
                                       rep("q63", nrow(my.data)),
                                       rep("q64", nrow(my.data)),
                                       rep("q65", nrow(my.data)),
                                       rep("q66", nrow(my.data)),
                                       rep("q67", nrow(my.data)),
                                       rep("q68", nrow(my.data))),
                      value = c(my.data$q62,
                                my.data$q63,
                                my.data$q64,
                                my.data$q65,
                                my.data$q66,
                                my.data$q67,
                                my.data$q68))

data_for_graph <- my.data %>% filter(!is.na(value) & value!=3) %>%
  mutate(strongly_disagree = ifelse(value==1,1,0),
         disagree = ifelse(value==2,1,0),
         agree = ifelse(value==4,1,0),
         strongly_agree = ifelse(value==5,1,0)) %>%
  group_by(service_type) %>%
  summarize(strongly_disagree = mean(strongly_disagree)*100,
            disagree = mean(disagree*100),
            agree = mean(agree*100),
            strongly_agree = mean(strongly_agree*100)) %>%
  pivot_longer(!service_type, names_to = "outcome", values_to = "value")
# arrange(outcome) %>%
# mutate(col = rep(pal,each=length.unit.types))

data_for_graph$col <- NA
data_for_graph[which(data_for_graph$outcome=="strongly_disagree"), "col"] <- pal[1]
data_for_graph[which(data_for_graph$outcome=="disagree"), "col"] <- pal[2]
data_for_graph[which(data_for_graph$outcome=="agree"), "col"] <- pal[3]
data_for_graph[which(data_for_graph$outcome=="strongly_agree"), "col"] <- pal[4]
data_for_graph$service_type <- with(data_for_graph, ifelse(service_type=="q62", ".. the recreation activities", ifelse(
  service_type=="q63", ".. the sports",ifelse(
    service_type=="q64", ".. the library",ifelse(
      service_type=="q65", ".. my work in this institution",ifelse(
        service_type=="q66", ".. the education/courses",ifelse(
          service_type=="q67", ".. the outdoor activity",ifelse(
            service_type=="q68", ".. the religious services", NA))))))))
data_for_graph$service_type <- as.factor(data_for_graph$service_type) # So that I can reverse row order later

lows <- data_for_graph[which(data_for_graph$outcome %in% c("strongly_disagree", "disagree")),]
lows$col <- factor(lows$col, levels = c("#DE2D26", "#FC9272"))
# lows <- lows[rev(rownames(lows)),]
# lows <- lows %>% arrange(outcome)
highs <- data_for_graph[which(data_for_graph$outcome %in% c("agree", "strongly_agree")),]
highs <- as.data.frame(highs)

# Plot
# Note to self: the order of the stackings is determined by the leveling of the col column!!
# See here: https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
p <- ggplot() + geom_bar(data=highs, aes(x = service_type, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = service_type, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks=pal, guide="legend") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title="Satisfaction with Activities",
       subtitle = "I am satisfied with..", y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5),
        plot.subtitle = element_text(size=10, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-75,75,10), limits=c(-75,75)) +
  scale_x_discrete(limits = rev(levels(data_for_graph$service_type)))

ggsave(paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_activities.jpg"), p, width = 10, height = 3.5, dpi = 300)

write.xlsx(data_for_graph, file = paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/data_activities", ".xlsx"))

# Satisfaction with care services ####
numlevels <- 4
pal<-c(brewer.pal(3,"Reds")[c(3,2)], brewer.pal(4,"Greens")[c(2,4)]) # To get a palette going from dark red to light red, to grey (neutral), to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Strongly Disagree", "Disagree", "Agree",  "Strongly Agree")

my.data <- pcq2 %>% filter(survey_wave %in% survey_wave_no)
my.data <- data.frame(service_type = c(rep("q119", nrow(my.data)),
                                       rep("q120", nrow(my.data)),
                                       rep("q121", nrow(my.data)),
                                       rep("q122", nrow(my.data))),
                      value = c(my.data$q119,
                                my.data$q120,
                                my.data$q121,
                                my.data$q122))

data_for_graph <- my.data %>% filter(!is.na(value) & value!=3) %>%
  mutate(strongly_disagree = ifelse(value==1,1,0),
         disagree = ifelse(value==2,1,0),
         agree = ifelse(value==4,1,0),
         strongly_agree = ifelse(value==5,1,0)) %>%
  group_by(service_type) %>%
  summarize(strongly_disagree = mean(strongly_disagree)*100,
            disagree = mean(disagree*100),
            agree = mean(agree*100),
            strongly_agree = mean(strongly_agree*100)) %>%
  pivot_longer(!service_type, names_to = "outcome", values_to = "value")
# arrange(outcome) %>%
# mutate(col = rep(pal,each=length.unit.types))

data_for_graph$col <- NA
data_for_graph[which(data_for_graph$outcome=="strongly_disagree"), "col"] <- pal[1]
data_for_graph[which(data_for_graph$outcome=="disagree"), "col"] <- pal[2]
data_for_graph[which(data_for_graph$outcome=="agree"), "col"] <- pal[3]
data_for_graph[which(data_for_graph$outcome=="strongly_agree"), "col"] <- pal[4]
data_for_graph$service_type <- with(data_for_graph, ifelse(service_type=="q119", "1. The doctor",ifelse(
    service_type=="q120", "2. The nurse",ifelse(
      service_type=="q121", "3. The dentist",ifelse(
        service_type=="q122", "4. The psychologist", NA)))))
data_for_graph$service_type <- as.factor(data_for_graph$service_type) # So that I can reverse row order later

lows <- data_for_graph[which(data_for_graph$outcome %in% c("strongly_disagree", "disagree")),]
lows$col <- factor(lows$col, levels = c("#DE2D26", "#FC9272"))
# lows <- lows[rev(rownames(lows)),]
# lows <- lows %>% arrange(outcome)
highs <- data_for_graph[which(data_for_graph$outcome %in% c("agree", "strongly_agree")),]
highs <- as.data.frame(highs)

# Plot
# Note to self: the order of the stackings is determined by the leveling of the col column!!
# See here: https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
p <- ggplot() + geom_bar(data=highs, aes(x = service_type, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = service_type, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks=pal, guide="legend") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title="Satisfaction with Health Care Services",
       subtitle = "I am satisfied with..", y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5),
        plot.subtitle = element_text(size=10, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-75,75,10), limits=c(-75,75)) +
  scale_x_discrete(limits = rev(levels(data_for_graph$service_type)))

ggsave(paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_care_services.jpg"), p, width = 10, height = 3.5, dpi = 300)

write.xlsx(data_for_graph, file = paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/data_care_services", ".xlsx"))

# Satisfaction with visits ####
numlevels <- 4
pal<-c(brewer.pal(3,"Reds")[c(3,2)], brewer.pal(4,"Greens")[c(2,4)]) # To get a palette going from dark red to light red, to grey (neutral), to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Strongly Disagree", "Disagree", "Agree",  "Strongly Agree")

my.data <- pcq2 %>% filter(survey_wave %in% survey_wave_no)
my.data <- data.frame(service_type = c(rep("q139", nrow(my.data)),
                                       rep("q143", nrow(my.data)),
                                       rep("q141", nrow(my.data)),
                                       rep("q144", nrow(my.data))),
                      value = c(my.data$q139,
                                my.data$q143,
                                my.data$q141,
                                my.data$q144))

data_for_graph <- my.data %>% filter(!is.na(value) & value!=3) %>%
  mutate(strongly_disagree = ifelse(value==1,1,0),
         disagree = ifelse(value==2,1,0),
         agree = ifelse(value==4,1,0),
         strongly_agree = ifelse(value==5,1,0)) %>%
  group_by(service_type) %>%
  summarize(strongly_disagree = mean(strongly_disagree)*100,
            disagree = mean(disagree*100),
            agree = mean(agree*100),
            strongly_agree = mean(strongly_agree*100)) %>%
  pivot_longer(!service_type, names_to = "outcome", values_to = "value")
# arrange(outcome) %>%
# mutate(col = rep(pal,each=length.unit.types))

data_for_graph$col <- NA
data_for_graph[which(data_for_graph$outcome=="strongly_disagree"), "col"] <- pal[1]
data_for_graph[which(data_for_graph$outcome=="disagree"), "col"] <- pal[2]
data_for_graph[which(data_for_graph$outcome=="agree"), "col"] <- pal[3]
data_for_graph[which(data_for_graph$outcome=="strongly_agree"), "col"] <- pal[4]
data_for_graph$service_type <- with(data_for_graph, ifelse(service_type=="q139", "1. The visiting room is pleasant",ifelse(
  service_type=="q143", "2. The staff treat my visitors nicely",ifelse(
    service_type=="q144", "3. The visiting hours are frequent enough",ifelse(
      service_type=="q141", "4. The visiting hours are long enough", NA)))))
data_for_graph$service_type <- as.factor(data_for_graph$service_type) # So that I can reverse row order later

lows <- data_for_graph[which(data_for_graph$outcome %in% c("strongly_disagree", "disagree")),]
lows$col <- factor(lows$col, levels = c("#DE2D26", "#FC9272"))
# lows <- lows[rev(rownames(lows)),]
# lows <- lows %>% arrange(outcome)
highs <- data_for_graph[which(data_for_graph$outcome %in% c("agree", "strongly_agree")),]
highs <- as.data.frame(highs)

# Plot
# Note to self: the order of the stackings is determined by the leveling of the col column!!
# See here: https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
p <- ggplot() + geom_bar(data=highs, aes(x = service_type, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = service_type, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks=pal, guide="legend") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title="Satisfaction with Visits",
       subtitle = "I am satisfied with..", y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5),
        plot.subtitle = element_text(size=10, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-75,75,10), limits=c(-75,75)) +
  scale_x_discrete(limits = rev(levels(data_for_graph$service_type)))

ggsave(paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_visits.jpg"), p, width = 10, height = 3.5, dpi = 300)

write.xlsx(data_for_graph, file = paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/data_visits", ".xlsx"))

# Satisfaction with grievance system ####
numlevels <- 4
pal<-c(brewer.pal(3,"Reds")[c(3,2)], brewer.pal(4,"Greens")[c(2,4)]) # To get a palette going from dark red to light red, to grey (neutral), to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Strongly Disagree", "Disagree", "Agree",  "Strongly Agree")

my.data <- pcq2 %>% filter(survey_wave %in% survey_wave_no)
my.data <- data.frame(service_type = c(rep("q24", nrow(my.data)),
                                       rep("q25", nrow(my.data))),
                      value = c(my.data$q24,
                                my.data$q25))

data_for_graph <- my.data %>% filter(!is.na(value) & value!=3) %>%
  mutate(strongly_disagree = ifelse(value==1,1,0),
         disagree = ifelse(value==2,1,0),
         agree = ifelse(value==4,1,0),
         strongly_agree = ifelse(value==5,1,0)) %>%
  group_by(service_type) %>%
  summarize(strongly_disagree = mean(strongly_disagree)*100,
            disagree = mean(disagree*100),
            agree = mean(agree*100),
            strongly_agree = mean(strongly_agree*100)) %>%
  pivot_longer(!service_type, names_to = "outcome", values_to = "value")
# arrange(outcome) %>%
# mutate(col = rep(pal,each=length.unit.types))

data_for_graph$col <- NA
data_for_graph[which(data_for_graph$outcome=="strongly_disagree"), "col"] <- pal[1]
data_for_graph[which(data_for_graph$outcome=="disagree"), "col"] <- pal[2]
data_for_graph[which(data_for_graph$outcome=="agree"), "col"] <- pal[3]
data_for_graph[which(data_for_graph$outcome=="strongly_agree"), "col"] <- pal[4]
data_for_graph$service_type <- with(data_for_graph,
                                    ifelse(service_type=="q24", "Grievances are taken seriously here",ifelse(
                                      service_type=="q26", "The grievance system works well here", NA)))
data_for_graph$service_type <- as.factor(data_for_graph$service_type) # So that I can reverse row order later

lows <- data_for_graph[which(data_for_graph$outcome %in% c("strongly_disagree", "disagree")),]
lows$col <- factor(lows$col, levels = c("#DE2D26", "#FC9272"))
highs <- data_for_graph[which(data_for_graph$outcome %in% c("agree", "strongly_agree")),]
highs <- as.data.frame(highs)

# Plot
# Note to self: the order of the stackings is determined by the leveling of the col column!!
# See here: https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
p <- ggplot() + geom_bar(data=highs, aes(x = service_type, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = service_type, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks=pal, guide="legend") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title="Satisfaction with the Grievance System", y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-100,100,10), limits=c(-100,100)) +
  scale_x_discrete(limits = rev(levels(data_for_graph$service_type)))

ggsave(paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_grievances.jpg"), p, width = 10, height = 3.5, dpi = 300)

write.xlsx(data_for_graph, file = paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/data_grievances", ".xlsx"))

# Satisfaction with food ####
numlevels <- 4
pal<-c(brewer.pal(3,"Reds")[c(3,2)], brewer.pal(4,"Greens")[c(2,4)]) # To get a palette going from dark red to light red, to grey (neutral), to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Strongly Disagree", "Disagree", "Agree",  "Strongly Agree")

my.data <- pcq2 %>% filter(survey_wave %in% survey_wave_no)
my.data <- data.frame(service_type = c(rep("q98", nrow(my.data)),
                                       rep("q99", nrow(my.data)),
                                       rep("q100", nrow(my.data)),
                                       rep("q101", nrow(my.data))),
                      value = c(my.data$q98,
                                my.data$q99,
                                my.data$q100,
                                my.data$q101))

data_for_graph <- my.data %>% filter(!is.na(value) & value!=3) %>%
  mutate(strongly_disagree = ifelse(value==1,1,0),
         disagree = ifelse(value==2,1,0),
         agree = ifelse(value==4,1,0),
         strongly_agree = ifelse(value==5,1,0)) %>%
  group_by(service_type) %>%
  summarize(strongly_disagree = mean(strongly_disagree)*100,
            disagree = mean(disagree*100),
            agree = mean(agree*100),
            strongly_agree = mean(strongly_agree*100)) %>%
  pivot_longer(!service_type, names_to = "outcome", values_to = "value")
# arrange(outcome) %>%
# mutate(col = rep(pal,each=length.unit.types))

data_for_graph$col <- NA
data_for_graph[which(data_for_graph$outcome=="strongly_disagree"), "col"] <- pal[1]
data_for_graph[which(data_for_graph$outcome=="disagree"), "col"] <- pal[2]
data_for_graph[which(data_for_graph$outcome=="agree"), "col"] <- pal[3]
data_for_graph[which(data_for_graph$outcome=="strongly_agree"), "col"] <- pal[4]
data_for_graph$service_type <- with(data_for_graph, ifelse(service_type=="q98", ".. the food that I get in the dining hall",ifelse(
    service_type=="q99", ".. the range of products in the commissary",ifelse(
      service_type=="q100", ".. the prices in the commissary",ifelse(
        service_type=="q101", ".. the quality of products in the commissary", NA)))))
data_for_graph$service_type <- as.factor(data_for_graph$service_type) # So that I can reverse row order later

lows <- data_for_graph[which(data_for_graph$outcome %in% c("strongly_disagree", "disagree")),]
lows$col <- factor(lows$col, levels = c("#DE2D26", "#FC9272"))
# lows <- lows[rev(rownames(lows)),]
# lows <- lows %>% arrange(outcome)
highs <- data_for_graph[which(data_for_graph$outcome %in% c("agree", "strongly_agree")),]
highs <- as.data.frame(highs)

# Plot
# Note to self: the order of the stackings is determined by the leveling of the col column!!
# See here: https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
p <- ggplot() + geom_bar(data=highs, aes(x = service_type, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = service_type, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks=pal, guide="legend") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title="Satisfaction with Food",
       subtitle = "I am satisfied with..", y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5),
        plot.subtitle = element_text(size=10, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-100,100,10), limits=c(-100,100)) +
  scale_x_discrete(limits = rev(levels(data_for_graph$service_type)))

ggsave(paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_food.jpg"), p, width = 10, height = 3.5, dpi = 300)

write.xlsx(data_for_graph, file = paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/data_food", ".xlsx"))

# Satisfaction with staff ####
numlevels <- 4
pal<-c(brewer.pal(3,"Reds")[c(3,2)], brewer.pal(4,"Greens")[c(2,4)]) # To get a palette going from dark red to light red, to grey (neutral), to light green, to dark green
length.unit.types <- 7 # excluding NA and INF
mylevels<-c("Strongly Disagree", "Disagree", "Agree",  "Strongly Agree")

my.data <- pcq2 %>% filter(survey_wave %in% survey_wave_no)
my.data <- data.frame(service_type = c(rep("q15", nrow(my.data)),
                                       rep("q16", nrow(my.data)),
                                       rep("q17", nrow(my.data)),
                                       rep("q18", nrow(my.data)),
                                       rep("q19", nrow(my.data)),
                                       rep("q20", nrow(my.data)),
                                       rep("q21", nrow(my.data)),
                                       rep("q23", nrow(my.data))),
                      value = c(my.data$q15,
                                my.data$q16,
                                my.data$q17,
                                my.data$q18,
                                my.data$q19,
                                my.data$q20,
                                my.data$q21,
                                my.data$q23))

data_for_graph <- my.data %>% filter(!is.na(value) & value!=3) %>%
  mutate(strongly_disagree = ifelse(value==1,1,0),
         disagree = ifelse(value==2,1,0),
         agree = ifelse(value==4,1,0),
         strongly_agree = ifelse(value==5,1,0)) %>%
  group_by(service_type) %>%
  summarize(strongly_disagree = mean(strongly_disagree)*100,
            disagree = mean(disagree*100),
            agree = mean(agree*100),
            strongly_agree = mean(strongly_agree*100)) %>%
  pivot_longer(!service_type, names_to = "outcome", values_to = "value")
# arrange(outcome) %>%
# mutate(col = rep(pal,each=length.unit.types))

data_for_graph$col <- NA
data_for_graph[which(data_for_graph$outcome=="strongly_disagree"), "col"] <- pal[1]
data_for_graph[which(data_for_graph$outcome=="disagree"), "col"] <- pal[2]
data_for_graph[which(data_for_graph$outcome=="agree"), "col"] <- pal[3]
data_for_graph[which(data_for_graph$outcome=="strongly_agree"), "col"] <- pal[4]
data_for_graph$service_type <- with(data_for_graph, ifelse(service_type=="q20", "1. explain their decisions to me",ifelse(
  service_type=="q18", "2. motivate me to participate in activities",ifelse(
    service_type=="q17", "3. are there to talk to if I feel worried or sad",ifelse(
      service_type=="q15", "4. help me if I have problems",ifelse(
        service_type=="q21", "5. treat me with respect",ifelse(
          service_type=="q19", "6. treat me fairly",ifelse(
            service_type=="q16", "7. are kind to me",ifelse(
              service_type=="q23", "8. are in control of this unit", NA)))))))))
data_for_graph$service_type <- as.factor(data_for_graph$service_type) # So that I can reverse row order later

lows <- data_for_graph[which(data_for_graph$outcome %in% c("strongly_disagree", "disagree")),]
lows$col <- factor(lows$col, levels = c("#DE2D26", "#FC9272"))
# lows <- lows[rev(rownames(lows)),]
# lows <- lows %>% arrange(outcome)
highs <- data_for_graph[which(data_for_graph$outcome %in% c("agree", "strongly_agree")),]
highs <- as.data.frame(highs)

# Plot
# Note to self: the order of the stackings is determined by the leveling of the col column!!
# See here: https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
p <- ggplot() + geom_bar(data=highs, aes(x = service_type, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = service_type, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity(paste0("Wave: ", paste(survey_wave_no, collapse = "&")), labels = mylevels, breaks=pal, guide="legend") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(title="Satisfaction with Staff",
       subtitle = "Staff on this unit..", y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5),
        plot.subtitle = element_text(size=10, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-90,90,10), limits=c(-90,90)) +
  scale_x_discrete(limits = rev(levels(data_for_graph$service_type)))

ggsave(paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/plot_staff.jpg"), p, width = 10, height = 3.5, dpi = 300)

write.xlsx(data_for_graph, file = paste0("output/dedicated_analyses/chs_report/item_plots/survey_wave_", paste(survey_wave_no, collapse = "_"), "/data_staff", ".xlsx"))


}
