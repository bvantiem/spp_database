# Functions for descriptives table ####
# Function to calculate n, range, mean and sd
rownames.table <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Time Served*",
                    "Life",
                    "Age",
                    "Education: High School or Higher",
                    "White",
                    "Black",
                    "Violent",
                    "Property",
                    "Drugs",
                    "Public Order",
                    "Children",
                    "Double Cell",
                    "Country of Origin",
                    "General Population",
                    "Therapeutic Community",
                    "Transitional Housing Unit",
                    "Honor Block",
                    "Little Scandinavia",
                    "Restricted Housing Unit",
                    "Recovery Unit")
f.descriptives <- function(data){
  lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}
f.tab <- function(basic_temp, house_temp, pcq_temp){
  # Calculate characteristics for selected variables 
  vars <- c("min_sent_days", "max_sent_days", "est_time_served_on_20220501")
  data <- basic_temp[basic_temp$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
  data$min_sent_days <- data$min_sent_days/365
  data$max_sent_days <- data$max_sent_days/365
  data$est_time_served_on_20220501 <- data$est_time_served_on_20220501/365
  tab <- t(as.data.frame(f.descriptives(data)))
  
  vars <- c("life","age_on_20220501","high_school", "race_white", "race_black",
            "violent_offense", "property_offense", "drugs_offense", "publicorder_offense")
  data <- basic_temp[vars]
  tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
  
  pcq_temp$children <- ifelse(pcq_temp$q167==1, 0, # do you have children?
                              ifelse(pcq_temp$q167==2, 1, NA))
  pcq_temp$cell <- ifelse(pcq_temp$q90==1, 1, # do you share a cell?
                          ifelse(pcq_temp$q90==2, 0, NA))
  pcq_temp$country_of_origin <- ifelse(pcq_temp$q70==1, 1, # In what country were you born?
                          ifelse(pcq_temp$q90==2, 0, NA))
  vars <- c("children", "cell", "country_of_origin")
  data <- pcq_temp[vars]
  tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
  
  vars <- c("unit_type_gp", "unit_type_gp-tc", "unit_type_thu", "unit_type_hons", "unit_type_ls", "unit_type_rhu", "unit_type_rec")
  data <- fastDummies::dummy_cols(data.frame(unit_type = house_temp$unit_type))
  data <- data[,vars]
  tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
  tab <- as.data.frame(tab)
  
  # Build table 
  tab <- as.data.frame(tab)
  names(tab) <- c("n","min", "max","mean","SD")
  row.names(tab) <- rownames.table
  tab$range <- paste0("(",round(tab$min,2), ",", round(tab$max,2), ")")
  tab <- tab %>% dplyr::select(-min, -max)
  tab <- tab[,c("n","range","mean", "SD")]
  return(tab)
}
# Population characteristics ####
# Data for population
basic_temp <- basic[which(basic$date_datapull %in% c(ymd(20220625), ymd(20220903))),]

house_temp <- house[which(house$date_datapull %in% c(ymd(20220625), ymd(20220903)) &
                            house$location_at_pcqwave1==1),]
pcq_temp <- pcq_full[which(pcq_full$research_id %in% unique(basic_temp$research_id) &
                             pcq_full$survey_wave==1),]

# Deduplicate for individuals for whom I have data from both datapulls 
basic_temp$pull_no <- 1
temp <- data.frame(table(basic_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
basic_temp[basic_temp$research_id %in% index & basic_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
basic_temp <- basic_temp[which(basic_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 

house_temp$pull_no <- 1
temp <- data.frame(table(house_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
house_temp[house_temp$research_id %in% index & house_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
house_temp <- house_temp[which(house_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 

tab <- f.tab(basic_temp, house_temp, pcq_temp)
tab1 <- tab # save for combined table later

print(xtable(tab, digits=c(0,0,0,2,2), caption="Surveyed Population Characteristics"),include.rownames=TRUE,
      file="output/tables/characteristics_surveyed_population.txt") 

# Survey respondents characteristics ####
# Data
basic_temp <- basic[which(basic$research_id %in% unique(pcq$research_id) &
                            basic$date_datapull %in% c(ymd(20220625), ymd(20220903))),]

house_temp <- house[which(house$research_id %in% unique(pcq$research_id) &
                            house$date_datapull %in% c(ymd(20220625), ymd(20220903)) &
                            house$location_at_pcqwave1==1),]

# Deduplicate for individuals for whom I have data from both datapulls 
basic_temp$pull_no <- 1
temp <- data.frame(table(basic_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
basic_temp[basic_temp$research_id %in% index & basic_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
basic_temp <- basic_temp[which(basic_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 

house_temp$pull_no <- 1
temp <- data.frame(table(house_temp$research_id))
index <- temp[temp$Freq>1,"Var1"]
house_temp[house_temp$research_id %in% index & house_temp$date_datapull==ymd(20220903),"pull_no"] <- 2 
house_temp <- house_temp[which(house_temp$pull_no==1),] # Drops data from the second datapull on these individiduals 


tab <- f.tab(basic_temp, house_temp, pcq_temp)
tab2 <- tab # save for combined table later

print(xtable(tab, digits=c(0,0,0,2,2), caption="Survey Respondent Characteristics"),include.rownames=TRUE,
      file="output/tables/characteristics_survey_respondents.txt") 

tab <- cbind(tab1$mean, tab2$mean)
row.names(tab) <- rownames.table
print(xtable(tab, digits=c(0,2,2), caption="Survey Respondents Characteristics"),include.rownames=TRUE,
      file="output/tables/characteristics_population_and_respondents.txt") 


# Build in with Dutch table
nl_tab1 <- data.frame(variable = c("Age",
                                   "Male",
                                   "Education: High School or Higher",
                                   "Children",
                                   "White",
                                   "Black",
                                   "Has Partner (PA) / Married (NL)",
                                   "Country of Birth: NL/PA",
                                   "Violent",
                                   "Property",
                                   "Drugs",
                                   "Public Order",
                                   "Double Cell",
                                   "General Population",
                                   "Therapeutic Community",
                                   "Transitional Housing Unit",
                                   "Honor Block",
                                   "Little Scandinavia",
                                   "Restricted Housing Unit",
                                   "Recovery Unit")
                      
                      