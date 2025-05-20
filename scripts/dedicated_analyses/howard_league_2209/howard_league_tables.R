library(lubridate)
library(xtable)
library(dplyr)
library(writexl)
load("data/inputs/basic_cleaned.Rda")
load("data/inputs/house_cleaned.Rda")
load("data/inputs/pcq.Rda")
load("data/inputs/idkey.Rda")
`%ni%` = Negate(`%in%`)

## Location experimental group ####
temp <- left_join(house[house$location_at_pcqwave1==1,],
                        idkey[,c("research_id", "treated")])
tab <- data.frame(table(temp$unit))
tab <- left_join(tab,data.frame(table(temp[temp$treated %in% c(1,0),]$unit)), by="Var1")
tab <- left_join(tab,data.frame(table(temp[temp$treated==1,]$unit)), by="Var1")
tab <- left_join(tab,data.frame(table(temp[temp$treated==0,]$unit)), by="Var1")
names(tab) <- c("unit","population" ,"exp_group", "treated","control")
tab$share_in_exp_group <- tab$exp_group/tab$population
tab$share_treated <- tab$treated/tab$exp_group
tab <- left_join(tab, unique(house[,c("unit_type", "unit")]), by = "unit")
tab <- tab[,c(1,8,2:7)]
for(i in 3:8){
tab[,i] <- as.numeric(tab[,i])
}
# write_xlsx(tab,"output/experimental_group_by_location.xlsx")

# Link Unit Location & Treatment Status to basic
# Unit location
index <- unique(house[house$location_at_pcqwave1==1,c("research_id", "unit_type")])
basic <- left_join(basic,index)

# Treatment status 
index <- unique(idkey[,c("research_id", "treated")])
basic <- left_join(basic,index)


# Summaries & Table ####
f.descriptives <- function(data){
lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}

vars <- c("min_sent_yrs", "max_sent_yrs")
data <- basic[basic$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
tab <- t(as.data.frame(f.descriptives(data)))

vars <- c("life","past_min","age_at_pcqwave1", "high_school", "race_black", "race_white")
data <- basic[vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))

vars <- c("unit_type_gp", "unit_type_gptc", "unit_type_thu", "unit_type_hons", "unit_type_ls", "unit_type_rhu", "unit_type_rec", "unit_type_inf")
data <- house[house$location_at_pcqwave1==1,vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
tab <- as.data.frame(tab)
names(tab) <- c("n","min", "max","mean","SD")
row.names(tab) <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Life Sentences",
                    "Past Min",
                    "Age",
                    "Education: High School or Higher",
                    "Black",
                    "White",
                    "General Population",
                    "Therapeutic Community",
                    "Transitional Housing Unit",
                    "Honor Block",
                    "Little Scandinavia",
                    "Restricted Housing Unit",
                    "Recovery Unit",
                    "Infirmary")
  
# Table

print(xtable(tab, digits=c(0,0,0,2,2), caption="Surveyed Population Characteristics"),include.rownames=TRUE, file = "output/tables/surveyed_population_characteristics.txt") 

tab_pop <- tab

# Summaries & Table for respondents only ####
ids <- unique(house[house$research_id %in% unique(pcq$research_id),]$research_id)
f.descriptives <- function(data){
  lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}

basic_temp <- basic[basic$research_id %in% ids,]
house_temp <- house[house$research_id %in% ids,]
vars <- c("min_sent_yrs", "max_sent_yrs")
data <- basic_temp[basic_temp$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
tab <- t(as.data.frame(f.descriptives(data)))

vars <- c("life","past_min","age_at_pcqwave1", "high_school", "race_black", "race_white")
data <- basic_temp[vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))

vars <- c("unit_type_gp", "unit_type_gptc", "unit_type_thu", "unit_type_hons", "unit_type_ls", "unit_type_rhu", "unit_type_rec", "unit_type_inf")
data <- house_temp[house_temp$location_at_pcqwave1==1,vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
tab <- as.data.frame(tab)
names(tab) <- c("n","min", "max","mean","SD")
row.names(tab) <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Life Sentences",
                    "Past Min",
                    "Age",
                    "Education: High School or Higher",
                    "Black",
                    "White",
                    "General Population",
                    "Therapeutic Community",
                    "Transitional Housing Unit",
                    "Honor Block",
                    "Little Scandinavia",
                    "Restricted Housing Unit",
                    "Recovery Unit",
                    "Infirmary")

# Table

print(xtable(tab, digits=c(0,0,2,2,2,2), caption="Prison Population Characteristics"),include.rownames=TRUE) 

tab_resp <- tab

tab <- data.frame(cbind(tab_pop, tab_resp))
names(tab)
tab <- tab[,-which(names(tab) %in% c("min", "max", "min.1", "max.1"))]
names(tab)[which(names(tab) %in% c("n.1", "mean.1", "SD.1"))] <- c("n", "mean", "SD")
print(xtable(tab, digits=c(0,0,2,2,0,2,2), caption="Prison Population Characteristics"),include.rownames=TRUE) 

# Summaries & Table for genpop & exp group ####
f.descriptives <- function(data){
  lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}

# genpop
vars <- c("min_sent_yrs", "max_sent_yrs")
data <- basic[basic$unit_type=="gp" &
                basic$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
tab <- t(as.data.frame(f.descriptives(data)))

vars <- c("life","past_min","age_at_pcqwave1", "high_school", "race_black", "race_white")
data <- basic[basic$unit_type=="gp",vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))


tab <- as.data.frame(tab)
names(tab) <- c("n","min", "max","mean","SD")
row.names(tab) <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Life Sentences",
                    "Past Min",
                    "Age",
                    "Education: High School or Higher",
                    "Black",
                    "White")

tab_genpop <- tab

# experimental group 
ids <- unique(basic[which(basic$treated %in% c(0,1)),]$research_id)
f.descriptives <- function(data){
  lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}

basic_temp <- basic[basic$research_id %in% ids,]
house_temp <- house[house$research_id %in% ids,]
vars <- c("min_sent_yrs", "max_sent_yrs")
data <- basic_temp[basic_temp$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
tab <- t(as.data.frame(f.descriptives(data)))

vars <- c("life","past_min","age_at_pcqwave1", "high_school", "race_black", "race_white")
data <- basic_temp[vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))

tab <- as.data.frame(tab)
names(tab) <- c("n","min", "max","mean","SD")
row.names(tab) <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Life Sentences",
                    "Past Min",
                    "Age",
                    "Education: High School or Higher",
                    "Black",
                    "White")

tab_exp <- tab

# treated group 
ids <- unique(basic[which(basic$treated %in% c(1)),]$research_id)
f.descriptives <- function(data){
  lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}

basic_temp <- basic[basic$research_id %in% ids,]
house_temp <- house[house$research_id %in% ids,]
vars <- c("min_sent_yrs", "max_sent_yrs")
data <- basic_temp[basic_temp$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
tab <- t(as.data.frame(f.descriptives(data)))

vars <- c("life","past_min","age_at_pcqwave1", "high_school", "race_black", "race_white")
data <- basic_temp[vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))

tab <- as.data.frame(tab)
names(tab) <- c("n","min", "max","mean","SD")
row.names(tab) <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Life Sentences",
                    "Past Min",
                    "Age",
                    "Education: High School or Higher",
                    "Black",
                    "White")

tab_treated <- tab

# control group 
ids <- unique(basic[which(basic$treated %in% c(0)),]$research_id)
f.descriptives <- function(data){
  lapply(data, function(x) c(length(which(!is.na(x))),range(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}

basic_temp <- basic[basic$research_id %in% ids,]
house_temp <- house[house$research_id %in% ids,]
vars <- c("min_sent_yrs", "max_sent_yrs")
data <- basic_temp[basic_temp$sentence_class %ni% c("LIFE", "COMMUTED_LIFE"),vars]
tab <- t(as.data.frame(f.descriptives(data)))

vars <- c("life","past_min","age_at_pcqwave1", "high_school", "race_black", "race_white")
data <- basic_temp[vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))

tab <- as.data.frame(tab)
names(tab) <- c("n","min", "max","mean","SD")
row.names(tab) <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Life Sentences",
                    "Past Min",
                    "Age",
                    "Education: High School or Higher",
                    "Black",
                    "White")

tab_control <- tab


# Table
tab <- data.frame(cbind(tab_genpop, tab_exp, tab_treated, tab_control))
names(tab)
tab <- tab[,which(names(tab) %in% c("mean", "mean.1", "mean.2", "mean.3"))]
names(tab)<- c("General Population", "Experimental Group", "Treated", "Control")
print(xtable(tab, digits=c(0,2,2,2,2), caption="Prison Population Characteristics For General Population and Experimental Group"),include.rownames=TRUE) 

# Summaries & Table by unit ####
f.descriptives <- function(data){
  lapply(data, function(x) c(mean(x, na.rm=TRUE)))
}
# For all
vars <- c("min_sent_yrs", "max_sent_yrs")
data <- basic[which(basic$sentence_class %ni% c("LIFE", "COMMUTED_LIFE")),vars]
tab <- t(as.data.frame(f.descriptives(data)))

vars <- c("life","past_min","age_at_pcqwave1", "high_school", "race_black", "race_white")
data <- basic[vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
tab <- rbind(tab,n=nrow(data))
tab <- as.data.frame(tab)
tab$unit_type <-"all"
tab$idvar <- rownames(tab)
names(tab) <- c("mean", "unit_type", "idvar")
keep <- tab
index <- unique(house[house$unit_type %ni% c(NA,"inf"),]$unit_type)
for(i in index){
vars <- c("min_sent_yrs", "max_sent_yrs")
data <- basic[which(basic$unit_type==i &
                basic$sentence_class %ni% c("LIFE", "COMMUTED_LIFE")),vars]
tab <- t(as.data.frame(f.descriptives(data)))

vars <- c("life","past_min","age_at_pcqwave1", "high_school", "race_black", "race_white")
data <- basic[which(basic$unit_type==i), vars]
tab <- rbind(tab,t(as.data.frame(f.descriptives(data))))
tab <- rbind(tab,n=nrow(data))
tab <- as.data.frame(tab)
tab$unit_type <-i
tab$idvar <- rownames(tab)
names(tab) <- c("mean", "unit_type", "idvar")
keep <- rbind(keep,tab)
}

keep <- reshape(data=keep,idvar="idvar",
                timevar = "unit_type",
                direction="wide")

names(keep) <- gsub("mean.","",names(keep))
keep <- keep[,names(keep)!="idvar"]
for(i in 1:ncol(keep)){
  keep[,i] <- as.numeric(keep[,i])
}
keep[nrow(keep)+1,] <- keep[nrow(keep),]/keep[which(row.names(keep)=="n"),"all"]

row.names(keep) <- c("Minimum Sentence (in years)*",
                    "Maximum Sentence (in years)*",
                    "Life Sentences",
                    "Past Min",
                    "Age",
                    "Education: High School or Higher",
                    "Black",
                    "White",
                    "Population",
                    "Population Share")

# Table (without LS)
keep <- keep[,c(1,2,6,8,4,3,5,7)]
print(xtable(keep[,-which(names(keep)=="ls")], digits=c(0,2,2,2,2,2,2,2), caption="Prison Population Characteristics, by Unit Type"),include.rownames=TRUE) 


# Table with LSU pre and Post --------------------------------------------- ####
ids <- unique(pcq[pcq$survey_wave==2,]$research_id)
length(ids) #21
lsu_comparison <- pcq[pcq$research_id %in% ids,]
temp <- data.frame(table(lsu_comparison$research_id)) # two measurements for 19 individuals 
ids <- temp[temp$Freq>1,]$Var1
names(pcq)

pcq <- pcq[which(pcq$research_id %in% ids),]
pcq2 <- pcq2[which(pcq2$research_id %in% ids),]

# Hold ###
hold <- pcq
hold2 <- pcq2


# For wave 1 ####
pcq <- pcq[pcq$survey_wave==1,]
pcq2 <- pcq2[pcq2$survey_wave==1,]

scales.pc <- unique(pcq_lookup[,c("scale", "pc")])
keep <- data.frame(mean = rep("boo", nrow(scales.pc)),
                   sd = rep("boo", nrow(scales.pc)),
                   scale = rep("boo", nrow(scales.pc)),
                   pc=rep("boo", nrow(scales.pc)))

for(i in scales.pc$scale){
  for(k in scales.pc[scales.pc$scale==i,]$pc){
    # Means and SDs
    ## Identify scales and accompanying question numbers 
    qnos <- data.frame(no = unique(pcq_lookup[pcq_lookup$scale==i & pcq_lookup$pc==k,"question_no"]), 
                       qno = NA)
    qnos$qno <- paste0("q",qnos$no)
    
    rows.without.nas <- which(complete.cases(pcq[,qnos$qno])==TRUE) # Complete cases include those that have no opinion or not applicable answers, but exclude questions left blank
    
    # Calculate mean of identified questions that were answered for each individual 
    pcq$ind.mean <- NA
    pcq[rows.without.nas,]$ind.mean <- rowSums(pcq2[rows.without.nas,qnos$qno], na.rm=TRUE) #summing answered questions
    pcq$no_completed_qs <- NA #Calulate number of answered questions that were not 'no opinion' (cannot count these for the average)
    for(j in 1:nrow(pcq)){
      pcq[j,]$no_completed_qs <- length(qnos$qno)-length(which(pcq[j,qnos$qno]==111)) 
    }
    pcq$ind.mean <- ifelse(!is.na(pcq$ind.mean),pcq$ind.mean/pcq$no_completed_qs, pcq$ind.mean)
    
    # Calculate sds of identified questions that were answered for each individual 
    pcq$ind.sd <- NA
    pcq[rows.without.nas,]$ind.sd <- rowSds(as.matrix(pcq2[rows.without.nas,qnos$qno]), na.rm=TRUE)
    
    ## Calculate means of individual means/sd's 
    temp <- pcq %>% summarize(across(c(ind.mean, ind.sd), ~ mean(.x, na.rm = TRUE))) %>% mutate(scale = i,pc = k) %>%
      `colnames<-`(c("mean", "sd", "scale","pc"))
    keep[which(scales.pc$scale==i & scales.pc$pc==k),c("mean", "sd", "scale","pc")] <- temp
  }
}

keep_wave1 <- keep
keep_wave1$survey_wave <- 1

# For wave 2 ####
pcq <- hold
pcq2 <- hold2

pcq <- pcq[pcq$survey_wave==2,]
pcq2 <- pcq2[pcq2$survey_wave==2,]


scales.pc <- unique(pcq_lookup[,c("scale", "pc")])
keep <- data.frame(mean = rep("boo", nrow(scales.pc)),
                   sd = rep("boo", nrow(scales.pc)),
                   pc=rep("boo", nrow(scales.pc)))

for(i in scales.pc$scale){
  for(k in scales.pc[scales.pc$scale==i,]$pc){
    # Means and SDs
    ## Identify scales and accompanying question numbers 
    qnos <- data.frame(no = unique(pcq_lookup[pcq_lookup$scale==i & pcq_lookup$pc==k,"question_no"]), 
                       qno = NA)
    qnos$qno <- paste0("q",qnos$no)
    
    rows.without.nas <- which(complete.cases(pcq[,qnos$qno])==TRUE) # Complete cases include those that have no opinion or not applicable answers, but exclude questions left blank
    
    # Calculate mean of identified questions that were answered for each individual 
    pcq$ind.mean <- NA
    pcq[rows.without.nas,]$ind.mean <- rowSums(pcq2[rows.without.nas,qnos$qno], na.rm=TRUE) #summing answered questions
    pcq$no_completed_qs <- NA #Calulate number of answered questions that were not 'no opinion' (cannot count these for the average)
    for(j in 1:nrow(pcq)){
      pcq[j,]$no_completed_qs <- length(qnos$qno)-length(which(pcq[j,qnos$qno]==111)) 
    }
    pcq$ind.mean <- ifelse(!is.na(pcq$ind.mean),pcq$ind.mean/pcq$no_completed_qs, pcq$ind.mean)
    
    # Calculate sds of identified questions that were answered for each individual 
    pcq$ind.sd <- NA
    pcq[rows.without.nas,]$ind.sd <- rowSds(as.matrix(pcq2[rows.without.nas,qnos$qno]), na.rm=TRUE)
    
    ## Calculate means of individual means/sd's 
    temp <- pcq %>% summarize(across(c(ind.mean, ind.sd), ~ mean(.x, na.rm = TRUE))) %>% mutate(scale = i,pc = k) %>%
      `colnames<-`(c("mean", "sd", "scale","pc"))
    keep[which(scales.pc$scale==i & scales.pc$pc==k),c("mean", "sd", "scale","pc")] <- temp
  }
}

# Combine dataframes ####
keep_wave2 <- keep
keep_wave2$survey_wave <- 2

keep <- cbind(keep_wave1, keep_wave2)
keep <- keep[keep$pc=="pc",]
keep <- keep[,c(3,1,6)]
keep

keep <- left_join(keep,unique(pcq_lookup[pcq_lookup$pc=="pc",c("scale", "domain", "scale_long")]))
index <- rev(order(keep$domain))
keep <- keep[index,]
keep <- keep[,-which(names(keep) %in% c("domain", "scale"))]
keep <- keep[,c(3,1,2)]
for(i in 2:3){
  keep[,i] <- as.numeric(keep[,i])
}
names(keep) <- c("Scale", "Before Move", "3 Months SPP")
print(xtable(keep, digits=c(0,0,2,2), caption="Descriptive Statistics for Treated Individuals"),include.rownames=FALSE) 

