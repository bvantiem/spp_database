library(lubridate)
library(xtable)
library(dplyr)
library(writexl)
library(psych)
library(psychTools)
library(matrixStats)
library(ggplot2)

load("data/inputs/basic_cleaned.Rda")
load("data/inputs/house_cleaned.Rda")
load("data/inputs/pcq.Rda")
load("data/inputs/pcq2.Rda")
load("data/inputs/pcq_lookup_factors.Rda")
load("data/inputs/idkey.Rda")
`%ni%` = Negate(`%in%`)

# Subset for histograms ####
pcq <- subset(pcq, survey_wave==1)
pcq2 <- subset(pcq2, survey_wave==1)

# Link 
matched.row <- match(pcq$research_id, house$research_id)
pcq$unit_type <- house$unit_type[matched.row]
pcq2$unit_type <- house$unit_type[matched.row]

matched.row <- match(pcq$research_id, basic$research_id)
pcq$time_to_min <- basic$time_to_min[matched.row]
pcq2$time_to_min <- basic$time_to_min[matched.row]

matched.row <- match(pcq$research_id, basic$research_id)
pcq$min_sent_yrs <- basic$min_sent_yrs[matched.row]
pcq2$min_sent_yrs <- basic$min_sent_yrs[matched.row]

pcq$time_to_min_cat <- ifelse(pcq$time_to_min<0, "Past min","Not yet past min")
pcq$min_sent_yrs_cat <- ifelse(pcq$min_sent_yrs<=1.5, "< 1.5 years", 
                               ifelse(pcq$min_sent_yrs>1.5 & pcq$min_sent_yrs>4, "1.5-4 years", "4+ years"))


# Plot histograms ####
colname.list <- grep("mcc_", names(pcq), value=TRUE)
for(i in colname.list){
  data <- pcq[!is.na(pcq[,i]),c(i, "unit_type")] # & pcq$unit_type %in% c("gp", "hons", "gp-tc", "rhu")
  no_respondents <- paste("Respondents =",nrow(data))
  temp.name <- gsub("mcc_", "", i)
  temp.title <- pcq_lookup[pcq_lookup$scale==temp.name,"scale_long"][1]
  names(data)[1] <- "meanscore"
  p <- ggplot(data, aes(x=meanscore)) + 
    geom_histogram(color="black", fill="lightgrey",
                   boundary = TRUE,
                   binwidth=.25)+
    geom_vline(aes(xintercept=mean(meanscore)),
               color="blue", linetype="dashed", size=1)+
    # facet_grid(unit_type ~ .) +
    xlab(paste0("Mean of answered questions ","(", no_respondents, ")")) +
    ylab("Number of respondents") +
    theme_bw() +
    ggtitle(temp.title)
  print(p)
  ggsave(paste0("output/asc/",gsub(" ","",temp.name),".jpg"),
                plot=p,
                width=10,
                height=6, 
                dpi=300)
}

# Violin Plots to compare units ####
colname.list <- grep("mcc_", names(pcq), value=TRUE)
for(i in colname.list){
  data <- pcq[!is.na(pcq[,i]) &
                pcq$unit_type %in% c("gp", "hons"),
              c(i, "unit_type")] 
  data$unit_type <- ifelse(data$unit_type=="gp", "General Population", "Honor Block")
  no_respondents <- paste("Respondents =",nrow(data))
  temp.name <- gsub("mcc_", "", i)
  temp.title <- pcq_lookup[pcq_lookup$scale==temp.name,"scale_long"][1]
  names(data)[1] <- "meanscore"
  p <- ggplot(data, aes(x=unit_type, y=meanscore, fill=unit_type)) + 
    geom_violin()+
    xlab("Unit Type") + 
    ylab("Scale Score") + 
    # facet_grid(unit_type ~ .) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(temp.title) +
    annotate("text", 
             x="General Population", 
             y = mean(data[data$unit_type=="General Population", "meanscore"]),
             label ="General Population")+
    annotate("text", 
             x="Honor Block", 
             y = mean(data[data$unit_type=="Honor Block", "meanscore"]), 
             label ="Honor Block")
  print(p)
  ggsave(paste0("output/asc/",gsub(" ","",temp.name),"unittype_violin.jpg"),
         plot=p,
         width=10,
         height=6, 
         dpi=300)
}

# Violin plot staff relationships 
i <- "mcc_staff"
data <- pcq[!is.na(pcq[,i]) &
              pcq$unit_type %in% c("gp", "hons"),
            c(i, "unit_type")] 
data$unit_type <- ifelse(data$unit_type=="gp", "General Population", "Honor Block")
no_respondents <- paste("Respondents =",nrow(data))
temp.name <- gsub("mcc_", "", i)
temp.title <- pcq_lookup[pcq_lookup$scale==temp.name,"scale_long"][1]
names(data)[1] <- "meanscore"
p <- ggplot(data, aes(x=unit_type, y=meanscore, fill=unit_type)) + 
  geom_violin()+
  xlab("Unit Type") + 
  ylab("Scale Score") + 
  # facet_grid(unit_type ~ .) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle(temp.title) +
  annotate("text", 
           x="General Population", 
           y = mean(data[data$unit_type=="General Population", "meanscore"]),
           label ="General \n Population",
           size=5)+
  annotate("text", 
           x="Honor Block", 
           y = mean(data[data$unit_type=="Honor Block", "meanscore"]), 
           label ="Honor \n Block", 
           size=5)
ggsave(paste0("output/asc/",gsub(" ","",temp.name),"unittype_violin.jpg"),
       plot=p,
       width=10,
       height=6, 
       dpi=300)

# Violin plot sentence length
i <- "mcc_staff"
data <- pcq[!is.na(pcq[,i]) & !is.na(pcq$time_to_min_cat),
            c(i, "min_sent_yrs_cat")] 
temp.name <- gsub("mcc_", "", i)
temp.title <- pcq_lookup[pcq_lookup$scale==temp.name,"scale_long"][1]
names(data)[1] <- "meanscore"
mean1 <- mean(data[data$min_sent_yrs_cat=="< 1.5 years", "meanscore"], na.rm=TRUE)
mean2 <- mean(data[data$min_sent_yrs_cat=="1.5-4 years", "meanscore"], na.rm=TRUE)
mean3 <- mean(data[data$min_sent_yrs_cat=="4+ years", "meanscore"], na.rm=TRUE)
p <- ggplot(data, aes(x=min_sent_yrs_cat, y=meanscore, fill=min_sent_yrs_cat)) + 
  geom_violin()+
  xlab("Unit Type") + 
  ylab("Scale Score") + 
  # facet_grid(unit_type ~ .) +
  theme_bw() +
  theme(legend.position = "none") +
  annotate("text", 
           x="< 1.5 years", 
           y = mean1,
           label ="< 1.5 years",
           size=5)+
  annotate("text", 
           x="1.5-4 years", 
           y = mean2, 
           label ="1.5 - 4 years", 
           size=5)+
  annotate("text", 
           x="4+ years", 
           y = mean3, 
           label ="> 4 years", 
           size=5)+
  ggtitle(temp.title) 
p

ggsave(paste0("output/asc/",gsub(" ","",temp.name),"minsent_violin.jpg"),
       plot=p,
       width=10,
       height=6, 
       dpi=300)

# Build Table ####
f.descriptives <- function(data){
  lapply(data, function(x) c(length(which(!is.na(x))),
                             mean(x, na.rm=TRUE), 
                             sd(x, na.rm=TRUE)))
}

data <- pcq2[colname.list]
tab <- t(as.data.frame(f.descriptives(data)))
rownames(tab) <- gsub("mcc_","", rownames(tab))
tab <- as.data.frame(tab)
tab$scale_long <- NA
for(i in 1:length(rownames(tab))){
  tab$scale_long[i] <- pcq_lookup[pcq_lookup$scale==rownames(tab)[i],"scale_long"][1]
}
tab <- as.data.frame(tab)
names(tab) <- c("Respondents", "Mean", "SD", "scale_long")
tab

tab <- left_join(tab,unique(pcq_lookup[,c("scale_long", "domain")]))
tab <- tab[order(tab$domain), ] # Sort by domain
tab <- tab[,-which(names(tab)=="domain")]
names(tab)[which(names(tab)=="scale_long")] <- "Scale"
tab <- tab[,c(4,1,2,3)]

print(xtable(tab, digits = c(0,0,0,2,2), caption = "Unit Means"), include.rownames = FALSE)
