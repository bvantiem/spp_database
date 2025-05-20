# Surplus code

```{r, eval=FALSE}
# Plot data distributions pre- and post imputation ####
data <- pcq2 |> 
  mutate(id = 1:n()) |> 
  pivot_longer(!id) |>
  group_by(name, value) |>
  summarise(n = n()) |> 
  group_by(name) |> 
  mutate(pct = round(n / sum(n),2))

data <- data[-which(is.na(data$value)),]
data$value <- factor(data$value, levels=c(5,4,3,2,1))
data <- as.data.frame(data)
data <- data[which(data$name %in% c("q136", "q139", "q140")),]
data$value <- with(data, ifelse(value=="1", "Strongly Disagree",
                                ifelse(value=="2", "Disagree", 
                                       ifelse(value== "3", "Neutral",
                                              ifelse(value=="4", "Agree",
                                                     ifelse(value=="5", "Strongly Agree",NA))))))
p <- ggplot()+
  geom_bar(data = data, aes(x = reorder(name, pct), y=pct, fill=value), position="stack", stat="identity")+
  coord_flip() + 
  ggtitle("Data distributions for items with high missingness")+
  ylab("Share of Answers")+
  xlab("Question Item")+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="bottom")
#scale_x_discrete(limits=c("StronglyAgree", "Agree", "DontKnow","Disagree","StronglyDisagree"))
p

data <- pcq2_forest |> 
  mutate(id = 1:n()) |> 
  pivot_longer(!id) |>
  group_by(name, value) |>
  summarise(n = n()) |> 
  group_by(name) |> 
  mutate(pct = round(n / sum(n),2))

data$value <- factor(data$value, levels=c(5,4,3,2,1))
data <- as.data.frame(data)
data <- data[which(data$name %in% c("q136", "q139", "q140")),]
data$value <- with(data, ifelse(value=="1", "Strongly Disagree",
                                ifelse(value=="2", "Disagree", 
                                       ifelse(value== "3", "Neutral",
                                              ifelse(value=="4", "Agree",
                                                     ifelse(value=="5", "Strongly Agree",NA))))))
p <- ggplot()+
  geom_bar(data = data, aes(x = reorder(name, pct), y=pct, fill=value), position="stack", stat="identity")+
  coord_flip() + 
  ggtitle("Data distributions for items with high missingness - with imputed data")+
  ylab("Share of Answers")+
  xlab("Question Item")+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="bottom")
#scale_x_discrete(limits=c("StronglyAgree", "Agree", "DontKnow","Disagree","StronglyDisagree"))
p
```
```{r, eval=FALSE}
# Impute data using Amelia ####
pcq2 <- as.data.frame(pcq2)
a.out <- amelia(pcq2, m = 1, p2s=1) # can specify dvars if we want to impute based on all variables in the data. Should be done if we include analyses based on unit. 
# Considering imputing more than 1 dataset 
a.out <- amelia(pcq2, m = 5, p2s=1, ords=seq(1:ncol(pcq2)))
random.name<-paste("data/processed/imputed_","stat571_ordinal",sep="")
write.amelia(obj=a.out, file.stem = random.name)
pcq_am <- read.csv(paste0(random.name, "1.csv"))

plot(a.out, c("q136", "q139", "q140", "q68")) #q68 = religious services
```

