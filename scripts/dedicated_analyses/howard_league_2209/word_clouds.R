library(xtable)
library(dplyr)
library(writexl)
library(udpipe)
library(textrank)
library(wordcloud2)
library(ggplot2)
`%ni%` = Negate(`%in%`)

load("data/inputs/lsu1_deidentified.Rda")
load("data/inputs/lsu2_deidentified.Rda")

# Notes ####
# See below for approach followed 
# https://www.r-bloggers.com/2018/04/an-overview-of-keyword-extraction-techniques/
# If you delete the file:
# ud_model <- udpipe_download_model(language = "english")
# ud_model <- udpipe_load_model(ud_model$file_model)

# Model for sentences ####
ud_model <- udpipe_load_model("C:/Users/britt/OneDrive/Documents/University/PhD/4. R_project/spp/spp/english-ewt-ud-2.5-191206.udpipe")

# For LSU1 ####
index <- which(names(lsu1) %in% c("q8_comment", "q9_comment", "q10_comment", "q11_comment"))
keep <- data.frame(word=c("boo"),
                   freq=c("boo"),
                   question=c("boo"))
lsu1<-data.frame(lsu1)
for(i in index){
temp <- lsu1[,i]
x <- udpipe_annotate(ud_model, x = temp)
x <- as.data.frame(x)
temp <- subset(x, upos %in% "NOUN")
temp <- txt_freq(x = temp$lemma)
names(temp)[which(names(temp)=="key")] <- "word"
temp <- temp[,c("word", "freq")]
temp$question <- names(lsu1)[i]
keep <- rbind(keep, temp)
}

temp <- keep[keep$question=="q10_comment" & keep$word %ni% c("none", "um"),]
temp$freq <- as.numeric(temp$freq)
p <- wordcloud2(temp, shuffle=FALSE, size=.3)
p

# For LSU2 ####

# Words that best describe 
lsu2<-data.frame(lsu2)
temp <- with(lsu2, c(q1.1_word, q1.2_word,q1.3_word))
temp[temp=="dissapointing"] <- "dissapointed"
temp[temp %in% c("confusing", "confusion")] <- "confused"
temp[temp=="maraurllso"] <- "maravilloso"
temp <- data.frame(table(temp))
temp <- temp[temp$temp!="999",]
names(temp) <- c("word", "freq")
p <- wordcloud2(temp, size=.5)

# Most Positive 
index <- which(names(lsu2) %in% c("q2.1_comment","q2.2_comment"))
keep <- data.frame(word=c("boo"),
                   freq=c("boo"),
                   question=c("boo"))
lsu2<-data.frame(lsu2)
for(i in index){
  temp <- lsu2[,i]
  x <- udpipe_annotate(ud_model, x = temp)
  x <- as.data.frame(x)
  temp <- subset(x, upos %in% "NOUN")
  temp <- txt_freq(x = temp$lemma)
  names(temp)[which(names(temp)=="key")] <- "word"
  temp <- temp[,c("word", "freq")]
  temp$question <- names(lsu2)[i]
  keep <- rbind(keep, temp)
}

temp <- keep[keep$question=="q2.1_comment" & keep$word %ni% c("none", "um"),]
temp$freq <- as.numeric(temp$freq)
p <- wordcloud2(temp, shuffle=FALSE, size=.8)
p
