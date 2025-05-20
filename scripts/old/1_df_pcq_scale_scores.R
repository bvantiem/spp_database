# Deleted from file 1_df_pcq on 230718 at the start of analysis for longitudinal project
# Keep code once I decide how to set up the main file

# Impute data using Amelia ####
pcq2[which(is.na(pcq2$date_l)),"date_l"] <- ymd(20990909)
list.vars <- c("research_id", "unit", "block", "survey_wave", "date", "date_l",
               "text_answer", "id_num_2", "notes", "children", "cell", "foreign_born",
               "partner", "unit_type", "q174")
pcq2 <- as.data.frame(pcq2)
a.out <- amelia(pcq2, m = 1, idvars = list.vars, p2s=1)

b<-round(runif(1,min=1111,max=9999))
random.name<-paste("data/processed/imputed_",b,sep="")
write.amelia(obj=a.out, file.stem = random.name)

pcq_am <- read.csv(paste0(random.name, "1.csv")) # 1 for first data file


# Add scale scores for each individual, to facilitate calculations later on ------- ####
# For pcq2 and pcq
retain <- pcq_lookup[which(pcq_lookup$include_comparative_psych_analysis=="yes"),]$question_qno
scales.list <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain,c("scale_theory", "pc_theory")])

# scale mean, calculated over all answered questions
names.cols <- paste0("maq_", scales.list$scale_theory)
pcq2[,names.cols] <- NA

names.cols <- paste0("nqs_", scales.list$scale_theory)
pcq2[,names.cols] <- NA

names.cols <- paste0("sd_", scales.list$scale_theory)
pcq2[,names.cols] <- NA

# scale means with missing values imputed by median
names.cols <- paste0("mip_", scales.list$scale_theory)
pcq2[,names.cols] <- NA

# Impute unanswered questions by taking the column median
col.names <- names(pcq2)[grep("^q", names(pcq2))]
pcq2_imputed <- as.data.frame(pcq2)
for(i in col.names){
  pcq2_imputed[which(is.na(pcq2_imputed[,i])),i] <- median(pcq2_imputed[,i], na.rm=TRUE)
}

for(i in scales.list$scale){
  # Means and SDs
  ## Identify scales and accompanying question numbers
  qnos <- data.frame(no = unique(pcq_lookup[which(pcq_lookup$scale_theory==i & pcq_lookup$pc_theory=="prison climate" &
                                                    pcq_lookup$include_comparative_psych_analysis=="yes"),"question_no_pa_2022a"]),
                     qno = NA)
  qnos$qno <- paste0("q",qnos$no)

  rows.without.nas <- which(complete.cases(pcq[,qnos$qno])==TRUE) # Complete cases include those that have no opinion or not applicable answers, but exclude questions left blank

  # Calculate mean of identified questions that were answered for each individual

  col_maq <- grep(paste0("maq_",i), names(pcq2)[grep(i, names(pcq2))], value=TRUE)
  col_nqs <- grep(paste0("nqs_",i), names(pcq2)[grep(i, names(pcq2))], value=TRUE)

  if(i == "overall"){
    pcq2[rows.without.nas,col_maq] <- NA
  }
  else{
    pcq2[rows.without.nas,col_maq] <- rowSums(pcq2[rows.without.nas,qnos$qno], na.rm=TRUE) #summing answered questions
  }

  for(j in 1:nrow(pcq2)){
    answered_questions <- length(qnos$qno)-length(which(pcq[j,qnos$qno]==111)) # note counting cases as complete if answered 111, but cannot count it for mean calculation
    pcq2[j,col_nqs] <- answered_questions
    if(answered_questions %in% c(0, NA)){
      pcq2[j,col_maq] <- NA
    }
    else{
      pcq2[j,col_maq] <- pcq2[j,col_maq]/pcq2[j,col_nqs]
    }
  }

  # Calculate sds of identified questions that were answered for each individual
  col_sd <- grep("sd_", names(pcq2)[grep(i, names(pcq2))], value=TRUE)
  pcq2[rows.without.nas,col_sd] <- rowSds(as.matrix(pcq2[rows.without.nas,qnos$qno]), na.rm=TRUE)

  # Calculate scale means based on pcq_imputed, where missing values are imputed by the median
  questions_in_scale <- length(qnos$qno)
  col_mip <- grep(paste0("mip_",i), names(pcq2_imputed)[grep(i, names(pcq2_imputed))], value=TRUE)
  if(i != "overall"){
    pcq2[,col_mip] <- rowSums(pcq2_imputed[,qnos$qno], na.rm=TRUE)/questions_in_scale
  }
  else{
    pcq2[,col_mip] <- pcq2_imputed[,qnos$qno]
  }
}

# Create two additional columns with the visits scale split
# scale means with missing values imputed by median
# pcq2$mip_visits_infrastructure <- NA
# pcq2$mip_visits_feelings <- NA
#
# for(i in c("visits_infrastructure", "visits_feelings")){
#   ## Identify scales and accompanying question numbers
#   qnos <- data.frame(no = unique(pcq_lookup[which(pcq_lookup$scale_exploratory==i & pcq_lookup$pc_theory=="prison climate" &
#                                                     pcq_lookup$include_comparative_psych_analysis=="yes"),"question_no_pa_2022a"]),
#                      qno = NA)
#   qnos$qno <- paste0("q",qnos$no)
#
#   # Calculate scale means based on pcq_imputed, where missing values are imputed by the median
#   questions_in_scale <- length(qnos$qno)
#   col_mip <- grep(paste0("mip_",i), names(pcq2)[grep(i, names(pcq2))], value=TRUE)
#   pcq2[,col_mip] <- rowSums(pcq2_imputed[,qnos$qno], na.rm=TRUE)/questions_in_scale
# }
#
# rm(pcq_imputed)

# Add same columns to pcq - HARD CODED!
start.col <- which(names(pcq2)=="maq_prisoners")
end.col <- which(names(pcq2)=="mip_severity")
pcq <- cbind(pcq,pcq2[,start.col:end.col])


# Calculate scale scores for Dataset imputed by Amelia
# For pcq_am and pcq
retain <- pcq_lookup[which(pcq_lookup$include_comparative_psych_analysis=="yes"),]$question_qno
scales.list <- unique(pcq_lookup[pcq_lookup$question_qno %in% retain,c("scale_theory", "pc_theory")])

# scale mean, calculated over all questions (missing values imputed Amelia)
names.cols <- paste0("mam_", scales.list$scale_theory)
pcq_am[,names.cols] <- NA

for(i in scales.list$scale){
  # Means and SDs
  ## Identify scales and accompanying question numbers
  qnos <- data.frame(no = unique(pcq_lookup[which(pcq_lookup$scale_theory==i & pcq_lookup$pc_theory=="prison climate" &
                                                    pcq_lookup$include_comparative_psych_analysis=="yes"),"question_no_pa_2022a"]),
                     qno = NA)
  qnos$qno <- paste0("q",qnos$no)

  # Calculate scale means based on pcq_am, where missing values are imputed by the median
  questions_in_scale <- length(qnos$qno)
  col_mam <- grep(paste0("mam_",i), names(pcq_am)[grep(i, names(pcq_am))], value=TRUE)
  if(i != "overall"){
    pcq_am[,col_mam] <- rowSums(pcq_am[,qnos$qno], na.rm=TRUE)/questions_in_scale
  }
  else{
    pcq_am[,col_mam] <- pcq_am[,qnos$qno]
  }
}

# Create two additional columns with the visits scale split
# scale means with missing values imputed by median
pcq_am$mam_visits_infrastructure <- NA
pcq_am$mam_visits_feelings <- NA

for(i in c("visits_infrastructure", "visits_feelings")){
  ## Identify scales and accompanying question numbers
  qnos <- data.frame(no = unique(pcq_lookup[which(pcq_lookup$scale_exploratory==i & pcq_lookup$pc_theory=="prison climate" &
                                                    pcq_lookup$include_comparative_psych_analysis=="yes"),"question_no_pa_2022a"]),
                     qno = NA)
  qnos$qno <- paste0("q",qnos$no)

  # Calculate scale means based on pcq_am, where missing values are imputed by the median
  questions_in_scale <- length(qnos$qno)
  col_mam <- grep(paste0("mam_",i), names(pcq_am)[grep(i, names(pcq_am))], value=TRUE)
  pcq_am[,col_mip] <- rowSums(pcq_am[,qnos$qno], na.rm=TRUE)/questions_in_scale
}
