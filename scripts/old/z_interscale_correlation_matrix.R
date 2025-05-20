# ---------- Interscale correlation matrix ---------- ####
# Example here: https://www.rdocumentation.org/packages/psych/versions/1.0-17/topics/cluster.cor
# Given a n x c cluster definition matrix of -1s, 0s, and 1s (the keys) , and a n x n correlation matrix, or an N x n data matrix, find the correlations of the composite clusters.
# Note to self - this also produces Alpha! 

# Using scoreItems. See old code section for approach using cluster.cor based on a correlation matrix
key.list <- list(pr=c("q10", "q11", "q12", "q13", "q14"), # prisoner relationships
                 sr=c("q15", "q16", "q17", "q18"), # staff relationships 
                 pj=c("q19", "q20", "q21", "q22"), # procedural justice 
                 sf=c("q26", "q29", "q30", "q31", "q32"), # safety
                 sv=c("q139", "q140", "q141", "q142", "q143", "q144", "q145", "q146"), #sat with visits
                 fc=c("q135", "q136", "q137"), # frequency of contact
                 sq=c("q94", "q95", "q96"), # sleep quality 
                 qc=c("q111", "q112", "q119", "q120", "q121", "q122"), # quality of care
                 sq=c("q99","q100", "q101"), # shop quality 
                 as=c("q62", "q63", "q64", "q65", "q66", "q67", "q68"), # satisfaction with activities 
                 aa=c("q69", "q70", "q71", "q72"), # availability of activities
                 re=c("q73", "q74", "q75", "q76"), # reintegration
                 au=c("q82", "q83", "q84", "q85"))# autonomy 

qnos <- unique(pcq_lookup[pcq_lookup$pc_theory=="prison climate" & 
                            pcq_lookup$in_which_survey =="NL & PA" &
                            pcq_lookup$scale_theory %ni% c("severity", "overall") &
                            pcq_lookup$include_comparative_psych_analysis=="yes",
                          c("question_no_pa_2022a","question_qno","scale_theory","scale_theory_long")]) 
scale_names <- data.frame(scale_abbreviation = names(key.list),
                          scale_theory_long = c("Prisoner relationships",
                                                "Staff-prisoner relationships",
                                                "Procedural Justice",
                                                "Safety",
                                                "Satisfaction with Visits",
                                                "Satisfaction With Frequency of Contact",
                                                "Sleep Quality",
                                                "Quality of care",
                                                "Shop Quality",
                                                "Satisfaction with Activities",
                                                "Availability of Meaningful Activities",
                                                "Reintegration",
                                                "Autonomy"))

n_scales <- length(unique(qnos$scale_theory))
my.scores <- scoreItems(key.list,
                        pcq2[,qnos$question_qno],
                        missing=TRUE,
                        impute="median") 
print(my.scores, short=FALSE)

# Generate all four possible interscale correlation matrices ####
# Retain only the corrected correlations (above the diagonal)
# Choose which matrix to use 
matrix.choice <- list(MIMS = my.scores$MIMS,
                      MIMT = my.scores$MIMT,
                      matrix.cor = my.scores$cor,
                      matrix.corrected = my.scores$corrected) # Used corrected one in current version of text 
for(k in 1:length(matrix.choice)){
file.name <- paste0("output/tables/interscale_correlations_",names(matrix.choice)[k],".txt")
keep <- t(as.vector(c(rep(0,1),rep(1,n_scales-1))))
for(i in 2:n_scales){
  keep <- rbind(keep,t(as.vector(c(rep(0,i), rep(1,n_scales-i)))))
}
tab <- t(as.data.frame(matrix.choice[k][[1]]*keep))
tab[which(tab==0)] <- ""
tab <- as.data.frame(tab)
tab <- tab[1:12]
tab <- as.data.frame(lapply(tab,as.numeric)) # convert all columns to numeric
names(tab) <- seq(1,n_scales-1,1)
row.names(tab) <- scale_names$scale_theory_long
print(xtable(tab, digits=c(0,rep(3,12)), caption=paste0("Interscale Correlation Matrix: ",names(matrix.choice)[k])),
      include.rownames=TRUE, 
      file = file.name)
}
