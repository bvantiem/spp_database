###Confirmatory Factor Analysis (CFA) in R with lavaan###
#IDRE Code and Seminar written by Johnny Lin 02/11/2020#
# https://stats.oarc.ucla.edu/r/seminars/rcfa/


#load libraries
library(foreign) 
library(lavaan)

# Retain only relevant variables 
retain <- pcq_lookup[which(pcq_lookup$include_comparative_psych_analysis=="yes"),]$question_qno
dat <- pcq_am[,c(retain, "unit_type")]

temp <- pcq_lookup[which(pcq_lookup$include_comparative_psych_analysis=="yes"),c("question_no_pa_2022a", "question_pa_2022a", "factor_no_efa")]

# CFA 
# CONSIDER using ordered factors. Doesn't work currently because imputed data is continuous 
# Explore group and cluster options in lavaan
#correlated two factor solution, marker method
# Leave out f5 (visits feelings), f6 (3 item, contact frequency), f7 (sleep), f9 (shop)
# f1 (prisoners), f2 (staff), f3 (safety), f4 (visits), f8 (care), f10 (actsat), f11 (actav), f12 (reint), f13 (autonomy)
m1 <- 'f1 =~ q10 + q11 + q12 + q13 + q14 
       f2 =~ q15 + q16 + q17 + q18 + q19 + q20 + q21 + q22
       f3 =~ q26 + q29 + q32 + q30 + q31 
       f4 =~ q139 + q140 + q141 + q142 + q143 + q144
       f8 =~ q120 + q119 + q121 + q122
       f9 =~ q99 + q100 + q101
       f10 =~ q62 + q63 + q64 + q65 + q66 + q67 + q68
       f11 =~ q69 + q70 + q71 + q72
       f12 =~ q73 + q74 + q75 + q76
       f13 =~ q82 + q83 + q84 + q85'
    
m1_cfa <- cfa(m1, data=dat,std.lv=TRUE) 
summary(m1_cfa,fit.measures=TRUE,standardized=TRUE) # Although lavaan defaults to the marker method, by specifying standardized=TRUE we implement the variance standardization method.

# Merge the two activity scales as per bosma 
# Fits the data less well (fit indexes worse)
m2 <- 'f1 =~ q10 + q11 + q12 + q13 + q14 
       f2 =~ q15 + q16 + q17 + q18 + q19 + q20 + q21 + q22
       f3 =~ q26 + q29 + q32 + q30 + q31 
       f4 =~ q139 + q140 + q141 + q142 + q143 + q144
       f8 =~ q120 + q119 + q121 + q122
       f9 =~ q99 + q100 + q101
       f10 =~ q62 + q63 + q64 + q65 + q66 + q67 + q68
       f11 =~ q69 + q70 + q71 + q72 + q73 + q74 + q75 + q76
       f13 =~ q82 + q83 + q84 + q85'

m2_cfa <- cfa(m2, data=dat,std.lv=TRUE) 
summary(m2_cfa,fit.measures=TRUE,standardized=TRUE) 

# second order factors - unit level factors 
m3 <- 'f1 =~ q10 + q11 + q12 + q13 + q14 
       f2 =~ q15 + q16 + q17 + q18 + q19 + q20 + q21 + q22
       f3 =~ q26 + q29 + q32 + q30 + q31 
       f4 =~ q139 + q140 + q141 + q142 + q143 + q144
       f8 =~ q120 + q119 + q121 + q122
       f9 =~ q99 + q100 + q101
       f10 =~ q62 + q63 + q64 + q65 + q66 + q67 + q68
       f11 =~ q69 + q70 + q71 + q72
       f12 =~ q73 + q74 + q75 + q76
       f13 =~ q82 + q83 + q84 + q85
       f14 =~ f1 + f2 + f12 + f13
       f14 ~~ f14'

m3_cfa <- cfa(m3, data=dat,std.lv=TRUE) 
summary(m3_cfa,fit.measures=TRUE,standardized=TRUE) 

f3 =~ 1*f1 + 1*f2
f3 ~~ f3' 