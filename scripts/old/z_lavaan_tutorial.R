###Confirmatory Factor Analysis (CFA) in R with lavaan###
#IDRE Code and Seminar written by Johnny Lin 02/11/2020#

#install packages 
#install.packages("foreign", dependencies=TRUE)
#install.packages("lavaan", dependencies=TRUE)

#load libraries
library(foreign) 
library(lavaan)

#import data
dat <- read.spss("https://stats.idre.ucla.edu/wp-content/uploads/2018/05/SAQ.sav",to.data.frame=TRUE, use.value.labels = FALSE)

#covariance of Items 3 to 5
round(cov(dat[,3:5]),2)

#one factor three items, default marker method
m1a  <- ' f  =~ q03 + q04 + q05'
onefac3items_a <- cfa(m1a, data=dat) 
summary(onefac3items_a) 

#one factor three items, variance std 
m1b  <- ' f =~ NA*q03 + q04 + q05
          f ~~ 1*f ' 
onefac3items_b <- cfa(m1b, data=dat) 
summary(onefac3items_b)

#alternative model to variance standardization 
onefac3items_a <- cfa(m1a, data=dat,std.lv=TRUE)
summary(onefac3items_a)

#obtain standardized loadings
summary(onefac3items_a,standardized=TRUE)

#alternative condensed standardized solutions
standardizedsolution(onefac3items_a)

#obtain implied variance covariance matrix 
inspect(onefac3items_a,"cov.ov")

#one factor three items, with means 
m1c <- ' f =~ q03 + q04 + q05 
              q03 ~ 1 
              q04 ~ 1 
              q05 ~ 1' 
onefac3items_c <- cfa(m1c, data=dat) 
summary(onefac3items_c)

#error with default two items
m2a <- 'f1 =~ q03 + q04' 
onefac2items <- cfa(m2a, data=dat)
summary(onefac2items)

#one factor, two items (var std) 
m2b <- 'f1 =~ a*q04 + a*q05' 
onefac2items_b <- cfa(m2b, data=dat,std.lv=TRUE) 
summary(onefac2items_b)

#one factor eight items, variance std 
m3a <- 'f =~ q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08' 
onefac8items_a <- cfa(m3a, data=dat,std.lv=TRUE) 
summary(onefac8items_a, fit.measures=TRUE, standardized=TRUE)

#covariance of Items 1 through 8 
round(cor(dat[,1:8]),2)

#fit statistics 
summary(onefac8items_a, fit.measures=TRUE, standardized=TRUE)

#model chi-square 
pchisq(q=554.191,df=20,lower.tail=FALSE)

#baseline model
b1 <- ' q01 ~~ q01 
        q02 ~~ q02 
        q03 ~~ q03 
        q04 ~~ q04 
        q05 ~~ q05 
        q06 ~~ q06 
        q07 ~~ q07 
        q08 ~~ q08' 

basemodel <- cfa(b1, data=dat)
summary(basemodel)

#uncorrelated two factor solution, var std method
m4a <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ a*q06 + a*q07 
        f1 ~~ 0*f2 ' 
twofac7items_a <- cfa(m4a, data=dat,std.lv=TRUE)

#alternative syntax - uncorrelated two factor solution, var std method
twofac7items_a <- cfa(m4a, data=dat,std.lv=TRUE, auto.cov.lv.x=FALSE) 
summary(twofac7items_a, fit.measures=TRUE,standardized=TRUE)

#correlated two factor solution, marker method
m4b <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ q06 + q07' 
twofac7items_b <- cfa(m4b, data=dat,std.lv=TRUE) 
summary(twofac7items_b,fit.measures=TRUE,standardized=TRUE)

#second order three factor solution, marker method
m5a <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ q06 + q07 
        f3 =~ 1*f1 + 1*f2 
        f3 ~~ f3' 
secondorder <- cfa(m5a, data=dat) 
summary(secondorder,fit.measures=TRUE,standardized=TRUE)

#second order three factor solution, var std method
m5b <- 'f1 =~ NA*q01+ q03 + q04 + q05 + q08 
        f2 =~ NA*q06 + q07 
        f3 =~ NA*f1 + equal("f3=~f1")*f2 
        f1 ~~ 1*f1 
        f2 ~~ 1*f2 
        f3 ~~ 1*f3' 
secondorder <- cfa(m5b, data=dat) 
summary(secondorder,fit.measures=TRUE)

#obtain the parameter table of the second order factor
inspect(secondorder,"partable")