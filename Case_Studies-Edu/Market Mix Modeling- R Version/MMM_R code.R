##Set working directory
setwd("D:/case studies/Case_Studies-Edu/Market Mix Modeling- R Version")

##Read data
data <-read.csv("MMM_ds_1.csv", stringsAsFactors = TRUE, strip.white = TRUE, na.strings = c("NA",""))
##inspect the imported data 
str(data)
head(data)
tail(data)
##Generate Summary of Data
summary(data)
## impute missing values
is.na(data) #Checking Missing Values exist or not
##When data volume is huge
data[!complete.cases(data),] # list rows of data that have missing values
#colSums(is.na(dataframe))
data$cmpgn1[is.na(data$cmpgn1)] <- 1 # Recode Missing to 1
data$campgn2[is.na(data$campgn2)] <- 1

##Multiple Regression Model
#install.packages(c("car", "sqldf", "plyr"))

fit <- lm(ln_sales ~ cmpgn1 + campgn2 + campgn3 + ln_P_A + ln_P_B + ln_P_C, data=data) # Regression Model
summary(fit) # Prints Output
summary(fit)$r.squared #R-square Value 
library(car)
vif(fit) ## Checks Multicolinearlity
(vif(fit)) > 2 # to identify Problematic Variables

## mean centering fix

##assigning the mean values
library(plyr)
##ddply gives summary by group: group on region_cd, keyword summrise 
mean1 <- as.data.frame(ddply(data,~Region_cd,summarise,mln_sales=mean(ln_sales)))
mean2 <- as.data.frame(ddply(data,~Region_cd,summarise,mcmpgn1=mean(cmpgn1)))
mean3 <- as.data.frame(ddply(data,~Region_cd,summarise,mcampgn2=mean(campgn2)))
mean4 <- as.data.frame(ddply(data,~Region_cd,summarise,mcampgn3=mean(campgn3)))
mean5 <- as.data.frame(ddply(data,~Region_cd,summarise,mln_P_A=mean(ln_P_A)))
mean6 <- as.data.frame(ddply(data,~Region_cd,summarise,mln_P_B=mean(ln_P_B)))
mean7 <- as.data.frame(ddply(data,~Region_cd,summarise,mln_P_C=mean(ln_P_C)))

##append means to the dataset
## sqldf works only on version 3.1.2 and above
library(sqldf)
meancnt <- sqldf('select a.*,b.mln_sales,c.mcmpgn1,d.mcampgn2,e.mcampgn3,f.mln_P_A,g.mln_P_B,h.mln_P_C
                 from data a 
                 left join mean1 b on a.Region_cd = b.Region_cd
                 left join mean2 c on a.Region_cd = c.Region_cd
                 left join mean3 d on a.Region_cd = d.Region_cd
                 left join mean4 e on a.Region_cd = e.Region_cd
                 left join mean5 f on a.Region_cd = f.Region_cd
                 left join mean6 g on a.Region_cd = g.Region_cd
                 left join mean7 h on a.Region_cd = h.Region_cd')

#when  to center:**************************************************************

#1. To lessen the correlation between a multiplicative term (interaction or polynomial term) and its component variables (the ones that were multiplied).

#2. To make interpretation of parameter estimates easier.

#when NOT to center:

#1. If all continuous predictors have a meaningful value of 0.

#2. If you have no interaction terms involving that predictor.

#3. And if there are no values that are particularly meaningful.**************************************

##View and Verify the new merged dataset*/
View(meancnt)
meancnt=data
meancnt$mcmpgn1=mean(meancnt$cmpgn1)
meancnt$mcampgn2=mean(meancnt$campgn2)
meancnt$mcampgn3=mean(meancnt$campgn3)
meancnt$mln_P_A=mean(meancnt$ln_P_A)
meancnt$mln_P_B=mean(meancnt$ln_P_B)
meancnt$mln_P_C=mean(meancnt$ln_P_C)
meancnt$mln_sales= mean(meancnt1$ln_sales)
##Calculation based on the seasonality factor  1.05874 and estimates  as derived from model and taking mean centering in consideration
meancnt1 <- meancnt
meancnt1$pred = 1.05874*exp(0.1356*(meancnt1$cmpgn1-meancnt1$mcmpgn1)+ 0.2432*(meancnt1$campgn2-meancnt1$mcampgn2)+ 
                              0.3532*(meancnt1$campgn3-meancnt1$mcampgn3)+ 0.03772*(meancnt1$ln_P_A-meancnt1$mln_P_A)-
                              0.3618*(meancnt1$ln_P_B-meancnt1$mln_P_B)+ 0.3792*(meancnt1$ln_P_C-meancnt1$mln_P_C)+ meancnt1$mln_sales)

meancnt1$res = meancnt1$pred - meancnt1$sales
meancnt1$abs_res = abs(meancnt1$res)
meancnt1$mape = 100*meancnt1$abs_res/meancnt1$sales

##View and Verify the new dataset with predicted value*/
View(meancnt1)

##preparing the contribution matrix
contribution <- meancnt
attach(contribution)

## Calculate value when all factors are present
contribution$pred = 1.05874*exp(0.1356*(cmpgn1-mcmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
                     0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales)
contribution$res = contribution$pred - contribution$sales

## Calculate value when all factors except cmpgn1 are present
contribution$pred_cmpgn1 = 1.05874*exp(0.1356*(-mcmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
                            0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales)

## Calculate contribution of cmpgn1 as diff of values when all factors are present 
##and when all factors except cmpgn1 are present
contribution$contr_cmpgn1 = contribution$pred - contribution$pred_cmpgn1

contribution$pred_campgn2 = 1.05874*exp(0.1356*(cmpgn1-mcmpgn1)+ 0.2432*(-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
                             0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales)
contribution$contr_campgn2 = contribution$pred - contribution$pred_campgn2

contribution$pred_campgn3 = 1.05874*exp(0.1356*(cmpgn1-mcmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
                             0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales)
contribution$contr_campgn3 = contribution$pred - contribution$pred_campgn3;

contribution$pred_ln_P_A = 1.05874*exp(0.1356*(cmpgn1-mcmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(-mln_P_A)-
                            0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales)
contribution$contr_ln_P_A = contribution$pred - contribution$pred_ln_P_A

contribution$pred_ln_P_B = 1.05874*exp(0.1356*(cmpgn1-mcmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
                            0.3618*(-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales)
contribution$contr_ln_P_B = contribution$pred - contribution$pred_ln_P_B

contribution$pred_ln_P_C = 1.05874*exp(0.1356*(cmpgn1-mcmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
                            0.3618*(ln_P_B-mln_P_B)+ 0.3792*(-mln_P_C)+ mln_sales)
contribution$contr_ln_P_C = contribution$pred - contribution$pred_ln_P_C

#Select Required Variables
contribution <- sqldf('select ln_sales,cmpgn1,campgn2,campgn3,ln_P_A,ln_P_B,ln_P_C, 
                            mln_sales,mcmpgn1,mcampgn2,mcampgn3,mln_P_A,mln_P_B,mln_P_C, 
                            timeperiod,region_cd,pred,sales,res,
                            pred_cmpgn1,pred_campgn2,pred_campgn3,pred_ln_P_A,pred_ln_P_B,pred_ln_P_C,
                            contr_cmpgn1,contr_campgn2,contr_campgn3,contr_ln_P_A,
                            contr_ln_P_B,contr_ln_P_C 
                            from contribution')


##Select required variables to generate contribution matrix
cont_mat1 <- sqldf('select pred,contr_cmpgn1,contr_campgn2,contr_campgn3 from contribution')
write.csv(cont_mat1,file="cont_mat1.csv",row.names=F)

library(sqldf)














#to be tested

test=data

test$meanc1 <- mean(data$cmpgn1)
test$meanc2 <- mean(data$campgn2)
test$meanc3 <- mean(data$campgn3)
test$meanPA <- mean(data$ln_P_A)
test$meanPB <- mean(ln_P_B)
test$meanPC <- mean(ln_P_C)

test=meancnt
test$c1=test$cmpgn1-test$meanc1
test$c2=test$campgn2-test$meanc2
test$c3=test$campgn3-test$meanc3

test$PA=test$ln_P_A-test$meanPA
test$PB=test$ln_P_B-test$meanPB
test$PC=test$ln_P_C-test$meanPC
names(test)

test1=test[,c(8,20:24)]
names(test1)
model=lm(ln_sales ~ .,data=test1)
summary(model)
summary(fit)

