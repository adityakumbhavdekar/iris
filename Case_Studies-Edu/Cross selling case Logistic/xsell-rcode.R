cust_data<-read.csv("cust_data.csv")
names(cust_data)
library(Amelia)
missmap(cust_data)
summary(cust_data)
colSums(is.na(cust_data))

### See the data summary (verify Data)
head(cust_data)
tail(cust_data)
summary(cust_data)

### Preparing Contigency table for categorical variables
### Frequency of Male-Female Responder wise (0/1)
str(cust_data)
table(cust_data$Gender,cust_data$Responder)
#  0    1 
# 2746  305 
#          0    1
# Female  747  205
# Male   1999  100
library(gmodels)
#CrossTable(cust_data2$Responder,cust_data2$pred,prop.t=FALSE,prop.chisq=FALSE,expected=FALSE, prop.r=FALSE,prop.c=FALSE)

### See the list of available columns in the dataset
colnames(cust_data)
### Column Names
# [1] "master_id"        "Responder"        "Age"              "Channel"          "FS_code"         
# [6] "Marital_status"   "Gender"           "Prosperity_Index" "WSI"              "IncomeGrp"       
# [11] "No_of_prod1"     "MSL_prod1"        "MR_prod"         "LifeStage"        "No_of_prod2"     
# [16] "MSL_prod2"       "num_of_cars"      "Family_doctor"   


###Multicollinearity check

###Get the list of numerical columns in dat1

dat1<- cust_data[,c(3,9,10,11,15,17)]
pairs(dat1)
###Prepare the correlation matrix for columns in dat1
correlation <- cor(dat1) 

### See the correlation matrix
correlation
write.csv(correlation,"cor.csv")
library(car)
###Prepare the Variance inflation factor table for columns in dat1
vif1 <- vif(lm(Responder ~ Age + WSI + IncomeGrp + No.of.prod1 + MSL_prod1 + No.of.prod2+MSL_prod2 +
                 num_of_cars + Family_doctor , data=cust_data))

vif1

contrasts(cust_data$MR_prod)
head(cust_data$MR_prod)
library(SamplingStrata)
library(sampling)

### sorting the data and generating the sample data
cust_data <- cust_data[order(cust_data$Responder,decreasing = TRUE),]
table(cust_data$Responder)
samp =strata(cust_data,c("Responder"),size=c(213,1922), method="srswor")
#get the training data
#trng<-getdata(cust_data,samp)
trng=cust_data[c(samp$ID_unit),]
test=cust_data[-c(samp$ID_unit),]
names(test)
names(trng)
trng=trng[,-c(1)]
test=test[,-c(1)]

nrow(trng)
nrow(test)
nrow(cust_data)


nrow(trng)
head(trng)
names(trng)
head(trng$ID_unit)

names(cust_data)
log=glm(formula = Responder~Age+as.factor(Channel)+
          as.factor(FS_code)+as.factor(Marital_status)+
          as.factor(Gender)+as.factor(Prosperity_Index)+
          WSI+IncomeGrp+MSL_prod1+MSL_prod2+
          num_of_cars+as.factor(Family_doctor),
        family = binomial("logit"),data=trng)

log1=glm(formula = Responder~.,family = binomial("logit"),data=trng)
summary(log)
trng1=trng
trng1$pred=ifelse(log1$fitted.values>0.5,1,0)
ccm=table(trng1$pred,trng1$Responder)
ccm
accuracy=(ccm[1,1]+ccm[2,2])/sum(ccm)
accuracy
names(test)
test1=test[,-c(1)]
names(test1)

forecast=predict(log1, test1, type="response")

forecast
head(forecast)
names(trng)
names(test1)
summary(log1)
library(MASS)
log2=step(log1)
length(log$fitted.values)
summary(log)
samp1=samp
samp1$prediction=log$fitted.values
names(samp1)
write.csv(samp1,"samp1.csv")

samp1$pred=ifelse(samp1$prediction>0.5,1,0)
table(samp1$pred,samp1$Responder)

library(gmodels)
CrossTable(trng1$pred,trng1$Responder)

 cust_data$predict=ifelse(log$fitted>0.5,1,0)
table(cust_data$predict,cust_data$Responder)
library(gmodels)
CrossTable(cust_data$predict,cust_data$Responder)
?CrossTable()
Con_ratio=Concordance(cust_data$Responder,cust_data$predict)


hosmerlem(samp1$Responder,samp1$prob)

library(ResourceSelection)
hoslem.test(samp1$Responder,samp1$prediction)

  # predict on test data
result <-  predict(log, samp, type="response") 
# check if u get similar result in converted to factor and other var eliminated
# for logisitc regression you need to type type response
help(predict.glm)

result_1=ifelse(result>0.5,1,0)
table(result_1,samp_1$Responder)

### Run the logistic regression to generate the coefficients and try to compare results

logistic1<- glm(formula = Responder ~ GRP_age+ GRP_channel+ GRP_gender+ 
                  GRP_marital_status+ GRP_FS_code+ 
                  GRP_Prosperity_Index+ No.of.prod1+ No.of.prod2+ MSL_prod1 ,
                family = binomial("logit"), data=samp)

logistic1<- glm(formula = Responder ~ GRP_age+ GRP_channel+ GRP_gender+ GRP_marital_status+
                  GRP_Prosperity_Index+ No.of.prod1+ No.of.prod2+ MSL_prod1 ,
                family = binomial("logit"), data=samp)






myfun<-function(x)
{
  x=x+2
}
x=myfun(2)

# function for concordance
Concordance(trng1$Responder,log1$fitted.values)
Concordance = function(y,yhat) 
{
  outcome_and_fitted_col<-data.frame(y, yhat)
  colnames(outcome_and_fitted_col)<-c("Responder","fitted.values")
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Equate the length of the event and non-event tables
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  ones_and_zeros = data.frame(ones, zeros)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0
    }
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0 
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
}


hosmerlem <- function (y, yhat, g = 10) 
{
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest = T)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}


names(trng1)
trng2=trng1
trng2$prob=log1$fitted.values
names(trng2)
df=trng2[,c(1,19)]
names(df)
head(df)
df
df <- df[order(df$prob,decreasing = TRUE),]
#rep(c(1:10),each=214)
df$bin_no=rep(c(1:10),each=214,length.out=nrow(df))
no_of_obs=as.data.frame(table(df$bin_no))
no_of_obs
df$bin_no
aggregate(df$Responder ~ bin_no, data = df, FUN = sum)

#or as below as done in excel 

nrow(df)
2135/10

df$seq=seq(c(1:2135))
head(df)
tail(df)
df$bin_number=round(df$seq/214)
write.csv(df,"decile.csv")
setwd("c:/edu")
bin
