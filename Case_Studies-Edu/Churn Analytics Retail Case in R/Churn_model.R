###Set the work directory
setwd("C:/edu")
library(VIM)
library(car)
library(glmnet)
library(SamplingStrata)
library(sampling)
setwd("c:/edu")
###Import data
cust_data<-read.csv("CustomerData.csv")

library(Amelia)
missmap(cust_data)
### See the data summary (verify Data)
summary(cust_data)
write.csv(summary(cust_data),"summary1.csv")

## drop variable with more than 50% missing values..Attr15 in this case
cust_data<-cust_data[,-c(17)]

## impute missing values
cust_data$Attr2<-ifelse(is.na(cust_data$Attr2),4565,cust_data$Attr2)

# Raj - modifd Attr 4- age --max is 176
cust_data$Attr4=ifelse(cust_data$Attr4>=176,60,cust_data$Attr4)

summary(cust_data$Attr4)


summary(cust_data)

head(cust_data)
names(cust_data)

categdata=cust_data[,-c(1,2,4,6,9,10,11,12,13,16,19)]
names(categdata)
numericdata=cust_data[,c(1,2,4,6,9,10,11,12,13,16,19)]
names(numericdata)


#remove cols from original data to get remaining categorical vars
for (i in 1:ncol(categdata))
{
  ulev = sort(unique(categdata[,i])) #get the ith unique category
  for (j in 1:(length(ulev) -1)) #run through n-1 dummy iterations
  {
    numericdata[,ncol(numericdata)+1] = ifelse(categdata[,i] == ulev[j],1,0) #find matching ulev value in categ data col and append to numeric data as new col
    names(numericdata)[ncol(numericdata)] = paste(names(categdata)[i],"_D_",ulev[j],sep="") #put proper name of dummy var as originalname_D_uniquelevel - note that ncol now refers to already created dummy var in previous step
  }
}


cust_data1=numericdata
names(cust_data1)
write.csv(cust_data1,"cust_data1.csv")
cust_data1=cust_data1[,-c(1)]
vif=vif(lm(Responder ~ .,data=cust_data1))

vif

corr_data=cor(cust_data1[,-c(1)])
write.csv(corr_data,"correlation_mat.csv")


model= glm(Responder ~ ., family = binomial("logit"), data=cust_data1)
cust_data2=cust_data1
cust_data2$prob=model$fitted.values
cust_data2$pred=ifelse(cust_data2$prob>0.5,1,0)
library(gmodels)
CrossTable(cust_data2$Responder,cust_data2$pred,prop.t=FALSE,prop.chisq=FALSE,expected=FALSE, prop.r=FALSE,prop.c=FALSE)
names(cust_data1)
?CrossTable()
# Misclass rate
mean(cust_data2$pred != cust_data2$Responder)
# Accuracy - Corect classification
Accuracy=1-mean(cust_data2$pred != cust_data2$Responder)
Accuracy
library(ResourceSelection)

hoslem.test(cust_data2$Responder,cust_data2$prob,g=10)
head(cust_data2$prob)
hosmerlem(model$y,fitted(model),g=10)


R_hat<-as.vector(fitted(model))
yhat<-R_hat
y<-model$y

hosmerlem(y, yhat)


hosmerlem <- function (y, yhat, g = 10) 
{
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest = T)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}


