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
CrossTable(cust_data2$Responder,cust_data2$pred,prop.t=FALSE,prop.chisq=FALSE,expected=FALSE, prop.r=FALSE,prop.c=FALSE)
names(cust_data1)
?CrossTable()
##Find correlation among numerical variables
Corr_data<- cust_data[,-c(1,3,5,7,8,14,15,17,18)]
head(Corr_data)
corr_matrix.csv <- cor(Corr_data,Corr_data)
write.csv(corr_matrix.csv, "corr_matrix.csv")

###Find Variance inflation factor for multicollinearity
vif_Cust_data.csv <- vif(lm(Responder ~ Attr2+Attr4+Attr7+Attr8+Attr9+
                              Attr10+Attr11+Attr14+Attr18
                            , data=cust_data))

write.csv(vif_Cust_data.csv, "vif_Cust_data.csv")

###Perform bivariate analysis
dat<-cust_data
head(dat)
## create table to identify the var type
bi_var<-apply(dat,2,typeof)
## add var name for the type and added the flag ..default set to 1 for all var
bi_var<-data.frame(colnames(dat),bi_var,flag=1)
## set the row names as numbers
row.names(bi_var)<-1:nrow(bi_var)
## set the column names
colnames(bi_var)<-c("variable","var_type","flag")
## get the position for variables to set the flag as 0
bi_var$flag[which( bi_var$variable %in% c("Cust_id","Responder"))]<-0
head(bi_var)
## remove those with flag as 0
bi_var<-bi_var[bi_var$flag==1,]
head(bi_var)
## created an object to get bi var analysis 
event_rate<-NULL
## loop in till all the var in the table
for ( i in 1:nrow(bi_var))
{
## get the freq table for each var..ensure that the deleted var and numbers 
## are in sequence..eg responder should be at 2nd position
  aa<-as.matrix(table(dat[,i+2],dat[,2]))
  ## append var name and the categories in that variable
  bb<-cbind(rep(as.character(bi_var$variable[i]),nrow(aa)),row.names(aa))
  ## merge the name, cat and freq table
  aa<-data.frame(cbind(bb,aa))
  ## append everything in new dataset
  event_rate<-rbind(event_rate,aa)
}
## give the column names for data created above ..after the for loop
colnames(event_rate)<-c("variable","Factor","Non-Res","Res")

head(event_rate)  

summ_cust_data.csv<-summary(cust_data)
write.csv(event_rate, "event_rate_tbl3.csv")

##Rebinning code for Attributes with significant Info values

cust_data$Attrgrp10<-ifelse(cust_data$Attr10 == 3,1,
                        ifelse(cust_data$Attr10 %in% c('5','2','4'),2,
                               ifelse(cust_data$Attr10 %in% c('1'),3,4)))

cust_data$Attrgrp11<-ifelse(cust_data$Attr11%in% c('2','5','4','3'),1,
                            ifelse(cust_data$Attr11 %in% c('6','1'),2,3))
                                   
cust_data$Attrgrp6<-ifelse(cust_data$Attr6%in% c('Unmarried'),1,
                            ifelse(cust_data$Attr6 %in% c('Married'),2,3))

cust_data$Attrgrp7<-ifelse(cust_data$Attr7%in% c('5','4'),1,
                            ifelse(cust_data$Attr7 %in% c('6','7','3'),2,3))

cust_data$Attrgrp9<-ifelse(cust_data$Attr9 %in% c('5','4','3'),1,
                            ifelse(cust_data$Attr9 %in% c('2','7','6','1'),2,3))

head(cust_data)

cust_modeldata<-cust_data[,c(1,2,20,21,22,23,24)]
write.csv(cust_modeldata, "cust_modeldata.csv")

## sorting and sampling the data
cust_modeldata <- cust_modeldata[order(cust_modeldata$Responder,decreasing = TRUE),]
training_data =strata(cust_modeldata,c("Responder"),size=c(1400,4200), method="srswor")
training_data<-getdata(cust_modeldata,training_data)
View(training_data)


logistic2= glm(Responder ~ as.factor(Attrgrp6)+as.factor(Attrgrp7)+as.factor(Attrgrp9)+
                 as.factor(Attrgrp10)+as.factor(Attrgrp11), family = binomial("logit"), data=cust_data)

logistic2= glm(Responder ~ as.factor(Attrgrp6)+as.factor(Attrgrp7)+as.factor(Attrgrp9)+
                 as.factor(Attrgrp10), family = binomial("logit"), data=cust_data)

summary(logistic2) # display results

summary_model.csv<-summary(logistic2)
write.csv(summary_model.csv, "summary_model.csv")


###############################################################################
### Hosmer lemeshow goodness of fit test
hosmerlem <- function (y, yhat, g = 10) 
  
{
  
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest = T)
  
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  
  chisq <- sum((obs - expect)^2/expect)
  
  P <- 1 - pchisq(chisq, g - 2)
  
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
  
}

R_hat<-as.vector(fitted(logistic2))
yhat<-R_hat
y<-cust_data$Responder

hosmerlem(y, yhat)

###############################################################
#### Preparing Gains/Lift chart
library(ROCR)
gain.chart <- function(y_hat,y) {
  plot(performance(prediction(y_hat,y), "tpr", "rpp"),lwd = 7, main = "Lift Chart")
  lines(ecdf((rank(-y_hat)[y == T]) / length(y)),verticals = T, do.points = F, col = "red", lwd = 3)
}

gain.chart(R_hat,cust_data$Responder)

##############################################################

### Test for concordance -C value and Gini Coefficient
# Assuming the input is a stored binomial GLM object

outcome_and_fitted_col<-data.frame(cust_data$Responder, R_hat)
colnames(outcome_and_fitted_col)<-c("Responder","fitted.values")
Concordance = function(outcome_and_fitted_col)
  
{
  
  #outcome_and_fitted_col = cbind(logistic1$Responder, logistic1$fitted.values)
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
     ties[i] = 0}
    
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

Concordance_test<-Concordance(outcome_and_fitted_col)
Concordance_test