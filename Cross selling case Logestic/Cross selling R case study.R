###set working directory
setwd("D:/case studies/5_Cross selling case Logestic/5_Cross selling case Logestic")

### import required packages
library(car)
library(glmnet)
library(SamplingStrata)
library(sampling)
library(gmodles)
###Read the data 
cust_data<-read.csv("cust_data.csv")

### See the data summary (verify Data)
head(cust_data)
tail(cust_data)
summary(cust_data)
missmap(cust_data)
str(cust_data)
table(cust_data$Gender,cust_data$Responder)
#  0    1 
# 2746  305 
#          0    1
# Female  747  205
# Male   1999  100
### Column Names
# [1] "master_id"        "Responder"        "Age"              "Channel"          "FS_code"         
# [6] "Marital_status"   "Gender"           "Prosperity_Index" "WSI"              "IncomeGrp"       
# [11] "No_of_prod1"     "MSL_prod1"        "MR_prod"         "LifeStage"        "No_of_prod2"     
# [16] "MSL_prod2"       "num_of_cars"      "Family_doctor"   

### Frequency of Male-Female Responder wise (0/1)

###Multicollinearity check
dat1<- cust_data[,c(3,9,10,11,15,17)]
correlation <- cor(dat1,dat1)


vif1 <- vif(lm(Responder ~ Age + WSI + IncomeGrp + No.of.prod1 + MSL_prod1 + No.of.prod2+MSL_prod2 +
                 num_of_cars + Family_doctor , data=cust_data))

vif1
contrasts(cust_data$Gender)
contrasts(cust_data$MR_prod)
          
          
### vif
# Age           WSI     IncomeGrp   No_of_prod1     MSL_prod1   No_of_prod2     MSL_prod2   num_of_cars 
# 1.003109      1.001896      1.003010      1.001883      1.002721      1.004788      1.001417      1.001903 
# Family_doctor 
# 1.001487 
# WSI  IncomeGrp  No_of_prod1  MSL_prod1  No_of_prod2  MSL_prod2  num_of_cars  Family_doctor



##Missing value and capping treatment along with categorization  					  
cust_data$GRP_age <- ifelse( cust_data$Age=="",1 ,ifelse(cust_data$Age>50,3,ifelse(cust_data$Age>25,2,1)))
cust_data$GRP_channel<-ifelse(cust_data$Channel=="Direct",1,ifelse(cust_data$Channel=="Broker",2,1))
cust_data$GRP_gender<-ifelse(cust_data$Gender=="Male",1,ifelse(cust_data$Gender=="Female",2,1))
cust_data$GRP_marital_status<-ifelse(cust_data$Marital_status=="Yes",1,ifelse(cust_data$Marital_status=="No",2,1))

cust_data$GRP_FS_code<-ifelse(cust_data$FS_code=="A",1,ifelse(cust_data$FS_code=="E",3,2))

cust_data$GRP_Prosperity_Index<-ifelse(cust_data$Prosperity_Index=="High",1,ifelse(cust_data$Prosperity_Index=="Medium",2,2))

cust_data$GRP_No.of.prod1<-ifelse(cust_data$No.of.prod1>5,1,ifelse(cust_data$No.of.prod1<=5,2,2))

cust_data$GRP_No.of.prod2<-ifelse(cust_data$No.of.prod2>5,1,ifelse(cust_data$No.of.prod2 >2,2,1))

cust_data$GRP_MSL_prod1<-ifelse(cust_data$MSL_prod1>24,1,ifelse(cust_data$MSL_prod1 >12 ,2,1))

## sorting the data and generating the sample data
cust_data <- cust_data[order(cust_data$Responder,decreasing = TRUE),]
table(cust_data$Responder)
samp =strata(cust_data,c("Responder"),size=c(213,1922), method="srswor")
#get the training data
#trng<-getdata(cust_data,samp)
trng=cust_data[c(samp$ID_unit),]
test=cust_data[-c(samp$ID_unit),]
nrow(samp)
head(trng)
names(cust_data)
#Logistic equation
log=glm(formula = Responder~Age+as.factor(Channel)+as.factor(FS_code)+as.factor(Marital_status)+as.factor(Gender)+as.factor(Prosperity_Index)+WSI+IncomeGrp+No.of.prod1+MSL_prod1+MR_prod+LifeStage+No.of.prod2+MSL_prod2+num_of_cars+as.factor(Family_doctor),family=binomial("logit"),data=trng)
summary(log)
log1=glm(formula = Responder~.,family=binomial("logit"),data=trng)
summary(log1)
trng1=trng
trng1$pred=ifelse(log1$fitted.values>0.5,1,0)
table(trng1$pred,trng1$Responder)
#classification confusing matrix
ccm=table(trng1$pred,trng1$Responder)
accuracy=(ccm[1,1]+ccm[2,2])/sum(ccm)
accuracy
#### Creating the decile chart
names(trng1)
trng2=trng1
trng2$prob=log1$fitted.values
names(trng2)
df=trng2[,c(2,29)]
names(df)
head(df)
tail(df)
df
df$bin=rep(c(1:214,lenght.out=2135))
bin
df$seq=seq(c(1:2135))
head(df)
df$bin_number=round(df$seq/214)-> decile
decile

