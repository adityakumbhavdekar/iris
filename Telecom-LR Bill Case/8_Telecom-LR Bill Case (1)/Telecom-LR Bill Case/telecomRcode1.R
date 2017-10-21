##Set working directory
setwd("D:/case studies/8_Telecom-LR Bill Case (1)/8_Telecom-LR Bill Case (1)/Telecom-LR Bill Case")

# Read data files for cust and their transactions in a file 
cust_billdata<-read.csv("billdata18052014.csv")

##inspect the imported data (will give first few records)
head(cust_billdata)
tail(cust_billdata)
names(cust_billdata)
##to get if any values are missing 
colSums(is.na(cust_billdata))
summary(cust_billdata)
##to get summary in excel file
df <- summary(cust_billdata)
write.csv(df,"outliers.csv")
##plot Independent var  against Dependent var to observe the distribution
plot(cust_billdata$age1, cust_billdata$AvgBill)
plot(cust_billdata$salary, cust_billdata$AvgBill)
plot(cust_billdata$Num_Citylived, cust_billdata$AvgBill)
plot(cust_billdata$ClsRelativesCnt, cust_billdata$AvgBill)
plot(cust_billdata$RelativesAbroad, cust_billdata$AvgBill)
plot(cust_billdata$jobtype, cust_billdata$AvgBill)
plot(cust_billdata$Payer, cust_billdata$AvgBill)
plot(cust_billdata$Travel, cust_billdata$AvgBill)


cust_billdata1 <- cust_billdata
names(cust_billdata)
cust_billdata2<-cust_billdata1[,-c(4,8,9,12,14,17,18,20,22)]
names(cust_billdata2)
MC<-cor(cust_billdata2[,-c(13)])
plot(MC)
write.csv(MC,"scattermatrix.csv")
MC
##to create linear regression model
library(car)
names(cust_billdata2)
model=lm(AvgBill~.,data = cust_billdata2)
vif(model)
alias(model)
names(cust_billdata2)
cust_billdata3<-cust_billdata2[,-c(7,8,9,12)]
model2=lm(AvgBill~.,data = cust_billdata3)
vif(model2)
alias(model2)
names(cust_billdata3)
cust_billdata3=cust_billdata2[,-c(7,8,9,12)]
model3=lm(AvgBill~.,data = cust_billdata3)
vif(model3)
alias(model3)
coefficients(model3)
summary(model3)
temp=cust_billdata3[,-c(9)]
names(temp)
#add column in R
temp$newcol="test"
names(temp)
head(temp)
p=predict(model3,data=temp)
sum(model3$residuals)
sum(model3$fitted.values)
#assumtions
hist(model3$residuals)
# check hetroskedacity
  plot(model3)
     

##Find correlation matrix for multicollinearity check
Corr_data<- cust_billdata[,-c(4,9,14,18)]
corr_matrix.csv <- cor(Corr_data,Corr_data)
write.csv(corr_matrix.csv, "corr_matrix.csv")

#split data for training and validation
set.seed(3)
train =  sample(1:nrow(cust_billdata),nrow(cust_billdata)/2)
sample(train)
test = -train
training_data = cust_billdata[train,]
testing_data = cust_billdata[test,]

summary(training_data)

##Save the training data
write.csv(training_data, "trainingdata.csv") 


##Run the code to generate model
fit <- lm(AvgBill ~ age1+ salary +as.factor(jobtype)+Num_Citylived+as.factor(Payer)+
             ClsRelativesCnt+as.factor(Travel)+as.factor(RelativesAbroad), data=training_data )


fit2 <- lm(AvgBill ~ age1+ salary +as.factor(Travel)+Num_Citylived+as.factor(RelativesAbroad)
           , data=training_data )


test_fit <-lm(AvgBill ~ age1+ salary +as.factor(jobtype)+Num_Citylived+as.factor(Payer)+
                        ClsRelativesCnt + as.factor(Travel) + as.factor(RelativesAbroad),
              data=testing_data )


##Generate the summary of the model
summary(fit1)
summary(fit2)
summary(test_fit)

## Get the fitted values
fitted(fit)

##Generate the diagnostic plots
plot(fit)
plot(test_fit)