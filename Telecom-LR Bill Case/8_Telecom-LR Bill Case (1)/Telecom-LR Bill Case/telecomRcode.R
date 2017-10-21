##Set working directory
setwd("D:/case studies/8_Telecom-LR Bill Case (1)/8_Telecom-LR Bill Case (1)/Telecom-LR Bill Case")

# Read data files for cust and their transactions in a file 
cust_billdata<-read.csv("billdata18052014.csv")

##inspect the imported data 
head(cust_billdata)

##plot Independent var  against Dependent var to observe the distribution
plot(cust_billdata$age1, cust_billdata$AvgBill)
plot(cust_billdata$salary, cust_billdata$AvgBill)
plot(cust_billdata$Num_Citylived, cust_billdata$AvgBill)
plot(cust_billdata$ClsRelativesCnt, cust_billdata$AvgBill)
plot(cust_billdata$RelativesAbroad, cust_billdata$AvgBill)
plot(cust_billdata$jobtype, cust_billdata$AvgBill)
plot(cust_billdata$Payer, cust_billdata$AvgBill)
plot(cust_billdata$Travel, cust_billdata$AvgBill)



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