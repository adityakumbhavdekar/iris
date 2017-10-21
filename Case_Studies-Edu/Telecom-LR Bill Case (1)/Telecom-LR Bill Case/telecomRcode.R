##Set working directory
setwd("c:/edu")
setwd("E:/ATI self study/R-course-material/R_WorkDir")

# Read data files for cust and their transactions in a file 
cust_billdata<-read.csv("billdata18052014.csv")
cust_billdata1<-read.csv("billdata18052014.csv", stringsAsFactors = FALSE)
##inspect the imported data 
head(cust_billdata)
names(cust_billdata)
colSums(is.na(cust_billdata))
df<-summary(cust_billdata)
write.csv(df,"outlier.csv")
boxplot(cust_billdata$age1)
str(cust_billdata1)
contrasts(cust_billdata$jobtype)


##plot Independent var  against Dependent var to observe the distribution
plot(cust_billdata$age1, cust_billdata$AvgBill)
plot(cust_billdata$salary, cust_billdata$AvgBill)
plot(cust_billdata$Num_Citylived, cust_billdata$AvgBill)
plot(cust_billdata$ClsRelativesCnt, cust_billdata$AvgBill)
plot(cust_billdata$RelativesAbroad, cust_billdata$AvgBill)
plot(cust_billdata$jobtype, cust_billdata$AvgBill)
plot(cust_billdata$Payer, cust_billdata$AvgBill)
plot(cust_billdata$Travel, cust_billdata$AvgBill)

cust_billdata1<-cust_billdata
names(cust_billdata1)
cust_billdata2<-cust_billdata1[,-c(4,8,9,12,14,17,18,20,22)]
names(cust_billdata2)
MC<-cor(cust_billdata2[,-c(13)])
plot(MC)
write.csv(MC,"scattermatrix.csv")
MC
library(car)
names(cust_billdata2)
model=lm(AvgBill~.,data=cust_billdata2)
vif(model)
alias(model)
summary(model)
names(cust_billdata2)
x=cust_billdata2[,-c(9,10,12)]
model2=lm(AvgBill~.,data=x)
vif(model2)
y=x[,-c(7)]
model2=lm(AvgBill~.,data=y)
alias(model2)
z=y[,-c(7)]
model2=lm(AvgBill~.,data=z)
vif(model2)
names(cust_billdata3)
cust_billdata3=cust_billdata2[,-c(7,8,9,12)]
model3=lm(AvgBill~.,data=cust_billdata3)
vif(model3)
alias(model3)
summary(model3)

names(cust_billdata3)
temp=cust_billdata3[,-c(9)]
names(temp)
p=predict(model3,data=temp)
p
head(temp)
names(temp)
sum(model3$residuals)
# Assumptions
hist(model3$residuals)

plot(model3)




##Find correlation matrix for multicollinearity check
Corr_data<- cust_billdata[,-c(4,9,14,18)]
names(Corr_data)
corr_matrix.csv <- cor(Corr_data,Corr_data)
write.csv(corr_matrix.csv, "corr_matrix.csv")


# check MC using VIF
Corr_data_1=Corr_data[,-c(18)]
names(Corr_data_1)
model=lm(AvgBill~.,data=Corr_data_1)
library(car)
vif(model)
x=alias(model)
nrow(cust_billdata)/2
 #split data for training and validation
set.seed(3)
train =  sample(1:nrow(cust_billdata),nrow(cust_billdata)/2)
nrow(train)
head(train,20)
train
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

contrasts(as.factor(training_data$jobtype))

fit2 <- lm(AvgBill ~ age1+ salary +as.factor(Travel)+Num_Citylived+as.factor(RelativesAbroad)
           , data=training_data )


test_fit <-lm(AvgBill ~ age1+ salary +as.factor(jobtype)+Num_Citylived+as.factor(Payer)+
                        ClsRelativesCnt + as.factor(Travel) + as.factor(RelativesAbroad),
              data=testing_data )
test_fit=lm(formula = AvgBill ~ age1 + salary + as.factor(jobtype) + Num_Citylived + 
     as.factor(Payer) + ClsRelativesCnt + as.factor(Travel) + 
     as.factor(RelativesAbroad), data = testing_data)
library(forecast)
accuracy(test_fit$fitted.values,testing_data$AvgBill)
##Generate the summary of the model
summary(fit)
summary(fit2)
summary(test_fit)

## Get the fitted values
fitted(fit)

##Generate the diagnostic plots
plot(fit)

# Use the below code to see the o/p of plot(fit) in matrix format
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit)
hist(fit$residuals)


plot(test_fit)




x1 <- rnorm( 100 )
x2 <- 2 * x1
y <- rnorm( 100 )
vif( lm( y ~ x1 + x2 ) )
alias(lm( y ~ x1 + x2))
