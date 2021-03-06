setwd("D:/case studies/email")
train<- read.csv('Email_Report.csv',stringsAsFactors = F,header = T)
head(train)
# Number of campaigns for which opened > delivered
nrow(subset(train, Opened>Delivered))
# Number of campaigns for which clicked > opened
nrow(subset(train, Clicked>Opened))
## [1] 16
# Number of campaigns for which unsubscribed > opened, clicked
nrow(subset(train, Unsubscribed>Opened))
## [1] 146
nrow(subset(train, Unsubscribed>Clicked))
# Remove the data issues
train <- subset(train,Opened<Delivered)
train <- subset(train,Clicked<Opened)
train <- subset(train,Unsubscribed<Opened)
train <- subset(train,Unsubscribed<Clicked)
nrow(train)
str(train)
# Convert Business and Email_Type into factors
train$Business <- factor(train$Business)
train$Email_Type <- factor(train$Email_Type)

# Adding email engagement metrics
train$Open_Rate <- train$Opened/train$Delivered
train$Click_Rate <- train$Clicked/train$Delivered
train$Unsub_Rate <- train$Unsubscribed/train$Delivered

# Checking Data Types Again
str(train)
# % of email campaigns that fall in inform and trade category for each business
prop.table(table(train$Business, train$Email_Type),1)*100
library("gmodels", lib.loc="~/R/win-library/3.4")
# % of email campaigns that fall in inform and trade category for each business
prop.table(table(train$Business, train$Email_Type),1)*100
# Check the distribution of open rate at a business and email type level
aggregate(Open_Rate ~ Business + Email_Type, data = train, FUN = summary)
# Default Value
train$Open_Score <- 0

# Clothing and Inform
train$Open_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Inform" & train$Open_Rate>0.35990 &
                             train$Open_Rate<0.65690,1, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Inform" & train$Open_Rate>0.65690 &
                             train$Open_Rate<0.73350,2, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Inform" & train$Open_Rate>0.73350 &
                             train$Open_Rate<0.78680,3, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Inform" &
                             train$Open_Rate>0.78680,4,train$Open_Score)


# Grocery and Inform
train$Open_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Inform" & train$Open_Rate>0.18130 &
                             train$Open_Rate<0.46180,1, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Inform" & train$Open_Rate>0.46180 &
                             train$Open_Rate<0.50610,2, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Inform" & train$Open_Rate>0.50610 &
                             train$Open_Rate<0.56810,3, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Inform" &
                             train$Open_Rate>0.56810,4,train$Open_Score)

# Homeware and Inform
train$Open_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Inform" & train$Open_Rate>0.03615 &
                             train$Open_Rate<0.25110,1, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Inform" & train$Open_Rate>0.25110 &
                             train$Open_Rate<0.30840,2, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Inform" & train$Open_Rate>0.30840 &
                             train$Open_Rate<0.45920,3, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Inform" &
                             train$Open_Rate>0.45920,4,train$Open_Score)


# Clothing and Trade
train$Open_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Trade" & train$Open_Rate>0.02925 &
                             train$Open_Rate<0.32400,1, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Trade" & train$Open_Rate>0.32400 &
                             train$Open_Rate<0.39530,2, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Trade" & train$Open_Rate>0.39530 &
                             train$Open_Rate<0.52240,3, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Trade" &
                             train$Open_Rate>0.52240,4,train$Open_Score)


# Grocery and Trade
train$Open_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Trade" & train$Open_Rate>0.08321 &
                             train$Open_Rate<0.29030,1, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Trade" & train$Open_Rate>0.29030 &
                             train$Open_Rate<0.34690,2, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Trade" & train$Open_Rate>0.34690 &
                             train$Open_Rate<0.40290,3, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Trade" &
                             train$Open_Rate>0.40290,4,train$Open_Score)


# Homeware and Trade
train$Open_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Trade" & train$Open_Rate>0.05140 &
                             train$Open_Rate<0.36430,1, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Trade" & train$Open_Rate>0.36430 &
                             train$Open_Rate<0.43520,2, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Trade" & train$Open_Rate>0.43520 &
                             train$Open_Rate<0.48150,3, train$Open_Score)
train$Open_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Trade" &
                             train$Open_Rate>0.48150,4,train$Open_Score)

# Check the distribution of unsubscribe rate
summary(train$Unsub_Rate)
# Check the distribution of unsubscribe rate
summary(train$Unsub_Rate)
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000000 0.0000000 0.0000724 0.0028160 0.0009823 0.5044000
# Default Value
train$Unsub_Score <- -1

# Unsubcribe Score
train$Unsub_Score <- ifelse(train$Unsub_Rate>0.0010 & train$Unsub_Rate<=0.0030,-2, train$Unsub_Score)
train$Unsub_Score <- ifelse(train$Unsub_Rate>0.0030 & train$Unsub_Rate<=0.0050,-3, train$Unsub_Score)
train$Unsub_Score <- ifelse(train$Unsub_Rate>0.0050,-4, train$Unsub_Score)

# Setting Default Value
train$Email_Score <- 0

# Email Score
# ES for Inform = 5 x Open Score + Unsubscribe Score                
# ES for Trade =    2 x Open Score + 3 x Click Score + Unsubscribe Score                

train$Email_Score <- ifelse(train$Email_Type=="Inform",(5*train$Open_Score + train$Unsub_Score),
                            (2*train$Open_Score + 3*train$Click_Score + train$Unsub_Score)) 

# Normalization is required as we need to normalize the difference between the emails with different length and depth of content in the same category

train$Norm_Email_Score <- (train$Email_Score)/(max(train$Email_Score)-min(train$Email_Score))

summary(train$Norm_Email_Score)
aggregate(Norm_Email_Score ~ Business + Email_Type, data = train, FUN = summary)
