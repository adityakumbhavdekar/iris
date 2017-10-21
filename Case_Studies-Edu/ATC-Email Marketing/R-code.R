train=read.csv("Email_Report.csv")
names(train)
# no of campaigns for which open>deli
nrow(subset(train,Opened>Delivered))
nrow(subset(train,Clicked>Opened))
nrow(subset(train,Unsubscribed>Opened))
nrow(subset(train,Unsubscribed>Clicked))
#..........................................

train<-subset(train,Opened<Delivered)
train<-subset(train,Clicked<Opened)
train<-subset(train,Unsubscribed<Opened)
train<-subset(train,Unsubscribed<Clicked)
nrow(train)


str(train)
train$Business<-factor(train$Business)
train$Email_Type <- factor(train$Email_Type)



train$Open_Rate <- train$Opened/train$Delivered
train$Click_Rate <- train$Clicked/train$Delivered
train$Unsub_Rate <- train$Unsubscribed/train$Delivered


prop.table(table(train$Business, train$Email_Type),1)*100


aggregate(Open_Rate ~ Business + Email_Type, data = train, FUN = summary)
train$Open_Score <- 0
head(train)

# assign score for inform 
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


head(train)
aggregate(Click_Rate ~ Business + Email_Type, data = train, FUN = summary)



# Default Value
train$Click_Score <- 0

# Clothing and Trade
train$Click_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Trade" & train$Click_Rate>0.004852 &
                              train$Click_Rate<0.039300,1, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Trade" & train$Click_Rate>0.039300 &
                              train$Click_Rate<0.063070,2, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Trade" & train$Click_Rate>0.063070 &
                              train$Click_Rate<0.119000,3, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Clothing" & train$Email_Type=="Trade" &
                              train$Click_Rate>0.119000,4,train$Click_Score)


# Grocery and Trade
train$Click_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Trade" & train$Click_Rate>0.001821 &
                              train$Click_Rate<0.020250,1, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Trade" & train$Click_Rate>0.020250 &
                              train$Click_Rate<0.034850,2, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Trade" & train$Click_Rate>0.034850 &
                              train$Click_Rate<0.061230,3, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Grocery" & train$Email_Type=="Trade" &
                              train$Click_Rate>0.061230,4,train$Click_Score)


# Homeware and Trade
train$Click_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Trade" & train$Click_Rate>0.001707 &
                              train$Click_Rate<0.076990,1, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Trade" & train$Click_Rate>0.076990 &
                              train$Click_Rate<0.107900,2, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Trade" & train$Click_Rate>0.107900 &
                              train$Click_Rate<0.144100,3, train$Click_Score)
train$Click_Score <- ifelse(train$Business=="Homeware" & train$Email_Type=="Trade" &
                              train$Click_Rate>0.144100,4,train$Click_Score)


summary(train$Unsub_Rate)




# Default Value
train$Unsub_Score <- -1

# Unsubcribe Score
train$Unsub_Score <- ifelse(train$Unsub_Rate>0.0010 & train$Unsub_Rate<=0.0030,-2, train$Unsub_Score)
train$Unsub_Score <- ifelse(train$Unsub_Rate>0.0030 & train$Unsub_Rate<=0.0050,-3, train$Unsub_Score)
train$Unsub_Score <- ifelse(train$Unsub_Rate>0.0050,-4, train$Unsub_Score)


# Setting Default Value
train$Email_Score <- 0
write.csv(train,"train.csv")

#Email Score
# ES for Inform = 5 x Open Score + Unsubscribe Score                
# ES for Trade =    2 x Open Score + 3 x Click Score + Unsubscribe Score                

train$Email_Score <- ifelse(train$Email_Type=="Inform",(5*train$Open_Score + train$Unsub_Score),
                            (2*train$Open_Score + 3*train$Click_Score + train$Unsub_Score)) 

write.csv(train,"train1.csv")
head(train$Email_Score)

train$Norm_Email_Score <- (train$Email_Score)/(max(train$Email_Score)-min(train$Email_Score))

write.csv(train,"train2.csv")
head(train$Email_Score)

summary(train$Norm_Email_Score)

aggregate(Norm_Email_Score ~ Business + Email_Type, data = train, FUN = summary)

# From above if we see that a campaign has avg score less than the avg- say a gorcerry campaing has score of 0.4.
#We will advise to stop the camaign

#Campaign Priortization

#Let's say on a given day, we have 4 campaigns - A, B, C and D with Email Scores as 0.2, 0.4, 0.7 and 0.9 respectively. 
#TO make sure that a customer receive only a single email in a day,
#we can rank these campaigns as per their ES like the followuing: 
#1.D
#2.C
#3.B
#4.A

#Now, say there is a customer "Pankaj" who is a part of the targeting for campaigns D, C and B. By this scoring, 
#we will only send him campaign D and suppress him from receving B and C. This way, the customer receives our best campaign.

# Customer segmentation - Kill the zombies
customer <- read.csv('Customer_Report.csv')
str(customer)
head(customer)
aggregate(Open_Rate ~ Business, data = train, FUN = median)

#We can see that for Clothing, Grocery and Homeware, the median open rate is 40.64%, 37.12% and 42.65% respectively. 
#Now, let's count the number of customers whose engagement is less than the threshold.


(nrow(subset(customer, Open.Rate > 0.4064 & Business == 'Clothing'))/nrow(subset(customer, Business == 'Clothing')))

(nrow(subset(customer, Open.Rate > 0.3712 & Business == 'Grocery'))/nrow(subset(customer, Business == 'Grocery')))

(nrow(subset(customer, Open.Rate > 0.4265 & Business == 'Homeware'))/nrow(subset(customer, Business == 'Homeware')))


#We can clearly see that only 18.6% of the customers who receive Clothing emails are actually engaged with the content
#54% of the customers who received Grocery emails are actually interested in them
#31.5% of the customers are interested in receiving Homeware emails
#Hence, we can stop sendng the emails to Zombies (basically the non-engaged customers)

