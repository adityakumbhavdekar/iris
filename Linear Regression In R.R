setwd("D:/Practice/Linear Regression/Participant/R Files")
data<-read.csv( "D:\\Practice\\Linear Regression\\Participant\\R Files\\Linear_Reg_Sample_Data.csv")
file.choose()
data
data
View(data)
str(data)
correlation<-cor(data,data)
correlation
write.csv(correlation)
data1<-data[,-c(1,8)]
data1
view(data1)
correlation1<-cor(data1,data1)
correlation1     
str(data1)
summary(data1)
data1=lm(Capped_Losses~.,data1)
summary(data1)
names(data1)
summary(data1)->data2
data2
data
names(data)
data3<-data[,-c(1,3,7)]
names(data3)
fitlinreg<-lm(Capped_Losses~.,data3)
summary(fitlinreg)
