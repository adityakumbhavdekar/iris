setwd("D:/Practice/Time Series")
library(forecast)
#Reading data in R
data<-read.csv(file.choose())
View(data)
#Converting data into time series using ts function... taking onlhy sales column
timedata<-ts(data,frequency=12,start=c(1995,1),end=c(1997,12))
head(timedata)
plot(timedata,ylab="Sales of motor", xlab="Years")
#Decomposing time series into components
components<-stl(timedata,s.window="periodic")
plot(components)
#Fitting simple exponential smoothing
fit1<-ses(timedata,alpha=0.6,initial="simple",h=12)
fit1
fit1<-ses(timedata,initial="simple",h=12)
fit1
plot(fit1)
summary(fit1)
SES<-summary(fit1)
write.csv(SES,"SES.csv")
#Fitting Holt's linear trend method
fit2<-holt(timedata,alpha=0.6,beta=0.1,initial="simple",h=12)
plot(fit2)
summary(fit2)
HOLT<-summary(fit2)
write.csv(HOLT,"HOLT.csv")
#Fitting Holtwinters seasonal method
fit3<-hw(timedata,seasonal="multiplicative",h=12)
plot(fit3)
summary(fit3)
HW<-summary(fit3)
write.csv(HW,"HW.csv")
