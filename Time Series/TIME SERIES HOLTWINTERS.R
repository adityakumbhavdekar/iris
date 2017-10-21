setwd("D:/Practice/Time Series")
sales=read.csv("sales.csv")
View(sales)
sales<- ts(sales[,1],start = 1991,frequency = 12)
sales
hws1<-HoltWinters(sales,alpha=0.2,beta=FALSE,gamma = FALSE)
install.packages("SamplingStrata")
install.packages("sampling")
install.packages("VIM")
install.packages("car")
install.packages("glmnet")
library(SamplingStrata)
library(sampling)
library(VIM)
library(car)
library(glmnet)
hws1
### SINGLE EXP SMOOTHING
hws1$fitted
single.exp.model<- hws1$fitted
write.csv(single.exp.model,"single.exp.model.csv")
hws2<-HoltWinters(sales)
hws2
hws2$fitted
single.exp.model1<-hws2$fitted
write.csv(single.exp.model1,"single.exp.model1.csv")
#### DOUBLE EXP SMOOTHING
hws3<-HoltWinters(sales,alpha = 0.8,beta=0.8,gamma=FALSE)
hws3
hws3$fitted
double.exp.model=hws3$fitted
write.csv(double.exp.model,"double.exp.model.csv")
plot(hws3$fitted)
hws4=HoltWinters(sales,gamma=FALSE)
hws4
hws4$fitted
double.exp.model2<-hws4$fitted
write.csv(double.exp.model2,"double.exp.model2.csv")
hws4
plot(hws4$fitted)
#### TO PREDICT THE VALUES FOR THE NEXT 12 MONTHS
model.predict<-predict(hws4,n.ahead = 12,prediction.interval = TRUE)
model.predict
#### TRIPLE EXP SMOOTHING
hws5<-HoltWinters(sales, alpha=0.8,beta = 0.1,gamma=0.8)
hws5
hws5$fitted
triple.exp.model<-hws5$fitted
write.csv(triple.exp.model,"triple.exp.model.csv")
plot(hws5$fitted)
hws6<-HoltWinters(sales)
hws6
hws6$fitted
triple.exp.model1<-hws6$fitted
write.csv(triple.exp.model1,"triple.exp.model1.csv")
hws6$fitted
model.pred2<-predict(hws6,n.ahead = 12,prediction.interval = TRUE)
model.pred2
summary(model.pred2)
