setwd("C:/Users/DR-STRANG/Desktop/Aditya/R/SVM")
data(iris)
str(iris)
library(ggplot2)
qplot(Petal.Length,Petal.Width,data=iris,color = Species)
## Support vector Machine
library(e1071)
mymodel<- svm(Species~.,data=iris)
summary(mymodel)
plot(mymodel, data=iris, Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))


## Confusion Matrix and Misclassificatin rate
pred<- predict(mymodel, data= iris)
tab<-table(Predicted= pred, Actual = iris$Species)
1-sum(diag(tab))/sum(tab)
