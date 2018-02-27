#wnload the Wine DatasetR
setwd("C:/Users/DR-STRANG/Desktop/Aditya/R/KNN")

dataurl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
download.file(url = dataurl, destfile = "wine.data")
wine_df <- read.csv("wine.data", header = FALSE)
1
2
3
dataurl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
download.file(url = dataurl, destfile = "wine.data")
wine_df <- read.csv("wine.data", header = FALSE)
getwd()
write.csv(wine_df,"wine.csv")

str(wine_df)

set.seed(3033)
intrain<- createDataPartition(y = wine_df$V1,p=0.7,list=F)
training<- wine_df[intrain,]
testing<- wine_df[-intrain,]

dim(training);dim(testing)

anyNA(wine_df)

summary(wine_df)

training[["V1"]] = factor(training[["V1"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(V1 ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit

plot(knn_fit)
## Accuracy on Training Data
train_pred <- predict(knn_fit, newdata = training)
train_pred

confusionMatrix(train_pred,training$V1)
## Accuracy on Test Data
test_pred <- predict(knn_fit, newdata = testing)
test_pred

confusionMatrix(test_pred,testing$V1)
## Creating Random Forest Model
library(randomForest)
rffit<- randomForest(V1~., data=training,
                     trControl=trctrl)

plot(rffit)

# Create Predictions with training data
pred_rf_train<- predict(rffit,newdata = training)
pred_rf_train
confusionMatrix(pred_rf_train,training$V1)

#Create Predictions with testing data
pred_rf_test<- predict(rffit,newdata = testing)
pred_rf_test
confusionMatrix(pred_rf_test,testing$V1)

#Creating Decision Tree
library(rpart)
tree<-rpart(V1~., data=training,method="class", 
            control = rpart.control(minsplit = 40, minbucket = 12, maxdepth = 10, usesurrogate = 2, xval =10 ))

plot(tree)
text(tree)
# Create Predictions with training data
tree_train<- predict(tree,newdata = training)
tree_train
confusionMatrix(tree_train,training$V1)

#Create Predictions with testing data
tree_test<- predict(tree,newdata = testing)
tree_test
confusionMatrix(tree_test,testing$V1)

## Creating SVM Model
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(V1 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
plot(svm_Linear)
# Create Predictions with training data
svm_train<- predict(svm_Linear,newdata = training)
svm_train
confusionMatrix(svm_train,training$V1)

#Create Predictions with testing data
svm_test<- predict(svm_Linear,newdata = testing)

confusionMatrix(svm_test,testing$V1)

## Non Linear kernel
svm_Radial <- train(V1 ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
plot(svm_Radial)
# Create Predictions with training data
rad_train<- predict(svm_Linear,newdata = training)
rad_train
confusionMatrix(rad_train,training$V1)

#Create Predictions with testing data
rad_test<- predict(svm_Linear,newdata = testing)

confusionMatrix(rad_test,testing$V1)




