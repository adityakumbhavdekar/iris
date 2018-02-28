# Loading Required Packages
library(caret)
library(caTools)
library(RCurl)
library(pROC)

# Reading data file
urlfile <-'https://raw.githubusercontent.com/hadley/fueleconomy/master/data-raw/vehicles.csv'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
vehicles <- read.csv(textConnection(x))
write.csv(vehicles,"vehicles.csv")
# Cleaning up the data and only use the first 24 columns
vehicles <- vehicles[names(vehicles)[1:24]]
vehicles <- data.frame(lapply(vehicles, as.character), stringsAsFactors=FALSE)
vehicles <- data.frame(lapply(vehicles, as.numeric))
vehicles[is.na(vehicles)] <- 0
vehicles$cylinders <- ifelse(vehicles$cylinders == 6, 1,0)

# Making dependent variable factor and label values
vehicles$cylinders <- as.factor(vehicles$cylinders)
vehicles$cylinders <- factor(vehicles$cylinders,
                             levels = c(0,1),
                             labels = c("level1", "level2"))

# Split data into two sets - Training and Testing
set.seed(107)
inTrain <- createDataPartition(y = vehicles$cylinders, p = .7, list = FALSE)
training <- vehicles[ inTrain,]
testing <- vehicles[-inTrain,]

#Training control
ctrl <- trainControl(
  method = "cv",
  number = 3,
  savePredictions = 'final',
  classProbs = T
)

#Training decision tree
dt <-train(cylinders~., data=training, method="rpart",trControl=ctrl, tuneLength=2)

#Training logistic regression
logit <-train(cylinders~., data=training, method="glm",trControl=ctrl, tuneLength=2)

#Training knn model
knn <-train(cylinders~., data=training, method="knn",trControl=ctrl,tuneLength=2)

#Check Correlation Matrix of Accuracy
results <- resamples(list(dt, logit, knn))
modelCor(results)

#Predicting probabilities for testing data
testing$dt<- predict(dt,testing,type='prob')$level2
colAUC(testing$dt, testing$cylinders)
# 0.9358045

testing$logit<-predict(logit,testing,type='prob')$level2
colAUC(testing$logit, testing$cylinders)
# 0.5054634

testing$knn<-predict(knn,testing,type='prob')$level2
colAUC(testing$knn, testing$cylinders)
# 0.9871729

#Predicting the out of fold prediction probabilities for training data
#In this case, level2 is event
#rowindex : row numbers of the data used in k-fold
#Sorting by rowindex
training$OOF_dt<-dt$pred$level2[order(dt$pred$rowIndex)]
training$OOF_logit<-logit$pred$level2[order(logit$pred$rowIndex)]
training$OOF_knn<-knn$pred$level2[order(knn$pred$rowIndex)]

#GBM as top layer model
model_gbm<- train(training[,c('OOF_dt','OOF_logit','OOF_knn')],
                  training[,"cylinders"],method='gbm',trControl=ctrl,
                  tuneLength=1)

#Predict using GBM
testing$stacking<-predict(model_gbm, testing[,c('dt','logit','knn')], type = 'prob')$level2
colAUC(testing$stacking, testing$cylinders)
# 0.9903686
