setwd("C:/Users/DR-STRANG/Desktop/Aditya/R/Standardizing the data")
# Creating a sample data
set.seed(123)
X =data.frame(k1 = sample(100:1000,1000, replace=TRUE),
              k2 = sample(10:100,1000, replace=TRUE))
X.scaled = scale(X, center= TRUE, scale=TRUE)

#Check Mean and Variance of Standardized Variable
colMeans(X.scaled)
var(X.scaled)

#Min-Max Scaling
#The formula is shown below -
 # x-min(x)/(max(x)-min(x))
library(dplyr)
mins= as.integer(summarise_all(X, min))
rng = as.integer(summarise_all(X, function(x) diff(range(x))))
X.scaled = data.frame(scale(X, center= mins, scale=rng))

#Check Min and Max of standardized variables
summarise_all(X.scaled, funs(min, max))

#Standard Deviation Method

#In this method, we divide each value by the standard deviation. 
#The idea is to have equal variance, but different means and ranges. Formula : x/stdev(x)
X.scaled = data.frame(scale(X, center= FALSE , scale=apply(X, 2, sd, na.rm = TRUE)))
plot(X.scaled)

#Check Equal Variance
summarise_all(X.scaled, var)


#Range Method

#In this method, we dividing each value by the range. 
#Formula : x /(max(x) - min(x)). In this case, the means, variances, and ranges of the variables are still different, 
#but at least the ranges are likely to be more similar.
library(dplyr)
rng = as.integer(summarise_all(X, function(x) diff(range(x))))
X.scaled = data.frame(scale(X, center= FALSE, scale=rng))
summarise_all(X.scaled, var)

#Centering
X=sample(1:100,1000, replace=TRUE)
scale(X,center = TRUE, scale=FALSE)

## Linear regression without standardization
# Create Sample Data
set.seed(123)
train <- data.frame(X1=sample(1:100,1000, replace=TRUE),
                    X2=1e2*sample(1:500,1000, replace=TRUE),
                    X3=1e-2*sample(1:100,1000, replace=TRUE))
train$y <- with(train,2*X1 + 3*1e-2*X2 - 5*1e2*X3 + 1 + rnorm(1000,sd=10))

#Fit linear regression model
fit  <- lm(y~X1+X2+X3,train)
summary(fit)


## Predict on test data
# create test dataset
set.seed(456)
test <- data.frame(X1=sample(-5:5,100,replace=TRUE),
                   X2=1e2*sample(-5:5,100, replace=TRUE),
                   X3=1e-2*sample(-5:5,100, replace=TRUE))
# predict y based on test data without standardization
pred   <- predict(fit,newdata=test)
head(cbind(test, pred))


## Linear regression with Standardization

# Standardize predictors
means   <- sapply(train[,1:3],mean)
stdev <- sapply(train[,1:3],sd)
train.scaled <- as.data.frame(scale(train[,1:3],center=means,scale=stdev))
head(train.scaled)
train.scaled$y <- train$y
# Check mean and Variance of Standardized Variables
library(dplyr)
summarise_at(train.scaled, vars(X1,X2,X3), funs(round(mean(.),4)))
summarise_at(train.scaled, vars(X1,X2,X3), var)

#Fit Scaled Data
fit.scaled <- lm(y ~ X1 + X2 + X3, train.scaled)
summary(fit.scaled)

 

test.scaled <- as.data.frame(scale(test,center=means,scale=stdev))
head(test.scaled)

# predict y based on new data scaled, with fit from scaled dataset
pred.scaled   <- predict(fit.scaled,newdata=test.scaled)

# Compare Prediction - unscaled vs. scaled fit
all.equal(pred,pred.scaled)

head(cbind(pred,pred.scaled),n=10)

#Compare RMSE Score
# RMSE on train data with un-scaled fit
pred_train   <- predict(fit,newdata=train)
rmse <- sqrt(mean((train$y - pred_train)^2))
# RMSE on train data with scaled fit
pred_train.scaled   <- predict(fit.scaled,newdata=train.scaled)
rmse.scaled <- sqrt(mean((train$y - pred_train.scaled)^2))
# Compare RMSE
all.equal(rmse,rmse.scaled) 

