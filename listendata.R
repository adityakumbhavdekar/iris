x= 150
x^(1/2)
library(rattle)
rattle()
setwd("C:/Users/DR-STRANG/Desktop/Aditya/R/Listendata")

## Ifelse() and Nested Ifelse()
set.seed(123)
mydata = data.frame(x1 = seq(1,20,by=2),
                    x2 = sample(100:200,10,FALSE),
                    x3 = LETTERS[1:10])
#Example 1 : Simple IF ELSE Statement

#Suppose you are asked to create a binary variable - 1 or 0 based on the variable 'x2'. 
#If value of a variable 'x2' is greater than 150, assign 1 else 0.

mydata$x4 = ifelse(mydata$x2>150,1,0)


#Create variable in a new data frame

#Suppose you need to add the above created binary variable in a new data frame. 
#You can do it by using the code below -
x = ifelse(mydata$x2>150,1,0)
newdata = cbind(x,mydata)


##Apply ifelse() on Character Variables

#If variable 'x3' contains character values - 'A', 'D', the variable 'x1' should be multiplied by 2.
#Otherwise it should be multiplied by 3
mydata$y = ifelse(mydata$x3 %in% c("A","D") ,mydata$x1*2,mydata$x1*3)


#Example 2 : Nested If ELSE Statement in R

#Multiple If Else statements can be written similarly to excel's If function. In this case, we are telling R to multiply variable x1 by 2 if variable x3 contains values 'A' 'B'. If values are 'C' 'D', multiply it by 3. Else multiply it by 4.
mydata$y = ifelse(mydata$x3 %in% c("A","B") ,mydata$x1*2,
                  ifelse(mydata$x3 %in% c("C","D"), mydata$x1*3,
                         mydata$x1*4))
k = 100
if(k > 100){
  print("Greater than 100")
} else if (k < 100){
  print("Less than 100")
} else {
  print ("Equal to 100")
}


df=data.frame(k=c(2,NA,3,4,5))
library(sqldf)
sqldf(
  "SELECT *,
  CASE WHEN (k%2)=0  THEN 'Multiple of 2'
  WHEN  k is NULL  THEN 'Missing'
  ELSE 'Not a multiple of 2'
  END AS T
  FROM df"
)


#>>>>>>>>>>>>
# Transposing the data
data <- read.table(text="X Y    Z
                   ID12   2012-06    566
                   ID1    2012-06  10239
                   ID6    2012-06    524
                   ID12   2012-07   2360
                   ID1    2012-07   13853
                   ID6    2012-07    2352
                   ID12   2012-08   3950
                   ID1    2012-08   14738
                   ID6    2012-08   4104",header=TRUE)


library(reshape2)
#Transform Long to Wide Format
mydt = dcast(data,X~Y,value.var = "Z")


#>>>>>>>>>>>>
#For Loop in R
# Create Sample Data Set
dat <- data.frame(x = c(1:5,NA),
                 z = c(1, 1, 0, 0, NA,0),
                 y = 5*c(1:6))
#>>>>>>>>>> Apply function
#Example 1 : Find Maximum value of each row
apply(dat, 1, max, na.rm= TRUE)

#Example 2 : Find Maximum value of each column
apply(dat, 2, max, na.rm= TRUE)

#>>>>>>>>>> Lapply function
#Example 1 : Calculate Median of each of the variables
lapply(dat, function(x) median(x, na.rm = TRUE))


##>>>>>>>>>>>>>>>> Sapply Function
#Example 1 : Number of Missing Values in each Variable
sapply(dat, function(x) sum(is.na(x)))

#Example 2 : Extract names of all numeric variables in IRIS dataset
colnames(iris)[which(sapply(iris,is.numeric))]


#>>>>>>>>>>>>>>>>>
##Lapply and Sapply Together

#In this example, we would show you how both lapply and sapply are used simultaneously to solve the problem.

#Create a sample data
dat <- data.frame(x = c(1:5,NA),
                  z = c(1, 1, 0, 0, NA,0),
                  y = factor(5*c(1:6)))

#Converting Factor Variables to Numeric

#The following code would convert all the factor variables of data frame 'dat' to numeric types variables.
index <- sapply(dat, is.factor)
dat[index] <- lapply(dat[index], function(x) as.numeric(as.character(x)))


#>>>> For LOOP Function
#Example 1 : Maximum value of each column
x = NULL
for (i in 1:ncol(dat)){
  x[i]= max(dat[i], na.rm = TRUE)}
x
# Min
x = NULL
for (i in 1:ncol(dat)){
  x[i]= min(dat[i], na.rm = TRUE)}
x



#The above FOR LOOP program can be written like the code below -
x = vector("double", ncol(dat))
for (i in seq_along(dat)){
  x[i]= max(dat[i], na.rm = TRUE)}
x 


#Example 2 : Split IRIS data based on unique values in "species" variable

#The program below creates multiple data frames based on the number of unique values in variable Species in IRIS dataset.
for (i in 1:length(unique(iris$Species))) {
require(dplyr)
  assign(paste("iris",i, sep = "."), filter(iris, Species == as.character(unique(iris$Species)[i])))
}


#Combine / Append Data within LOOP
#Method 1 : Use do.call with rbind
#do.call() applies a given function to the list as a whole. 
#When it is used with rbind, it would bind all the list arguments. 
#In other words, it converts list to matrix of multiple rows.

temp =list()
for (i in 1:length(unique(iris$Species))) {
  series= data.frame(Species =as.character(unique(iris$Species))[i])
temp[[i]] =series
}
output = do.call(rbind, temp)
output

#Method 2 :  Use Standard Looping Technique
#In this case, we are first creating an empty table (data frame). Later we are appending data to empty data frame.
dummydt=data.frame(matrix(ncol=0,nrow=0))
for (i in 1:length(unique(iris$Species))) {
  series= data.frame(Species =as.character(unique(iris$Species))[i])
  if (i==1) {output = rbind(dummydt,series)}  else {output = rbind(output,series)}
}
output


dummydt=data.frame(matrix(ncol=0,nrow=0))
temp = function(data, var) {
for (i in 1:length(unique(data[[var]]))) {
  series= data.frame(Species = as.character(unique(data[[var]]))[i])
  if (i==1) {output = rbind(dummydt,series)}  else {output = rbind(output,series)}
}
return(output)}
temp(iris, "Species")

##>>>>>>>>>>>>>>>>>>>>
#For Loop and Sapply Together

for (i in which(sapply(dat, is.numeric))) {
  dat[is.na(dat[, i]), i] <- median(dat[, i],  na.rm = TRUE)
}


#>>>>>>>>>>
# Create New Columns Using LOOP
#Suppose you need to standardise multiple variables. To accomplish this task, we need to execute the following steps -
 #Identify numeric variables
#Calculate Z-score i.e. subtracting mean from original values and then divide it by standard deviation of the raw variable.
#Run Step2 for all the numeric variables
#Make names of variables based on original names. For example x1_scaled.

#Create dummy data
mydata = data.frame(x1=sample(1:100,100), x2=sample(letters,100, replace=TRUE), x3=rnorm(100))
#Standardize Variables
lst=list()
for (i in which(sapply(mydata, is.numeric))) {
x.scaled = (mydata[,i] - mean(mydata[,i])) /sd(mydata[,i])
lst[[i]] = x.scaled
}

names(lst) <- paste(names(sapply(mydata, is.numeric)),"_scaled", sep="")
mydata.scaled= data.frame(do.call(cbind, lst))

a <- factor(c(2, 4, 3, 3, 4))
str(a)

a1 = as.numeric(a)
str(a1)
  
a2 = as.numeric(as.character(a))
str(a2)

x = sample(LETTERS,100, replace = TRUE)
x[x %in% c("A","B","C")]

#Apply function on rows
#Sample Data
data = read.table(text="
X Y Z
6 5 0
6 3 NA
6 1 5
8 5 3
1 NA 1
8 7 2
2 0 2", header=TRUE)

#Apply Function

#When we want to apply a function to the rows or columns of a matrix or data frame. 
#It cannot be applied on lists or vectors.


#apply arguments

#Calculate maximum value across row
apply(data, 1, max)
#It returns NA if NAs exist in a row. To ignore NAs, you can use the following line of code.
apply(data, 1, max, na.rm = TRUE)
#Calculate mean value across row
apply(data, 1, mean)
apply(data, 1, mean, na.rm = TRUE)
#Calculate number of 0s in each row
apply(data == 0, 1, sum, na.rm= TRUE)
#Calculate number of values greater than 5 in each row
apply(data > 5, 1,  sum, na.rm= TRUE)
#Select all rows having mean value greater than or equal to 4
df = data[apply(data, 1, mean, na.rm = TRUE)>=4,]
#Remove rows having NAs
helper = apply(data, 1, function(x){any(is.na(x))})
df2 = data[!helper,]
#It can be easily done with df2 = na.omit(data).

#Count unique values across row
df3 = apply(data,1, function(x) length(unique(na.omit(x))))


  