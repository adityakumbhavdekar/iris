###
setwd("C:/Users/babycorn/Documents/SelfStudy/clustering and Decision tree")
library(amap)
##Read the data in the file
cust_data<-read.csv("Insurance_Dataset_Clustering_Analysis.csv")
### Select the requried columns for clustering
cust_data<-cust_data[,c(1,2,4,5,7,9,10,13)]

###Verify the data
colnames(cust_data)
head(cust_data)

###Run the kmeans algorithm to generate the clusters
k1<-Kmeans(cust_data[,-c(1)], 3, iter.max = 200, nstart = 1, method = c("euclidean"))

###See the clustering results
###Fetch the group means for each variable
k1$centers

###Fetch size/n of obs for the groups
k1$size
###Fetch sum of squared  for the groups
k1$withinss
###Fetch the cluster for each obs
k1$cluster




