#Importing the packages
library(readxl)
library(dplyr)
library(factoextra)
library(cluster)
library(NbClust)
library(caret)
library(tidyverse)
library(ggplot2)


#accessing the data set to a new variable
Whitewine_v2 <- read_excel("D:/IIT/Year 2/Sem 2/Machine Learning/CW/Whitewine_v2.xlsx")

#viewing the data set
View(Whitewine_v2)

#plotting the data set
boxplot(Whitewine_v2)

#viewing the summary of the dataset
summary(Whitewine_v2)

#function for identifying the outliers
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

#function for removing outliers
remove_outliers <- function(Whitewine_v2, cols = names(Whitewine_v2)) {
  for (col in cols) {
    Whitewine_v2 <- Whitewine_v2[!outliers(Whitewine_v2[[col]]),]
  }
  Whitewine_v2
}


#removing the outliers from the dataset and assigning them to a new dataset
newdata <- remove_outliers(Whitewine_v2, c('fixed acidity', 'volatile acidity', 'citric acid', 'residual sugar','chlorides','free sulfur dioxide','total sulfur dioxide', 'density', 'pH', 'sulphates', 'alcohol', 'quality' ))

#plotting the data set without outliers
boxplot(newdata)

cleanedNew <- subset(newdata, select = -(quality))

#scaling the data set
wineDataScale <- scale(newdata)

#viewing the new scaled data set without the outliers
View(wineDataScale)

#plotting the new scaled data set without the outliers
boxplot(wineDataScale)

#applying the elbow method to get cluster centeres
fviz_nbclust(wineDataScale, kmeans, method = "wss")+
  labs(subtitle = "Elbow Method")

wss

#applying the Silhouette method to get cluster centeres
fviz_nbclust(wineDataScale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#getting cluster centers with NbClust
clusterNo = NbClust(wineDataScale, distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
clusterNo


#applying k means when k = 2
km.out2 <- kmeans(wineDataScale, centers =2, nstart =50)
#printing the results
km.out2
km2.clusters <- km.out2$cluster

#plotting the results for k = 2 value
fviz_cluster(list(data=wineDataScale, cluster = km2.clusters))
#printing the TSS results
km.out2$totss
#printing the BSS results
km.out2$betweenss
#printing the WSS results
km.out2$withinss
#printing the ratio between BSS : TSS
ratio2 <- km.out2$betweenss / km.out2$totss
#printing the Cluster Centers
km.out2$centers

#applying k means when k = 3
km.out3 <- kmeans(wineDataScale, centers =3, nstart =50)
#printing the results
km.out3
km3.clusters <- km.out3$cluster
#plotting the results for k = 3 value
fviz_cluster(list(data=wineDataScale, cluster = km3.clusters))

#printing the TSS results
km.out3$totss
#printing the BSS results
km.out3$betweenss
#printing the WSS results
km.out3$withinss
#printing the ratio BSS/TSS
ratio3 <- km.out3$betweenss / km.out3$totss
ratio3
#printing the Cluster Centers
km.out3$centers




kam3 <- kmeans(wineDataScale, centers = 3, nstart =50)
kam4 <- kmeans(newdata, centers = 2, nstart =50)

#applying k means when k = 4
km.out4 <- kmeans(wineDataScale, centers =4, nstart =50)
#printing the results
km.out4
km4.clusters <- km.out4$cluster
#plotting the results for k = 4 value
fviz_cluster(list(data=wineDataScale, cluster = km4.clusters))

#printing the TSS results
km.out4$totss
#printing the BSS results
km.out4$betweenss
#printing the WSS results
km.out4$withinss
#printing the ratio between BSS : TSS
ratio4 <- km.out4$betweenss / km.out4$totss
ratio4
#printing the Cluster Centers
km.out4$centers



#getting the 12 column of the data set
qualityColumn <-factor(newdata$quality)
wineQual <-as.numeric(qualityColumn)

#confusion Matrix for k means = 2

cnMat2 <- confusionMatrix(as.factor(km.out2$cluster), as.factor(wineQual))
cnMat2

#confusion Matrix for k means = 3


cnMat3 <- confusionMatrix(as.factor(kam3$cluster), as.factor(wineQual))
cnMat3


#confusion Matrix for k means = 4
cnMat4 <- confusionMatrix(as.factor(kam4$cluster), as.factor(wineQual))
cnMat4




newScaled <- wineDataScale

#applying principal components analysis
transformed.pca <- prcomp(newScaled, center = TRUE,scale. = TRUE)
plot(transformed.pca)


#creating a new dataset with PCs that provide a cumulative score > 96%
newTransformed.cpa <- as.data.frame(transformed.pca$x[,1:9])
plot(newTransformed.cpa)

#applying K means
km.pca <- kmeans(newTransformed.cpa,centers = 2)

fviz_cluster(list(data = newTransformed.cpa, cluster = km.pca$cluster))
#printing the TSS results
km.pca$totss
#printing the BSS results
km.pca$betweenss
#printing the WSS results
km.pca$withinss
#printing the ratio BSS/TSS
ratio.pca <- km.pca$betweenss / km.pca$totss
ratio.pca
km.pca$centers




