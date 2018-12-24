

## ======================================================================================= ##
#                     This is an example of K-means clust
## ======================================================================================= ##

## ---------------------- Data Reading and Preprocessing ----------------------------------
Province.data0 <- read.csv("1999city.csv")
Province.data <- as.matrix(Province.data0[,2:9])
rownames(Province.data) <- Province.data0[,1]
Province.data.sd <- scale(Province.data)
# Remark: Genernally, we only scale numeric data rather than logical data 

## ----------------------------------- K-means -----------------------------------------------
km <- kmeans(Province.data.sd, 5, nstart=20)
print(km)
sort(km$cluster)

## ---------------------------------- Visualization -----------------------------------------
library("cluster") 
clusplot(Province.data.sd, km$cluster)



## ======================================================================= ##
#                                A New Example
## ======================================================================= ##

Province.data0 <- read.csv("2015city.csv")
Province.data <- as.matrix(Province.data0[,2:9])
rownames(Province.data) <- Province.data0[,1]
Province.data.sd <- scale(Province.data)

km <- kmeans(Province.data.sd, 5, nstart=20)
print(km)
sort(km$cluster)

library("cluster") 
jpeg("dose12.jpeg", quality = 100)
clusplot(Province.data.sd, km$cluster,labels=2)
dev.off()
