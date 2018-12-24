

## ===================================================================================== ##
#        An Example of Hierarchical Clustering using in Real Datas 
## ===================================================================================== #

# data reading and preprocessing
Province.data0 <- read.csv("1999city.csv")
Province.data <- as.matrix(Province.data0[,2:9])
rownames(Province.data) <- Province.data0[,1]
Province.data.sd <- scale(Province.data)

# calulate distances
province.dist <- dist(Province.data.sd, diag=T)

# Hierarchical Clustering based on distances
hc1 <- hclust(province.dist, "single")
hc2 <- hclust(province.dist, "complete")
hc3 <- hclust(province.dist, "median")
hc4 <- hclust(province.dist, "average")
hc5 <- hclust(province.dist, "centroid")
hc6 <- hclust(province.dist, "mcquitty")
hc7 <- hclust(province.dist, "ward.D")
hc8 <- hclust(province.dist, "ward.D2")

# visualization
plot(hc1, hang=-1)
plot(hc2, hang=-1)
plot(hc3, hang=-1)
plot(hc4, hang=-1)
plot(hc5, hang=-1)
plot(hc6, hang=-1)
plot(hc7, hang=-1)
plot(hc8, hang=-1)


# Visualize the results on figures
# Remark: the parameter k in function rect.hclust(hc,k,border)
# means the number of class you need.

jpeg("hclust1.jpeg", height=1200, width = 1600, quality = 100)
opar=par(mfrow=c(2,1), mar=c(5.2,4,0,0))
plot(hc1,hang=-1,cex=2)
re2=rect.hclust(hc1,k=5,border="red")
plot(hc2,hang=-1,cex=2)
re4=rect.hclust(hc2,k=5,border="red")
dev.off()

jpeg("hclust2.jpeg", height=1200, width = 1600, quality = 100)
opar=par(mfrow=c(2,1), mar=c(5.2,4,0,0))
plot(hc3,hang=-1,cex=2)
re5=rect.hclust(hc3,k=5,border="red")
plot(hc4,hang=-1,cex=2)
re6=rect.hclust(hc4,k=5,border="red")
par(opar)
dev.off()

jpeg("hclust3.jpeg", height=1200, width = 1600, quality = 100)
opar=par(mfrow=c(2,1), mar=c(5.2,4,0,0))
plot(hc5,hang=-1,cex=2)
re2=rect.hclust(hc5,k=5,border="red")
plot(hc6,hang=-1,cex=2)
re4=rect.hclust(hc6,k=5,border="red")
dev.off()

jpeg("hclust4.jpeg", height=1200, width = 1600, quality = 100)
opar=par(mfrow=c(2,1), mar=c(5.2,4,0,0))
plot(hc7,hang=-1,cex=2)
re2=rect.hclust(hc7,k=5,border="red")
plot(hc8,hang=-1,cex=2)
re4=rect.hclust(hc8,k=5,border="red")
dev.off()
