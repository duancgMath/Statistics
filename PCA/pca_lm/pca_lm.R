

rm(list=ls())
## ---------------- read data ---------------------- 
data <- read.csv("conomy.csv")
data


## ====================================================

nr <- nrow(data)
nc <- ncol(data)
## ---------------- linear model  ---------------------
lm.sol1 <- lm(y~x1+x2+x3, data=data)
# lm.sol1 <- lm(y~x1+x2+x3-1, data=conomy) # do not include intercept
summary(lm.sol1)

## --------- principal components analysis -------------
X.pca = princomp(~x1+x2+x3, data=data, cor=T)
summary(X.pca, loadings=TRUE)

## -------- principal components regression ------------
pre <- predict(X.pca)
z1 <- pre[,1]
z2 <- pre[,2]
z3 <- pre[,3]
lm.sol2 <- lm(y~z1+z2+z3, data=data)
summary(lm.sol2)

## ----------- Reduce dimension: n->p -----------------
p <- 3

## --------------- Back to X ------------------------- 
A <- loadings(X.pca)
A <- as.matrix(A)
A <- t(A[,])
coef.z <- coef(lm.sol2)
beta0 <- coef.z[1]; 
beta.x <- coef.z[2:4]
X.bar=X.pca$center
X.sd=X.pca$scale
coef.mat <- beta.x*A
coef.mat <- coef.mat[1:p,]
coef.x <- colSums(coef.mat)/X.sd
coef.inter <- beta0-sum(coef.x*X.bar)
coef.result <- c(coef.inter, coef.x)
coef.result
