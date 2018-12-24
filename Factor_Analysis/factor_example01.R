

## ========================================================================= ##
#                 An Example of Factor Analysis Model
## ========================================================================= ##

## ----------------- Read data ------------------------
rt <- read.csv("applicant.csv")
data <- rt[,-1]

## ----------------- Factor Analysis ------------------

# Factor Analysis using different methods
# Method 1
fa1 <- factanal(~., factors=5, data=data)
print(fa1)
A <- loadings(fa1)
A <- as.matrix(A[,])
# Method 2
fa2=factanal(~., factors=5, data=data, scores="Bartlett")
print(fa2)
fa2$scores
# Method 3
fa3=factanal(~., factors=5, data=data, scores="regression")
print(fa3)
fa3$scores

# Visualiztion
plot(fa2$scores[, 1:2], type="n")
text(fa2$scores[,1], fa2$scores[,2])

plot(fa2$scores[, c(1,3)], type="n")
text(fa2$scores[,1], fa2$scores[,3])

plot(fa3$scores[, 1:2], type="n")
text(fa3$scores[,1], fa3$scores[,2])


