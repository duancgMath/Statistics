
## ------------------------------------------------ ##
## Basic use of PCA 
## ------------------------------------------------ ##

# ---------------- read data ---------------------- # 
student <- read.csv("student.csv")
print(student)

# ---- principal components analysis  ------------- #
# student.pr <- princomp(student, cor=TRUE)
student.pca <- princomp(~X1+X2+X3+X4, data=student, cor=TRUE) 

#----------------- summary ------------------------ #
summary(student.pca, loadings=TRUE)

# ---------------- scores ------------------------ #
predict(student.pca)

# ------------ screepplot ------------------------- #
screeplot(student.pca)
screeplot(student.pca, type="lines")
biplot(student.pca)
