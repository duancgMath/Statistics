


R <- read.csv("faCov.csv", header=F)
R <- as.matrix(R)

fa <- factanal(factors=2, covmat=R)
print(fa)
