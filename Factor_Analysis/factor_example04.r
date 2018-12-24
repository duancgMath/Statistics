

## ========================================================================= ##
#        An Example of Factor Analysis Model with Package -- psych
## ========================================================================= ##

library(psych)

R <- read.csv("faCov.csv", header=F)
data <- as.matrix(R)

fa.parallel(data, fa="fa", show.legend=F, main="Scree plot with parallel analysis")
fa.parallel(data, fa="both", show.legend=F, main="Scree plot with parallel analysis")

fa1 <- fa(data, nfactors=2, rotate="none")    
fa1 <- fa(data, nfactors=2, rotate="none", fm="ml")
fa1 <- fa(data, nfactors=2, rotate="none", fm="pa")
fa1 <- fa(data, nfactors=2, rotate="none", fm="wls")
fa1 <- fa(data, nfactors=2, rotate="none", fm="gls")
fa1

fa2 <- fa(data, nfactors=2, rotate="varimax", fm="pa")
fa2
fa.diagram(fa2, simple=F)
factor.plot(fa2, labels=rownames(fa2$loadings))

