

## ========================================================================= ##
#        An Example of Factor Analysis Model with Package -- psych
## ========================================================================= ##

library(psych)

# Read data
rt <- read.csv("applicant.csv")
data <- rt[,-1]

# Scree plot 
# one way to determine the number of factors or components in a data matrix or a 
# correlation matrix is to examine the “scree" plot of the successive eigenvalues. 
fa.parallel(data, fa="fa", show.legend=F, main="Scree plot with parallel analysis")
fa.parallel(data, fa="both", show.legend=F, main="Scree plot with parallel analysis")

# Factor Analysis with none rotate
fa1=fa(data, nfactors=3, rotate="none")    
fa1=fa(data, nfactors=3, rotate="none", fm="ml")
fa1=fa(data, nfactors=3, rotate="none", fm="pa")
fa1=fa(data, nfactors=3, rotate="none", fm="wls")
fa1=fa(data, nfactors=3, rotate="none", fm="gls")
fa1

# Factor Analysis with rotate based on varmax
fa2=fa(mydata, nfactors=3, rotate="varimax", fm="pa")
fa2
fa.diagram(fa2, simple=F)

fa3=fa(mydata, nfactors=2, rotate="varimax", fm="pa")
fa3
factor.plot(fa3, labels=rownames(fa3$loadings))

