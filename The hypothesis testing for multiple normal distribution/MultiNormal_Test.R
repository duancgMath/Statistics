

## =========== The hypothesis testing for multiple normal distribution =============


## ------------------ testing for parameter mu  ------------------------------------- 

mu.test.givenSigma <- function(data, mu0, Sigma0, alpha=0.05)   
# ================================================================================ #
# H0: mu=mu0 when Sigma0 is given
# this is a Chisq testing
# ------------------------- Input ----------------------------------------------- #
# data  = design matrix with the ith sample in the ith line
# mu0   = mu0 for null hypothesis
# Sigma0= the known variance matrix
# alpha = the significant level, default value = 0.05
# ------------------------ Output ----------------------------------------------- #
# Reject.area = reject region
# p.value     = p value
# =============================================================================== #
{
data <- as.matrix(data)
n <- nrow(data)
p <- ncol(data)

X.bar <- colMeans(data)
temp <- X.bar-mu0
T.squ <- n*t(temp)%*%solve(Sigma0)%*%(temp)

a <- qchisq(1-alpha, p)

reject <- matrix(c(T.squ, a), nrow=1)
rownames(reject) <- c("Reject")
colnames(reject) <- c("Obs", ">1-alpha")

p.val <- 1-pchisq(T.squ, p)
return(list(Reject.area=reject, p.value=p.val))
}


mu.test <- function(data, mu0)   
# ================================================================================ #
# H0: mu=mu0 when Sigma is unknown
# this is an F testing
# ------------------------- Input ----------------------------------------------- #
# data  = design matrix with the ith sample in the ith line
# mu0   = mu0 for null hypothesis
# ------------------------- Output ----------------------------------------------- #
# p.value     = p value
# ================================================================================ #
{
data <- as.matrix(data)
n <- nrow(data)
p <- ncol(data)

X.bar <- colMeans(data)
A <- (n-1)*var(data)

T2 <- (n-1)*n*t(X.bar-mu0)%*%solve(A)%*%(X.bar-mu0)
F <- (n-p)/((n-1)*p)*T2

p.two <- 1-pf(F, p, n-p)
return(list(stat.obs=NULL,p.value=p.two))
}



## ------ two independent normal distribution with equal covariance---------------
two.mu.equalCov <- function(data1, data2)   
# ================================================================================= #
# H0: mu1=mu2 
# this is an F testing
# ------------------------------ Input -------------------------------------------- #
# data1  = design matrix for X with the ith sample in the ith line
# data2  = design matrix for X with the ith sample in the ith line
# ------------------------------ Output ------------------------------------------- #
## p.value     = p value
# ================================================================================= #
{
data1=as.matrix(data1)
data2=as.matrix(data2)
n1=nrow(data1)
n2=nrow(data2)
p=ncol(data1)

X.bar=apply(data1, 2, mean) 
A1=(n1-1)*var(data1)
Y.bar=apply(data2, 2, mean)
A2=(n2-1)*var(data2) 
A=(A1+A2)/(n1+n2-2)

T2=(n1*n2/(n1+n2))*t(X.bar-Y.bar)%*%solve(A)%*%(X.bar-Y.bar)
F=(n1+n2-2-p+1)/((n1+n2-2)*p)*T2

p.two=1-pf(F, p, (n1+n2-p-1))
return(list(p.value=p.two))
}

## ------ two independent normal distribution with unequal covariance---------------
two.mu.unequalCov <- function(data1, data2)
# ================================================================================= #
# H0: mu1=mu2 
# this is an F testing
# ------------------------------ Input -------------------------------------------- #
# data1  = design matrix for X with the ith sample in the ith line
# data2  = design matrix for X with the ith sample in the ith line
# ------------------------------ Output ------------------------------------------- #
## p.value     = p value
# ================================================================================= #
{
  data1 <- as.matrix(data1)
  data2 <- as.matrix(data2)
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  p <- ncol(data1)
  
  if (n1==n2) {
    dataZ <- data1-data2;
    test.result <- mu.test(dataZ, 0)
  } else if (n1<n2) {
    dataY  <- data2[1:n1, ]
    sumY1 <- (1/sqrt(n1*n2))*colSums(dataY);
    sumY2 <- (1/n2)*colSums(data2);
    dataZ <- t(t(data1-sqrt(n1/n2)*dataY)+sumY1-sumY2)
    test.result <- mu.test(dataZ, 0)
  } else {
    dataX <- data1[1:n2, ]
    sumX1 <- (1/sqrt(n2*n1))*colSums(dataX);
    sumX2 <- (1/n1)*colSums(data1);
    dataZ <- t(t(data2-sqrt(n2/n1)*dataX)+sumX1-sumX2)
    test.result <- mu.test(dataZ, 0)
  }
  return(list(stat.obs=test.result$stat.obs, p.value=test.result$p.value))
}


## ================== k independent normal distribution ===========================  

multi.mu.test <- function(data, k)            
# ================================================================================= #
# H0: mu1=mu2=...=muk
# this is asymptotically a Chisq testing
# ------------------------------ Input -------------------------------------------- #
## data  = design matrix with a group index ind
# ------------------------------ Output ------------------------------------------- #
## p.value     = p value
# ================================================================================= #
{
ind=data$ind

n=nrow(data)
p=ncol(data)-1

data=data[ ,1:p]
T=(n-1)*var(data)
  
A=0
for (i in 1:k)                                
{
datai=data[ind==i, ]
ni=nrow(datai)                                 
A=A+(ni-1)*var(datai)
}

Lambda=det(A)/det(T)
n1=n-k
n2=k-1
r=n1-(p-n2+1)/2
Chi=(-1)*r*log(Lambda)

p.value=1-pchisq(Chi, p*n2)
return(p.value=p.value)
}


## ========================= variance testing =======================================


var.test <- function(data, Sigma0)
# ================================================================================= #
# H0: Sigma=Sigma0
# this is aymptotically a Chisq testing
# ------------------------------ Input -------------------------------------------- #
# data  = design matrix with the ith sample in the ith line
# Sigma0= Simga0 for null hypothesis
# ------------------------------ Output -------------------------------------------- #
# p.value     = p value
# ================================================================================= #
{
n=nrow(data)
p=ncol(data)

A=(n-1)*var(data)
S=A%*%solve(Sigma0)

lambda=exp(sum(diag((-1)*S/2)))*(det(S))^(n/2)*(exp(1)/n)^(n*p/2)
T5=-2*log(lambda)

p.value=1-pchisq(T5, p*(p+1)/2)
return(p.value)
}

## ======================== k independent normal distribution ============================ 
multi.var.test <- function(data, k)
# ================================================================================= #
# H0: Sigma1=Sigma2=...=Sigmak
# this is asymptotically a Chisq testing
# ------------------------------ Input -------------------------------------------- #
# data  = design matrix with a group index ind
# ------------------------------ Output -------------------------------------------- #
# p.value     = p value
# ================================================================================= #
{
ind=data$ind

n=nrow(data)
p=ncol(data)-1
data=data[ ,1:p]

A=0
for (i in 1:k)                                
{
datai=data[ind==i, ]
ni=nrow(datai)                                 
A=A+(ni-1)*var(datai)
}
  
det.A=0
for (i in 1:k)                                
{
datai=data[ind==i, ]
ni=nrow(datai)                                 
det.A=det.A+(ni-1)*log(det(var(datai)))
}

M=(n-k)*log(det(A/(n-k)))-det.A
d=(2*p^2+3*p-1)*(k+1)/(6*(p+1)*(n-k))
f=p*(p+1)*(k-1)/2

T6=(1-d)*M

p.value=1-pchisq(T6, f)
return(p.value=p.value)
}


