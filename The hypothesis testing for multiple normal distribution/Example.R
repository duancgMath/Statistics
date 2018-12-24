

library(MASS)
##===================== Examples ========================
source("MultiNormial_Test.r")

## ------------------- Example 1 -----------------------
wheat <- read.csv("wheat.csv")
wheat

mu0 <- c(22.75, 32.75, 51.50, 61.50)
# Testing for mu while sigma is not given
wheat.result <- mu.test(wheat, mu0)
print(wheat.result)


## ------------------- Example 2 -----------------------
female.data <- read.csv("female.csv")
mu0 <- c(4, 50, 10)
Sigma0 <- var(female.data)


# Testing for mu while sigma is given
female.result1=mu.test.givenSigma(female.data, mu0, Sigma0)
print(female.result1)

female.data=read.csv("female.csv")
mu0 <- c(4, 50, 10)
# Testing for mu while sigma is not given
female.result2 <- mu.test(female.data, mu0)
print(female.result2)

## ------------------- Example 3 -----------------------
japan <- read.csv("Economy_Japan.csv")
usa <- read.csv("Economy_USA.csv") 

# Testing for mu of two independent normal distribution
economy.result <- two.mu.equalCov(japan, usa)
print(economy.result)


## ------------------- Example 4 -----------------------
health <- read.csv("health.csv")
health.result1 <- multi.mu.test(health, 3) 
print(health.result1)

## ------------------- Example 5 -----------------------
health <- read.csv("health.csv") 
health.result2 <- multi.var.test(health, 3)
print(health.result2)

## Simulations

## ------------------ Example 6 ----------------------
sigma <- matrix(c(1,0.5,1,0.5),2,2)
mu0 <- c(1,1)
# Test
errNum <- 0
N <- 1000
for (i in 1:N) {
  data <- mvrnorm(100,mu0,sigma)
  result <- mu.test(data,c(1,1))
  p.value <- result$p.value
  if (p.value>=0.05) {
    errNum <- errNum + 1
  }
}
EP <- 1-errNum/N

## ------------------- Example 7 -----------------------
sigma = matrix(c(1,0.5,1,0.5),2,2)
mu0 = c(0,0)
# Test
errNum = 0
N = 1000
for (i in 1:N) {
  data = mvrnorm(100,mu0,sigma)
  result = mu.test(data,c(0,0))
  p.value = result$p.value
  if (p.value<0.05) {
    errNum = errNum + 1
  }
}

EE <- errNum/N

## -------------------- Example 8 ---------------------
eco_Jap  <-  read.csv("Economy_Japan.csv")
eco_Jap2 <- eco_Jap[-10, ]
eco_USA  <- read.csv("Economy_USA.csv")

result  = two.mu.unequalCov(eco_Jap, eco_USA)
result2 = two.mu.unequalCov(eco_Jap2, eco_USA)

print(result)
print(result2)


