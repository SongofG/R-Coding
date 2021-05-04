# AMS 597 HW #4
# Q1) Best Fitting Model
    # a)
data <- read.delim('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/leukemiaDataSet.txt', header=T, sep='\t')

str(data)

set.seed(123)
trainID <- sample(1:72, round(0.7*72))

trainData <- data[trainID, ]
testData <- data[-trainID, ]

    # b)
library(glmnet)

# Getting an optimal lambda for Ridge Regression, 10-fold cross validation by default.
x.train <- data.matrix(trainData[, -1])
x.test <- data.matrix(trainData[, 1])
alpha0.fit <- cv.glmnet(x.train, x.test, type.measure = "deviance", alpha = 0, family = "binomial")
alpha0.predicted <- predict(alpha0.fit, s = alpha0.fit$lambda.1se, newx = x.train)
# Q2) Generating a random exponential function and get a sample size of 1000 from the Exp(2, 1)
my.rexp <- function(n, lambda, v){
  # Generate a sample from runif with size n
  u <- runif(n)
  
  # Plug u in the inverse function of cdf of exponential distribution
  # Since 1-U follows U. log(u) works
  x = v - (log(u)/lambda)
  return(x)
}

result1 = my.rexp(1000, 2, 1)


# Q3) Generating a random sample from Standard Cauchy distribution.
my.Cauchy <- function(n){
  # Generate a sample from runif with size n
  u <- runif(n)
  
  # Plug u in the inverse function of cdf of Cauchy distribution
  x <- tan(pi*(u-1/2))
  return(x)
}

result2 <- my.Cauchy(1000)
qqplot(result2, rcauchy(1000))
abline(0,1,col="red")

# Q4) Generating a random sample with size n from BETA(a,b)
# Using the property of Beta distribution that the maximum value can be found
# by (alpha - 1)/(alpha + beta -2)

beta.value <- function(a, b, x){(factorial(a+b-1)/(factorial(a-1)*factorial(b-1)))*(x^(a-1)*(1-x)^(b-1))}

my.rbeta <- function(n, a, b){
  result <- c() # result vector
  k <- 0
  x <- (a-1)/(a+b-2)
  c <- beta.value(a, b, x) # Calculating c
  while(k<n){
    y <- runif(1) # step 1
    u <- runif(1) # step 2
    if(u <= beta.value(a, b, y)/c){ # step 3
      k <- k+1
      result[k] = y
    }
  }
  return(result)
}
result3 <- my.rbeta(1000, 3, 2)
qqplot(result3, rbeta(1000, 3, 2))
abline(0,1,col="red")


# Q5) Generating random Gamma(alpha, 1)

my.rgamma <- function(n, a){
  result <- c()
  c <- optimise(function(x){(a/factorial(a-1))*(x^(a-1))*(exp(1)^(-x+x/a))}, interval = c(0,1000), maximum = TRUE)$objective
  k <- 0
  while(k < n){
    y <- rexp(1, 1/a) # step 1
    u <- runif(1) # step 2
    if(u <= (a/(c*factorial(a-1)))*(y^(a-1)*exp(1)^(-y+y/a))){
      k <- k+1
      result[k] <- y # step 3
    }
  }
  return(result)
}

result4 <- my.rgamma(1000, 3)
qqplot(result4, rgamma(1000, 3))
abline(0,1,col="red")












