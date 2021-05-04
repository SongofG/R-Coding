# AMS_597_HW_5

# Q1) Generating 1000 random samples from F distribution with 5 and 10 degrees of freedom.
    # 1) Generate two 7,500 random uniform samples for a Box-Muller algorithm.
u1 <- runif(2500)
u2 <- runif(2500)
u3 <- runif(5000)
u4 <- runif(5000)

    # 2) Generate 15,000 random standard normal samples using a Box-Muller formula
n1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
n2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
n3 <- sqrt(-2*log(u3))*cos(2*pi*u4)
n4 <- sqrt(-2*log(u3))*sin(2*pi*u4)
Z1 <- matrix(c(n1, n2)^2, ncol = 5) # Standard Normals for making Chi-Square random samples with df 5
Z2 <- matrix(c(n3, n4)^2, ncol = 10) # Standard Normals for making Chi-Square random samples with df 10
Chi1 <- apply(Z1, 1, sum) # 1000 Chi-squared random samples with df 5
Chi2 <- apply(Z2, 1, sum) # 1000 Chi-squared random samples with df 10
F_dist <- (Chi1/5)/(Chi2/10) # F random samples with df 5, 10

hist(F_dist) # result
qqplot(F_dist, rf(1000, 5, 10)) # result qqplot and line
abline(0,1,col="red")

# Q2) Implementation of Polar Method for Generating Standard Normal Random Variables.
Z <- NULL # A vector for collecting the generated samples.
count <- 0 # Counting the number of collected samples.
while(count < 10000){
  # Step (a)
  u_1 <- runif(1)
  u_2 <- runif(1)
  # Step (b)
  v_1 <- 2*u_1 - 1
  v_2 <- 2*u_2 -1
  s <- v_1^2 + v_2^2
  # Step (c)
  if(s <= 1) {
    count <- count + 1
    Z[count] <- sqrt((-2*log(s))/(s))*v_1
    count <- count + 1
    Z[count] <- sqrt((-2*log(s))/(s))*v_2
  }
}

hist(Z) # Result histogram
qqplot(Z, rnorm(10000)) # Result QQ-Plot
abline(0,1, col="red") # Result fitting line

# Q3) Generating 100 random samples from X ~ 0.3*t_3 + 0.35*t_5 + 0.35*t_7
#     Where t_k = Z/sqrt(W/k)
#     Where Z ~ Norm(0, 1), and W ~ Chi_Square with df = k.

# First, construct a function that generates a given number of t random samples with given df

my.rt <- function(n, k){
  # to generate n random t samples, we need n*(k+1) independent standard normal samples.
  Z <- NULL # standard normal random samples collector
  count <- 0 # counting variable
  while(count < n*(k+1)){ # Use Polar method to generate random standard normal samples.
    # Step (a)
    u_1 <- runif(1)
    u_2 <- runif(1)
    # Step (b)
    v_1 <- 2*u_1 - 1
    v_2 <- 2*u_2 -1
    s <- v_1^2 + v_2^2
    # Step (c)
    if(s <= 1) {
      count <- count + 1
      Z[count] <- sqrt((-2*log(s))/(s))*v_1
      count <- count + 1
      Z[count] <- sqrt((-2*log(s))/(s))*v_2
    }
  }
  Z <- matrix(Z, ncol=k+1)
  numerator <- Z[, 1] # Standard normal samples for the numerator of generating t-random sample.
  denominator <- apply(Z[, -1]^2, 1, sum) # Chi-Square random samples for the denominator of generating t-random sample.
  t <- numerator/sqrt(denominator/k) # our random T samples
  return(t)
}

# Since our goal is to generate a mixture distribution, using t-distribution,
# we generate 100 random uniform and use that to generate our goal distribution
Mix <- runif(100)
Mix[which(Mix>0 & Mix<=0.3)] = my.rt(length(Mix[which(Mix>0 & Mix<=0.3)]), 3) # 0.3*t_3
Mix[which(Mix>0.3 & Mix<=0.65)] = my.rt(length(Mix[which(Mix>0.3 & Mix<=0.65)]), 5) # 0.35*t_5
Mix[which(Mix>0.65 & Mix<=1)] = my.rt(length(Mix[which(Mix>0.65 & Mix<=1)]), 7) # 0.35*t_7
hist(Mix)
plot(density(Mix))

# Q4) writing my multivarNorm(n, mu, Sigma) function
    # n <- the number of samples
    # mu <- a vector of means
    # Sigma <- Covariance matrix
rmultivarNorm <- function(n, mu, Sigma){
  d <- length(mu) # The number of means which will be the number of columns
  
  # step 1) Generate n*d rnorms using runif() function
  Z <- NULL # standard normal random samples collector
  count <- 0 # counting variable
  while(count < n*d){ # Use Polar method to generate random standard normal samples.
    # Step (1a)
    u_1 <- runif(1)
    u_2 <- runif(1)
    # Step (1b)
    v_1 <- 2*u_1 - 1
    v_2 <- 2*u_2 -1
    s <- v_1^2 + v_2^2
    # Step (1c)
    if(s <= 1) {
      count <- count + 1
      Z[count] <- sqrt((-2*log(s))/(s))*v_1
      count <- count + 1
      Z[count] <- sqrt((-2*log(s))/(s))*v_2
    }
  }
  # step 2) Generating an n x d matrix Z containing nd random N(0, 1) variates
  Z <- matrix(Z, nrow = n)
  
  # step 3) Computing a square root of matrix
  evD <- eigen(Sigma, symmetric = T)
  Delta.mat <- diag(evD$values)
  P <- evD$vectors
  Q <- P %*% sqrt(Delta.mat) %*% t(P)
  J <- matrix(1, nrow = n, ncol = 1)
  X <- Z %*%Q+J%*%mu
  return(X)
}

# Q5) Writing my own Least Square Estimation Function
    # y <- our target to predict
    # x1 <- our first independent variable: numerical
    # x2 <- our second independent variable: categorical
attach(ChickWeight)
my.ls <- function(y, x1, x2){
  X <- model.matrix(y ~ x1 + x2) # Since x2 is categorical variable, in order to use regression, we need to change it into a dummy variable
  beta.est <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta.est)
}

# Compare the results
my.ls(weight, Time, Diet)
summary(lm(weight ~ Time + Diet))

# Q6) Additive model: Spline Regression
library(MASS)
library(splines)
attach(mcycle)
bic <- NULL
for (i in 1:30){
  polyfit <- lm(accel~poly(times,i,raw=TRUE))
  bic[i] <- BIC(polyfit)
}
fit1 = lm(accel~poly(times, which(bic==min(bic))[1], raw=TRUE))
summary(fit5)
# Now let use spline regression with LOOCV and GCV
fit2 <- smooth.spline(times, accel, cv = TRUE) # LOOCV
fit3 <- smooth.spline(times, accel) # GCV

# Let's see the MSEs of the three fitted models.
MSE1 <- sum(residuals(fit1)^2)/length(residuals(fit1))
MSE2 <- sum(residuals(fit2)^2)/length(residuals(fit2))
MSE3 <- sum(residuals(fit3)^2)/length(residuals(fit3))

MSE <- c(MSE1, MSE2, MSE3) # MSE1: 463.8606, MSE2: 462.713, MSE3: 466.4046
                           # We can conclude that spline regression with LOOCV has the least MSE.





