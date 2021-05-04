# AMS 597 HW #6

# Q1)
set.seed(123)
true.val1 <- 1 - cos(pi/3)
x <- runif(1000, 0, pi/3)
esti.val <- (pi/3)*mean(sin(x))
    # True Value: 0.5
    # Estimated Value: 0.497681

# Q2)
    # Q2 a)
n <- 10000
x1 <- runif(n, 0, 0.5)
omega_hat <- 0.5*mean(exp(-x1))
var_hat <- var(0.5*exp(-x1))/n
    # Q2 b)
x2 <- rexp(n, rate=1)
omega_star <- mean(x2 >= 0 & x2 <= 0.5)
var_star <- var(x2 >= 0 & x2 <= 0.5)/n
    # Q2 c)
var_hat/var_star # Comparing the result variances. Variance of omega_hat is smaller than the omega_star.

# Q3)
    # Q3 a)
my.pbeta1 <- function(x, a, b){
  n <- 10000
  X <- runif(n, 0, x)
  Beta <- factorial(a+b-1)/(factorial(a-1)*factorial(b-1)) * X^(a-1) * (1-X)^(b-1)
  omega_hat <- x*mean(Beta) # Should multiple by (b-a) since we used runif(n, a=0, b=x)
  var_omega_hat <- sum((Beta - omega_hat)^2)/(n-1)/n
  return(list(omega_hat = omega_hat, var_omega_hat = var_omega_hat))
}

    # Q3 b)
my.pbeta2 <- function(x, a, b){
  n <- 10000
  U <- rgamma(n, a, 1)
  V <- rgamma(n, b, 1)
  Beta <- U/(U+V)
  omega_hat <- mean(Beta>=0 & Beta<=x)
  var_omega_hat <- var(Beta>=0 & Beta<=x)/n
  return(list(omega_hat = omega_hat, var_omega_hat = var_omega_hat))
}

    # Q3 c)
Xs <- seq(0.1, 0.9, 0.1)
compareFunc <- function(Xs){
  result <- list()
  for (i in 1:length(Xs)){
    result[[i]] <- c(pbeta(Xs[i], 3, 3), my.pbeta1(Xs[i], 3, 3)$omega_hat, my.pbeta2(Xs[i], 3, 3)$omega_hat)
  }
  return(result)  
}
compareFunc(Xs) # For all cases from 0.1 to 0.9, Using my.pbeta2 was more efficient than my.pbeta1

# Q4)
    # Q4 a) t-test and wilcoxon signed rank test
n <- 1000
p.vals.t = p.vals.wil<- rep(NA, n)
for (i in 1:n){
  m <- 20
  X <- rnorm(m)
  p.vals.t[i] <- t.test(X, mu=0)$p.value
  p.vals.wil[i] <- wilcox.test(X, exact = T)$p.value
}
emp.T1error.t <- sum(1 + length(which(p.vals.t <= 0.05)))/(n+1)
emp.T1error.wil <- sum(1 + length(which(p.vals.wil <= 0.05)))/(n+1)
    # Both of the emphirical Type 1 error is less than 0.05. Therefore it's a vaild test.

    # Q4 b)
p.vals.t = p.vals.wil<- rep(NA, n)
for (i in 1:n){
  m <- 20
  X <- rnorm(m, 0.5, 1)
  p.vals.t[i] <- t.test(X, mu=0)$p.value
  p.vals.wil[i] <- wilcox.test(X, exact = T)$p.value
}
emp.power.t <- 1 - sum(1 + length(which(p.vals.t > 0.05)))/(n+1)
emp.power.wil <- 1 - sum(1 + length(which(p.vals.wil > 0.05)))/(n+1)

# Q5)
    # Q5 a)
ns <- seq(10, 100, 10)
n <- 1000
emp.powers <- rep(NA, length(ns))
for(i in 1:length(ns)){
  p.vals <- rep(NA, n)
  for(j in 1:n){
    m <- ns[i]
    X <- rnorm(m)
    Y <- rnorm(m, 0.5, sqrt(1.5))
    p.vals[j] <- t.test(X,Y)$p.value
  }
  emp.powers[i] <- 1 - length(which(p.vals > 0.05))/n
}
result <- data.frame(n=ns, power=emp.powers)

    # Q5 b)
n <- (qnorm(0.025) + qnorm(0.2))^2*(1 + 1.5)/(0 - 0.5)^2 # 78.4888
    # Comparing this n with the testing above shows similar result.











