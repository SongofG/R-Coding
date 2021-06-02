# AMS 597 HW #7
### Q1)
set.seed(123)
x <- rnorm(50)

#bias
medians <- c()
for (i in 1:1000){
  medians[i] <- median(sample(x,length(x),replace=TRUE))
}
bias <- mean(medians) - median(x)
#standard error
se <- sd(medians)
#Bootstrap normal CI
snbCI <- c(median(x)-qnorm(0.025, lower.tail = F)*se, median(x)+qnorm(0.025,lower.tail = F)*se)
#Basic bootstrap CI
bbCI <- c(2*median(x) - quantile(medians, 0.975), 2*median(x) - quantile(medians, 0.025))
#Bootstrap t-interval
theta.hat <- median(x) # observed statistic
ts <- c()
for(i in 1:1000){
  x.b <- sample(x,length(x),replace = T)
  theta.hat.b <- median(x.b)
  medians.b <- c()
  for(j in 1:50){
    x.b.b <- sample(x.b, length(x.b), replace = T)
    medians.b[j] <- median(x.b.b)
  }
  se.hat.b <- sd(medians.b)
  ts[i] <- (theta.hat.b - theta.hat)/se.hat.b
}
bti <- c(theta.hat - quantile(ts,0.975)*se, theta.hat - quantile(ts,0.025)*se)

### Q2) Cramer-Von Mises Test
cramer.von.mises.test <- function(x,y){
  n <- length(x)
  m <- length(y)
  # ecdfs
  F1 <- sapply(sort(x),function(i){length(which(x<=i))/length(x)}) 
  G1 <- sapply(sort(x),function(i){length(which(y<=i))/length(x)})
  F2 <- sapply(sort(y),function(i){length(which(x<=i))/length(y)})
  G2 <- sapply(sort(y),function(i){length(which(y<=i))/length(y)})
  
  W2 <- ((n*m)/(n+m)^2)*(sum((F1-G1)^2)+sum(F2-G2)^2) # observed statistic
  
  B <- 10000
  W2s <- c()
  for (i in 1:B){
    z <- sample(c(x,y))
    x_per <- z[1:n]
    y_per <- z[-(1:n)]
    F1_per <- sapply(sort(x_per),function(i){length(which(x_per<=i))/n})
    G1_per <- sapply(sort(x_per),function(i){length(which(y_per<=i))/n})
    F2_per <- sapply(sort(y_per),function(i){length(which(x_per<=i))/m})
    G2_per <- sapply(sort(y_per),function(i){length(which(y_per<=i))/m})
    
    W2s[i] <- ((n*m)/(n+m)^2)*(sum((F1_per-G1_per)^2)+sum(F2_per-G2_per)^2)
  }
  
  p.val <- (1+length(which(W2s>=W2)))/(B+1)
  
  return(data.frame(obs.stat=W2, p.val=p.val, iteration=B))
}

### Q3) Spearman Rank Correlation Test
set.seed(123)
x <- rnorm(50)
y <- 0.2*x+rnorm(50)

spearman.test <- function(x,y){
  id <- which(!is.na(x)&!is.na(y))
  x <- x[id]
  y <- y[id]
  d <- rank(x) - rank(y)
  N <- length(x)
  rs <- 1 - 6*sum(d^2)/(N^3 - N)
  
  B <- 10000
  rs.perm <- c()
  for(i in 1:B){
    z <- sample(c(x,y))
    x.perm <- z[1:N]
    y.perm <- z[-c(1:N)]
    d.perm <- rank(x.perm) - rank(y.perm)
    rs.perm[i] <- 1 - 6*sum(d.perm^2)/(N^3 - N)
  }
  
  p.value <- (1 + length(which(rs.perm >= rs)))/(1 + B)
  
  return(data.frame(obs.stat=rs, p.value=p.value, iteration=B))
}

### Q4)
# a) Newton-Raphson Method
# Criteria
epsilon <- .Machine$double.eps^0.25
fx <- function(x){
  exp(2*x) - x - 6
}
fprimex <- function(x){
  2*exp(2*x) - 1
}
x <- c(0,1)
t <- 2
while(abs(x[t]-x[t-1] > epsilon & t <= 1000)){
  curx <- x[t]-fx(x[t])/fprimex(x[t])
  x <- c(x,curx)
  t <- t+1
  if(t > 1000)warning('max iteration without conversion')
}
solution1 <- x[length(x)]

# b) Using uniroot function
solution2 <- uniroot(fx, c(-4,4))$root
data.frame(Newton = solution1, Brent = solution2)

### Q5)
# a) Poisson MLE
set.seed(123)
lambda.true <- 10
x <- rpois(1000,lambda.true)

L.func <- function(lambda){
  sum(log((lambda^x)*exp(-lambda)/factorial(x)))
}

result1 <- optimize(L.func, c(0,1000), maximum = T)$maximum
result1

# b) Beta MLE
set.seed(123)
a.true <- 5
b.true <- 2
x <- rbeta(1000,shape1=a.true,shape2=b.true)

beta.L.func <- function(z){
  -sum(log(factorial(z[1] + z[2] - 1)/(factorial(z[1]-1)*factorial(z[2]-1))*(x^(z[1]-1))*((1-x)^(z[2]-1))))
}

result2 <- optim(c(1,1), beta.L.func)$par # Using optim function

library(optimx)
optimx(c(1,1), beta.L.func)


