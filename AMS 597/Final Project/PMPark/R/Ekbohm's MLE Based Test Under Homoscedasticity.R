# Ekbohm's MLE Based Test Under Homoscedasticity
ekbohm.test <- function(x, y, alternative="two.sided"){
  if(is.null(x) | is.null(y)){
    stop("Both of the input vectors should not be NULL")
  }
  else if(length(x) != length(y)){
    stop("Two input vectors should have the same length")
  }
  else if(is.double(x) == F | is.double(y) == F){
    stop("Only double vectors are accepted")
  }
  else if(length(x)==length(which(is.na(x))) | length(y)==length(which(is.na(y)))){
    stop("The vector is consisted of all NAs.")
  }
  else if(length(x)==1 | length(y)==1){
    stop("Test cannot be conducted due to the small size of sample")
  }
  
  n1 <- length(which(!is.na(x)&!is.na(y))) # The number of complete matched pairs
  n2 <- length(which(!is.na(x)&is.na(y))) # The number of samples that are not NA in x but NA in y
  n3 <- length(which(is.na(x)&!is.na(y))) # The number of samples that are NA in x but not in y
  
  if(n1 == 0 | n2 ==0 | n3 == 0){
    stop("Given vectors do not meet the testing condition: either n1, n2, or n3 is 0.")
  }
  
  STN1 <- cov(x[which(!is.na(x)&!is.na(y))], y[which(!is.na(x)&!is.na(y))])
  ST1 <- sd(x[which(!is.na(x)&!is.na(y))])
  SN1 <- sd(y[which(!is.na(x)&!is.na(y))])
  ST <- sd(x[which(!is.na(x))])
  SN <- sd(y[which(!is.na(y))])
  r <- STN1/(ST1*SN1)
  f_star <- n1*(n1+n3+n2*r)/((n1+n2)*(n1+n3) - n2*n3*r^2)
  g_star <- n1*(n1+n2+n3*r)/((n1+n2)*(n1+n3) - n2*n3*r^2)
  Var_hat <- (ST1^2*(n1-1) + SN1^2*(n1-1) + (1+r^2)*(ST^2*(n2-1) + SN^2*(n3-1)))/(2*(n1-1) + (1+r^2)*(n2+n3-2))
  V_1_star <- Var_hat*((2*n1*(1-r) + (n2+n3)*(1-r^2))/((n1+n2)*(n1+n3) - n2*n3*r^2))
  
  Z_E <- (f_star*(mean(x[which(!is.na(x)&!is.na(y))]) - mean(x[which(!is.na(x))]))
          - g_star*(mean(y[which(!is.na(x)&!is.na(y))]) - mean(y[which(!is.na(y))]))
          + mean(x[which(!is.na(x))])
          - mean(y[which(!is.na(y))]))/sqrt(V_1_star)
  
  if(alternative == "greater"){
    p <- pt(Z_E, n1, lower.tail = F)
    message <- "alternative hypothesis: true difference in means is greater than 0"
  }
  else if(alternative == "less"){
    p <- pt(Z_E, n1)
    message <- "alternative hypothesis: true difference in means is less than 0"
  }
  else{
    p <- 2*pt(abs(Z_E), n1, lower.tail = F)
    message <- "alternative hypothesis: true difference in means is not equal to 0"
  }
  
  cat("       ", "Ekbohm's MLE Based Test Under Homoscedasticity\n\n", "p-value =", p, "\n", message, "\n", "number of matched:", n1, "\n", "number of partially matched pairs:", n2+n3, "\n", "Test Statistic:", Z_E, "\n\n")
  if(var.test(x,y)$p.value > 0.05){
    warning("F-test of the two vectors present a p-value greater than 0.05. This test is under unequal variance assumption.")
  }
  
}
