# Kim et al.'s Modified T-test
modified.t.test <- function(x, y, alternative="two.sided"){
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
  
  D_bar <- mean(x[which(!is.na(x)&!is.na(y))]-y[which(!is.na(x)&!is.na(y))])
  T_bar <- mean(x[which(!is.na(x)&is.na(y))])
  N_bar <- mean(y[which(!is.na(y)&is.na(x))])
  
  SD <- sd(x[which(!is.na(x)&!is.na(y))]-y[which(!is.na(x)&!is.na(y))])
  ST <- sd(x[which(!is.na(x)&is.na(y))])
  SN <- sd(y[which(!is.na(y)&is.na(x))])
  
  nH <- 2/(1/n2 + 1/n3)
  
  t3 <- (n1*D_bar + nH*(T_bar - N_bar))/sqrt(n1*SD^2 + nH^2*(SN^2/n3 + ST^2/n2))
  
  p <- NULL
  
  if(alternative == "greater"){
    p <- pnorm(t3, lower.tail = F)
    message <- "alternative hypothesis: true difference in means is greater than 0"
  }
  else if(alternative == "less"){
    p <- pnorm(t3)
    message <- "alternative hypothesis: true difference in means is less than 0"
  }
  else{
    p <- pnorm(abs(t3), lower.tail = F)
    message <- "alternative hypothesis: true difference in means is not equal to 0"
  }
  
  cat("       ", "Kim et al.'s Modified T-test\n\n", "p-value =", p, "\n", "alternative hypothesis:", message, "\n", "number of matched:", n1, "\n", "number of partially matched pairs:", n2+n3, "\n", "Test Statistic:", t3, "\n\n")
  
}
