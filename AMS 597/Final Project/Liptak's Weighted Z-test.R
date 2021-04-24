# Liptak's Weighted Z-test
weighted.z.test <- function(x, y, alternative=c("two.sided", "greater", "equal"), mu=0, conf.level=0.95){
  if(is.null(x) | is.null(y)){
    stop("Both of the input vectors should not be NULL")
  }
  else if(length(x) != length(y)){
    stop("Two input vectors should have the same length")
  }
  else if(is.double(x) == F | is.double(y) == F){
    stop("Only double vectors are accepted")
  }
  
  n1 <- length(which(!is.na(x)&!is.na(y))) # The number of complete matched pairs
  n2 <- length(which(!is.na(x)&is.na(y))) # The number of samples that are not NA in x but NA in y
  n3 <- length(which(is.na(x)&!is.na(y))) # The number of samples that are NA in x but not in y
  w1 <- sqrt(n1) # weight of the complete matched pairs
  w2 <- sqrt(n2+n3) # weight of the partially matched pairs
  p <- NULL
  Z1 <- NULL
  Z2 <- NULL
  message <- NULL
  if (alternative == "greater"){
    p1 <- t.test(x[which(!is.na(x)&!is.na(y))], y[which(!is.na(x)&!is.na(y))], alternative = "greater", paired = T, conf.level = conf.level)$p.value
    p2 <- t.test(x[which(!is.na(x)&is.na(y))], y[which(is.na(x)&!is.na(y))], alternative = "greater", paired = F, conf.level = conf.level)$p.value
    Z1 <- qnorm(1-p1)
    Z2 <- qnorm(1-p2)
    p <- 1 - pnorm((w1*Z1 + w2*Z2)/sqrt(w1^2 + w2^2))
    message <- "alternative hypothesis: true difference in means is greater than"
  }
  else if (alternative == "less"){
    p1 <- t.test(x[which(!is.na(x)&!is.na(y))], y[which(!is.na(x)&!is.na(y))], alternative = "less", paired = T, conf.level = conf.level)$p.value
    p2 <- t.test(x[which(!is.na(x)&is.na(y))], y[which(is.na(x)&!is.na(y))], alternative = "less", paired = F, conf.level = conf.level)$p.value
    Z1 <- qnorm(1-p1)
    Z2 <- qnorm(1-p2)
    p <- 1 - pnorm((w1*Z1 + w2*Z2)/sqrt(w1^2 + w2^2))
    message <- "alternative hypothesis: true difference in means is less than"
  }
  else{
    p1 <- t.test(x[which(!is.na(x)&!is.na(y))], y[which(!is.na(x)&!is.na(y))], alternative = "greater", paired = T, conf.level = conf.level)$p.value
    p2 <- t.test(x[which(!is.na(x)&is.na(y))], y[which(is.na(x)&!is.na(y))], alternative = "greater", paired = F, conf.level = conf.level)$p.value
    Z1 <- qnorm(1-p1)
    Z2 <- qnorm(1-p2)
    p <- 1 - pnorm((w1*Z1 + w2*Z2)/sqrt(w1^2 + w2^2))
    if (p < 0.5){
      p <- 2*p
    }
    else{
      p <- 2*(1-p)
    }
    message <- "alternative hypothesis: true difference in means is not equal to"
  }
  
  cat("       ", "Liptak's Weighted Z-test\n", "p-value =", p, "\n", "alternative hypothesis:", message, mu, "\n", "number of matched:", n1, "\n", "number of partially matched pairs:", n2+n3, "\n", "weight of matched pairs:", w1, "\n", "weight of partially matched pairs", w2, "\n", "Z score of matched pairs:", Z1, "\n", "Z score of partially matched pairs:", Z2, "\n")
  
  return(list(p.value=p, weight1=w1, weight2=w2, Z.score1=Z1, Z.score2=Z2))
}
