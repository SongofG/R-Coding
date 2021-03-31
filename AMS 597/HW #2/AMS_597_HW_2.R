# Question 1_a) Multinomial Distribution using "sample()" function
my.multi.sam <- function(x, n){
  if(length(x) != 4){ # Error Buffer!
    print("Error!")
    return()
  }
  return(sample(x, n, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.3)))
}
    # prob for A = 0.1
    # prob for B = 0.2
    # prob for C = 0.4
    # prob for D = 0.3

# Question 1_b) Multinomial Distribution using "runif()" function
my.multi.unif <- function(x, n){
  if (length(x) != 4){
    print("Error!")
    return()
  }
  result = runif(n)
  result[result<=0.1] = x[1] # Probability of 0.1
  result[result>0.1 & result <= 0.3] = x[2] # Probability of 0.2
  result[result>0.3 & result <= 0.7] = x[3] # Probability of 0.4
  result[result>0.7 & result <= 1] = x[4] # Probability of 0.3
  return(result)
}

# Question 2) 100 random exponentially distributed samples
x = rexp(100, 2)
n = length(x)
# Plotting the emirical distribtution function
plot(sort(x), (1:n)/n, xlab = "Observations", ylab ="Probability", type = "s", ylim = c(0,1), )

# Question 3)
data = read.table(file = "http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header = TRUE)

    # a)
t.test(data$AmerExp, mu = 0)

    # b)
wilcox.test(data$AmerExp)

    # c)
t.test(data$Pfizer, data$AmerExp, mu = 0)

    # d)
var(data$Pfizer) # 0.0005327471
var(data$AmerExp) # 0.000900819

    #e)
wilcox.test(data$Pfizer, data$AmerExp)


# Question 4)

printer1 <- function(t, df, p_val){ # A helper function
  print(paste0("T Statistics: ", t))
  print(paste0("Degrees of Freedom: ", df))
  print(paste0("P-value: ", p_val))
}

my.t.test <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0){
  if (is.null(y)){                              # One Sample Test
    xbar = mean(x) # Sample mean
    sd = sd(x) # Standard Error
    n = length(x) # Size of the sample
    df = n - 1 # Degrees of Freedom
    t = (xbar - mu)/(sd/sqrt(n))
    if (alternative == "two.sided"){ # two sided test t test
      p_val = 2*pt(-abs(t), df)
      printer1(t, df, p_val)
    }
    else if (alternative == "less"){ # one sided t-test: less
      p_val = pt(abs(t), df)
      printer1(t, df, p_val)
    }
    else{ # one sided t-test: greater
      p_val = 1 - pt(abs(t), df)
      printer1(t, df, p_val)
    }
  }
  else{ # two sample t-test: Assuming the two samples are independent
    xbar = mean(x) # Mean of x
    ybar = mean(y) # Mean of y
    s_x = sd(x) # S.D of x
    s_y = sd(y) # S.D of y
    n_x = length(x) # Size of x
    n_y = length(y) # Size of y
    if(var.test(x, y)$p.value <= 0.05){ # Assumption: Two Variance Not Equal
      t = ((xbar - ybar) - mu) / (sqrt((s_x^2)/n_x + (s_y^2)/n_y))
      df = ((s_x^2)/n_x + (s_y^2)/n_y)^2 / ((s_x^2/n_x)^2/(n_x-1) + (s_y^2/n_y)^2/(n_y-1))
      if(alternative == "two.sided"){ # Two Sided Test
        p_val = 2*pt(-abs(t), df)
        printer1(t, df, p_val)
      }
      else if(alternative == "less"){ # One Sided Test: Less
        p_val = pt(abs(t), df)
        printer1(t, df, p_val)
      }
      else{ # One Sided Test: Greater
        p_val = 1 - pt(abs(t), df)
        printer1(t, df, p_val)
      }
    }
    else{ # Assumption: Two Variance Equal
      df = n_x+n_y-2
      s_p = sqrt(((n_x-1)*(s_x^2) + (n_y-1)*(s_y^2))/(df)) # Pooled Standard Deviation
      t = ((xbar-ybar) - mu)/(s_p*sqrt((1/n_x) + (1/n_y))) # Test Statistics for Equal Variance Assumption T-Test.
      if(alternative == "two.sided"){ # Two Sided Test
        p_val = 2*pt(-abs(t), df)
        printer1(t, df, p_val)
      }
      else if(alternative == "less"){ # One Sided Test: Less
        p_val = pt(abs(t), df)
        printer1(t, df, p_val)
      }
      else{ # One Sided Test: Greater
        p_val = 1 - pt(abs(t), df)
        printer1(t, df, p_val)
      }
    }
  }
}

# Question 5)
# One of the two samples, we choose the one with smaller sample size

printer2 <- function(method, w1, w2, p){ # A helper function
  print(paste0("Method: ", method))
  print(paste0("W1: ", w1))
  print(paste0("W2: ", w2))
  print(paste0("P-value: ", p))
}

my.wilcox.test <- function(x, y){
  n1 = length(x)
  n2 = length(y)
  n = n1 + n2
  w1 = sum(rank(c(x, y))[1 : n1]) # Test statistics of x
  w2 = sum(rank(c(x, y))[(n1 + 1):(n1+n2)]) # Test statistics of y
  if(n1 >= 12 & n2 >= 12){ # Normal Approximation P-Value
    exp.x = n1*(n+1)/2
    var = n1*n2*(n+1)/12
    z1 = (w1 - exp.x)/sqrt(var)
    p = 2 * pnorm(-abs(z1))
    printer2("Normal Approximation", w1, w2, p)
  }
  else{ # Exact P-Value
    dist = c() # Ranked Sum Distribution
    ranking.combn.p1 = combn(rank(c(x, y)), n1) # Collection of combinations of ranked sum of x
    for (i in 1:choose(n, n1)){ # Since the number of combinations are nCn_1.
      dist[i] = sum(ranking.combn.p1[,i])
    }
    p1 = length(dist[dist<=w1])/length(dist)
    
    dist = c()
    ranking.combn.p2 = combn(rank(c(x, y)), n2) # Collection of combinations of ranked sum of y
    for(i in 1:choose(n, n2)){
      dist[i] = sum(ranking.combn.p2[,i])
    }
    p2 = length(dist[dist<=w2])/length(dist)
    
    p = 2*min(p1, p2)
    
    printer2("Exact", w1, w2, p)
  }
}


