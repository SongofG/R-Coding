# AMS 597 HW #3

# Q1)
data = read.table(file = "http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header = TRUE)
    # a)
fit = lm(data$Pfizer ~ data$Exxon + data$Citigroup)
summary(fit)

    # b)
layout(matrix(1:2))
plot(data$Pfizer ~ data$Exxon)
pp1 = predict(lm(data$Pfizer ~ data$Exxon), interval = "confidence")
matlines(sort(data$Exxon), pp[order(data$Exxon), ], lty = c(1,2,2), col = c("black", "red", "red"))
plot(data$Pfizer ~ data$Citigroup)
pp1 = predict(lm(data$Pfizer ~ data$Citigroup), interval = "confidence")
matlines(sort(data$Citigroup), pp[order(data$Citigroup), ], lty = c(1,2,2), col = c("black", "red", "red"))

    # c)
anova(fit)

    # d)
fit2 = lm(data$Pfizer ~ -1 + data$Exxon + data$Citigroup)
summary(fit2)

    #e)
cor.test(data$Pfizer, data$Exxon)

# Q2)
    # a) Writing my.oneway.anova

g1 = c(data$Citigroup, data$AmerExp) # Citigroup and American Express
g2 = c(data$Exxon, data$GenMotor) # Exxon and General Motors
g3 = c(data$Intel) # Intel
my.oneway.anova <- function(lst){ # Data in the form of list
  k = length(lst) # number of groups
  n = length(unlist(lst)) # number of all observations
  grand_mean = mean(unlist(lst)) # grand mean of all observations
  
  SSW = 0 # Sum of Squares Within the Groups
  for(i in 1:k){
    SSW = SSW + sum((lst[[i]]-mean(lst[[i]]))^2)
  }
  
  SSB = 0 # Sum of Squares Between the Groups
  for(i in 1:k){
    SSB = SSB + length(lst[[i]])*sum((mean(lst[[i]]) - grand_mean)^2)
  }
  
  MSW = SSW/(n-k) # Mean Square Error of Within Groups
  MSB = SSB/(k-1) # Mean Square Error of Between Groups
  f = MSB/MSW
  p.val = 1 - pf(f, k-1, n-k)
  result = list(MSB = MSB, MSW = MSW, F = f, P_value = p.val)
  return(result)
}

    # b) Comparing my function and the built in function using Group 1 and 2
test1_data <- data.frame(Observations = c(g1, g2), Groups = c(rep("Group 1", length(g1)), rep("Group 2", length(g2))))
my.oneway.anova(list(g1, g2))
summary(aov(lm(Observations ~ Groups, data = test1_data)))

    # c) Comparing my function and the built in function using Group 1-3
test2_data <- data.frame(Observations = c(g1, g2, g3), Groups = c(rep("Group 1", length(g1)), rep("Group 2", length(g2)), rep("Group 3", length(g3))))
my.oneway.anova(list(g1, g2, g3))
summary(aov(lm(Observations ~ Groups, data = test2_data)))

# Q3)

    # a) Performing two-way ANOVA.
anova(lm(weight ~ Time + Diet, data = ChickWeight))

    # b) Performing one-way ANOVA of weight and Diet of the subset of Time 2.

time_2 = ChickWeight[which(ChickWeight$Time == 2),] # Filtering the dataset
attach(time_2)
fit = aov(weight ~ Diet) # Since the p-value is significantly small, the null hypothesis is rejected.
                                        # Therefore, we need to perform the Post-Hoc Pairwise Comparison to find out
                                        # which diet group is actually different from others.
TukeyHSD(fit)
detach(time_2)
# After testing, we could realize that Diet 1 is significantly different from Diet 4 and 3.
# Diet 4 and 3 could be the same
# Diet 3 and 2 could be the same
# Diet 2 gives quite high p-value when compared to Diet 1 and 4

# Q4)
attach(data)
    # a) Proportion test of Pfizer, p_0 = 0.55
x_1 = length(data[data[,2] > 0, 2]) # Filtering the data. Getting the Pfizer column when it has positive number
N1 = length(data[,2]) # The number of all observations of Pfizer stock
p0_1 = 0.55 # Null proportion.
prop.test(x_1, N1, p0_1) # Since N1*(x1/N1)*(1 - x1/N1) > 10, It's ok to use normal approximation

    # b) Testing if the proportion of positive return of Intel is larger
x_2 = length(data[data[ ,3] > 0, 3]) # Filtering to obtain positive Intel returns only.
N2 = length(data[, 3]) # Number of all observations of the Intel Return
p0_2 = 0.55
prop.test(x_2, N2, p0_2, alternative = "greater") # Since N2*(x2/N2)*(1 - x2/N2) > 10, it is ok to use normal approximation

    # c) Testing if the proportion of positive returns of Pfizer and Intel are same.
positives = c(x_1, x_2)
Ns = c(N1, N2)
prop.test(positives, Ns)

    # d)
g1 = c(Citigroup, AmerExp)
g2 = c(Exxon, GenMotor)
g3 = Intel
groups = list(g1 = g1, g2 = g2, g3 = g3)
my.chisq.test.helper <- function(lst){
  collector = c()
  for(i in 1:length(lst)){
    result = c()
    result[1] = length(lst[[i]][lst[[i]] < -0.1]) # r < -0.1
    result[2] = length(lst[[i]][lst[[i]] >= -0.1 & lst[[i]] < 0]) # -0.1 <= r < 0
    result[3] = length(lst[[i]][lst[[i]] >= 0 & lst[[i]] < 0.1]) # 0 <= r < 0.1
    result[4] = length(lst[[i]][lst[[i]] >= 0.1]) # r >= 0.1
    collector = c(collector, result)
  }
  return(collector)
}
vector_for_matrix = my.chisq.test.helper(groups)
rctable = matrix(vector_for_matrix, nrow = 3, byrow = T)
colnames(rctable) <- c("|r < -0.1|", "|-0.1 <= r < 0|", "|0 <= r < 0.1|", "|r >= 0.1|")
rownames(rctable) <- c("g1", "g2", "g3")
chisq.test(rctable)
detach(data)

# Q5) Best Fitting Polynomial Regression Model.
library(MASS)
attach(mcycle)
bic <- NULL
for (i in 1:30){
  polyfit <- lm(accel~poly(times,i,raw=T))
  bic[i] <- BIC(polyfit)
}
fit5=lm(accel~poly(times,which(bic==min(bic))[1],raw=T))
summary(fit5)
fit5a = step(fit5,k=log(133))
#Selected the fitted model with has the smallest value of Bayesian information criterion

par(mfrow=c(2,2))
plot(fit5)
# From the top-left plot, the mean of residuals of the selected model is zero. As the fitted values along x increase, the residuals are approximately flat, so the disturbances are homoscedastic. The plot on the bottom-left also shows the homoscedasticity of residuals. The normal QQ plot in top-right indicates that the residuals are normally distributed. The residuals vs leverage plot in the bottom-right shows the spread is almost uniform and no point has excess leverage.


# Q6)
####Method 1
mydat <-read.delim('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/Qn1Data.txt',header=T)
BIC.all <- NULL
X <- mydat[,-1]

BIC.all <- BIC(glm(mydat$y~1,family='binomial'))
names(BIC.all) <- c('Intercept')
for(i in 1:6){
  tmp <- combn(6,i)
  for(j in 1:dim(tmp)[2]){
    subX <- X[,tmp[,j]]
    subdat <- data.frame(cbind(mydat$y,subX))
    colnames(subdat)[1] <- 'y'
    BIC.all <- c(BIC.all,BIC(glm(y~.,data=subdat,family='binomial')))
    names(BIC.all)[length(BIC.all)] <- paste('x',tmp[,j],collapse='',sep='')
  }
}
BIC.all[which(BIC.all==min(BIC.all))]
#> BIC.all[which(BIC.all==min(BIC.all))]
#      x3 
#266.9233
# y~x3 is the best model using BIC
