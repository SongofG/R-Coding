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
plot(times, accel)
    # By plotting the data points, we can notice that
    # there are roughly four inflection points.
    # Therefore I am going to try degrees of 3, 4, and 5 to see what suits the best.
    # Since all combinations of degree 3 and 4 are nested polynomial of degrees of 5,
    # First we see the summary of each fits.
summary(lm(accel ~ poly(times, degree = 3, raw = TRUE))) # Intercept, Beta1, Beta2, Beta3, all of them are significant
summary(lm(accel ~ poly(times, degree = 4, raw = TRUE))) # Beta 2, 3, and 4 are not significant
summary(lm(accel ~ poly(times, degree = 5, raw = TRUE))) # All the estimations are significant.

    # Since degrees of 5 and 3 are good ones, we compare the two using anova test.
anova(lm(accel ~ poly(times, degree = 3, raw = TRUE)), lm(accel ~ poly(times, degree = 5, raw = TRUE)))
    # The result of this anova test gives us really small p-value.
    # That means there is difference in the model of degrees of 3 and 5
    # This means that degrees of 5 is better model!
    # Therefore linear model with the degrees of 5 is the best fit.
detach(mcycle)

# 6)
data = read.table(file = "http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/HW3Qn6Data.txt", header = TRUE)
attach(data)
indep_vars = names(data)[-1] # -1 for excluding y















