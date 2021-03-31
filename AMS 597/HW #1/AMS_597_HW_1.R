# Question 1)
# a)
weight <- c(60, 72, 34, 56, 87, 80, 89, 95, 76, 28, 48, 59)

# b)
mean(weight) # 65.33333
mean(weight^2) # 4694.667

# c)
length(weight) # length = 12

# d)
length(weight[weight > 55]) # 9

# e)
tf_vec = weight > 55 & weight < 85 # Change variables into True and False
replace(tf_vec, tf_vec==TRUE, "Yes") # Change TRUE to Yes
replace(tf_vec, tf_vec==FALSE, "No") # Change FALSE to No
tf_vec

#Question 2)
tmp <- matrix(rnorm(12), 3, 4)

# a)
ans1 = tmp[,1] + tmp[,3]
ans1

# b)
ans2 = tmp[1,] * tmp[3,]
ans2

# c)
dim(tmp)

# d)
cat(tmp[1,][tmp[1,] > 0.5])

# Question 3)
same.vec <- function(a, b){
  if(is.na(all(a==b))){
    a[is.na(a)] = 0
    b[is.na(b)] = 0
    return(all(a==b))
  }
  else{
    return(all(a==b))
  }
}

# Question 4)
    # Answer
    # y[x] will return a vector with elements from y with an order
    # assigned by the levels of x. The lowest level will be the first index of y
    # and the highest level will be the last index of y.

# Question 5)
myMedian.cal <- function(vec){
  vec = sort(vec)
  if(length(vec)%%2 == 0){
    return((vec[length(vec)/2]+vec[length(vec)/2 + 1])/2)
  }
  else{
    return(vec[ceiling(length(vec)/2)])
  }
}

# Question 6)
mydna <- paste(sample(c('a','t','c','g'),1000,replace=T),collapse='')

myXYer <- function(string){
  substr = strsplit(string, "cg", perl=TRUE) # To count the number of "cg"s.
  print(paste0("we have ", length(substr[[1]])-1, " cg.")) # Printing the outputs.
  print(gsub("cg", "XY", string)) # Printing the manipulated string.
}

# mydna
# myXYer(mydna)

# Question 7)
data = read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/PhoneNumber.txt", sep = "\n")
pattern = "\\(*\\d{3}\\)*[- \\.]+\\d{3}[- \\.]+\\d{4}"
data.frame(Numbers = unlist(regmatches(unlist(data, use.names = FALSE), gregexpr(pattern, unlist(data, use.names = FALSE)))))
