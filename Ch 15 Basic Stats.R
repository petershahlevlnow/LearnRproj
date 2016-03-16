# Chapter 15 Basic Statistics

#15.1 Summary Statistics

# generate random sample n=100 from 1:100, replace means multiple numbers can be drawn
x <- sample(1:100, size = 100, replace = TRUE)
x
mean(x)
# consider missing values 1. set 20% of x to NA
y <- x 
y[sample(x = 1:100, size = 20, replace = FALSE)] <- NA # randomly set 20 elements to NA
y
mean(y)  # returns NA
mean(y, na.rm = TRUE) # disregards NAs

# weighted means
grades <- c(95, 72, 87, 66)
weights <- c(0.5, 0.25, 0.125, 0.125)
mean(grades) # equal weight of grades
weighted.mean(x = grades, w = weights) # weighted means

# variance and SD
var(x)
sum((x - mean(x))^2)/(length(x) - 1) # verify
sqrt(var(x)) # sq root of variance is SD
sd(x) # or use SD function
sd(y) # returns NA with missing values
sd(y, na.rm = TRUE) # remove missing values

#min max, also have na.rm arguments
min(x)
max(x)
median(x)
min(y)
min(y, na.rm = TRUE)

#summary statistics
summary(x)
summary(y) #no need for na.rm as the function automatically removes missing values

#quantiles
quantile(x, probs = c(.25, .75))
quantile(y, probs = c(.25, .75), na.rm = TRUE) # na.rm required when missing values exist
quantile(y, probs = c(.01, .10, .25, .50, .75, .90, .99), na.rm = TRUE)


