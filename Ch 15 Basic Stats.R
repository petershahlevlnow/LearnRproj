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

#15.2 correlation and covariance
#relationship of two variables to each other
#get economics data
require(ggplot2)
head(economics)

#calculate the correlation between economic savings rate and spending rate
cor(economics$pce, economics$psavert)
#verify wth correlation formula
xPart <- economics$pce - mean(economics$pce)
yPart <- economics$psavert - mean(economics$psavert)
xSD <- sd(economics$pce)
ySD <- sd(economics$psavert)
nMinusOne <- (nrow(economics) - 1)
sum(xPart * yPart) / (nMinusOne * xSD * ySD)

# compute multiple variables, use cor on matrix, correlation table
cor(economics[, c(2, 4:6)])
GGally::ggpairs(economics[, c(2, 4:6)])

# create a heat map of correlations
require(reshape2) #for melting
require(scales) #for plotting features
econCor <- cor(economics[, c(2, 4:6)])
econMelt <- melt(econCor, varnames = c("x", "y"), value.name = "Correlation")
econMelt <- econMelt[order(econMelt$Correlation),] # order by correlation
econMelt
#initialize plot with x and y 
ggplot(econMelt, aes(x = x, y = y)) +
  geom_tile(aes(fill = Correlation)) + #draw tiles filling tiles based on correlation
  scale_fill_gradient2(low = muted("red"), mid = "white", high = "steelblue", # colors red, white, blue
                       guide = guide_colorbar(ticks = FALSE, barheight = 10), limits = c(-1, 1)) + #limit -1 to 1
  theme_minimal() + labs(x = NULL, y = NULL)

#dealing with missing values in correlations
#create a 5 column matrix
m <- c(9, 9, NA, 3, NA, 5, 8, 1, 10, 4)
n <- c(2, NA, 1, 6, 6, 4, 1, 1, 6, 7)
p <- c(8, 4, 3, 9, 10, NA, 3, NA, 9, 9)
q <- c(10, 10, 7, 8, 4, 2, 8, 5, 5, 2)
r <- c(1, 9, 7, 6, 5, 6, 2, 7, 9, 10)
theMat <- cbind(m, n, p, q, r)
cor(theMat, use = "everything") #columns with NAs return a NA correlation
cor(theMat, use = "all.obs") # returns errors for missing values
rco <- cor(theMat, use = "complete.obs") #correlation only computed on rows without NA
cor(theMat, use = "na.or.complete") #correlation only computed on rows without NA
ra <- cor(theMat[c(1, 4, 7, 9, 10), ]) # rows without NAs
#check for identical matrices 
identical(ra, rco)
# pairwise complete - compares two columns at a time and keeps rows - for those two columns - where neither
# entry is NA. 
cor(theMat, use = "pairwise.complete.obs")
#compare the entries for m vs. n to this matrix
cor(theMat[, c("m", "n")], use = "complete.obs")
cor(theMat[, c("m", "p")], use = "complete.obs")

#load data on tips from the reshape package
data(tips, package = "reshape2")
head(tips)
#view data columns against each other in a plot. (Sometimes not very useful)
GGally::ggpairs(tips)

#correlation doesn't mean causation
require(RXKCD) 
getXKCD(which = "552")

# covariance
econCov <- cov(economics$pce, economics$psavert)
cov(economics[, c(2, 4:6)])
identical(econCov, cor(economics$pce,economics$psavert) * sd(economics$pce) * sd(economics$psavert))

# 15.3 t-tests
# camparing mean(s)
head(tips)
unique(tips$sex)
unique(tips$day)

#15.3.1 one-sample t-tests
t.test(tips$tip, alternative = "two.sided", mu = 2.5) # are average tips equal to $2.50, H0 = tips = $2.50
                                                      # result says that we can reject the null, tips are not equal to $2.50
#plot the result
randT <- rt(30000, df=NROW(tips) - 1) #build a t distribution
tipTTest <- t.test(tips$tip, alternative = "two.sided", mu = 2.5) #get t test information
#plot tdistribution with 3 vertical lines - 1 at -2*sd(RandT), 1 at 2*sd(RandT), 1 at t-statistic of tips t test
ggplot(data.frame(x = randT)) + geom_density(aes(x = x), fill = "grey", color = "grey") +
  geom_vline(xintercept = tipTTest$statistic) + geom_vline(xintercept = mean(randT) + c(-2,2)*sd(randT), linetype = 2)

#is the tips mean > $2.50
t.test(tips$tip, alternative = "greater", mu = 2.5)

#15.3.2 two-sample t-test
# see how male and females are tipped
# first check for variances. Are they equal or unequal?
aggregate(tip ~ sex, data = tips, var)
# next check for normally distributed
shapiro.test(tips$tip) # all sexes
shapiro.test(tips$tip[tips$sex == "Female"]) # just females
shapiro.test(tips$tip[tips$sex == "Male"]) # just Males
# check normal distributed visually
ggplot(tips, aes(x = tip, fill = sex)) + geom_histogram(binwidth = .5, alpha = 1/2)
# all checks show that the show that tips for groups are not normally distributed, so use 
# Ansari.bradley test to examine variance equality
ansari.test(tip ~ sex, tips)
# variances are in fact EQAUL, so we can use the standard two sample  t test
t.test(tip ~ sex, data = tips, var.equal = TRUE) # var.equal is false by default or the Welch test
# this test indicates that men are tipped roughly equal to females. note p-value > 0.05 not statistiaclly sig

require(plyr)
#split data according to sex then apply to summarize function.
tipSummary <- ddply(tips, "sex", summarize, tip.mean = mean(tip), tip.sd = sd(tip), 
                      Lower = tip.mean - 2*tip.sd/sqrt(NROW(tip)), Upper = tip.mean + 2*tip.sd/sqrt(NROW(tip)))
tipSummary
#visualize tipSummary, confidence ranges overlap suggesting tips are roughly equivalent between sexes
ggplot(tipSummary, aes(x = tip.mean, y = sex)) + geom_point() + geom_errorbarh(aes(xmin = Lower,
                                                                                   xmax = Upper),
                                                                               height = .2)



