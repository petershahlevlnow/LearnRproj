#ch 20 non-linear models

# 20.1 nonlinear least squares
# model for where a wifi hotspot could be based on device data and non-linear model
#load wifi data, wifi data where x, y dimensional location of device known and distance to hotspot, subj. to noise/fluctuations
download.file(url = "http://www.jaredlander.com/data/wifi.rdata", destfile = "data/wifi.rdata", quiet = TRUE)
load("data/wifi.rdata")
head(wifi)

#plot x y
ggplot(wifi, aes(x=x, y=y, color=Distance)) + geom_point() + 
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint=mean(wifi$Distance))

# we know the non-linear formula for distance. use nls 
# specify the square root model
# starting values at the center of grid

wifiMod <- nls(Distance ~ sqrt((betaX - x)^2 + (betaY - y)^2), data = wifi, start = list(betaX = 50, betaY = 50)) 
summary(wifiMod)

# estimates for betaX and Y are 17.8 and 52.9
ggplot(wifi, aes(x=x, y=y, color=Distance)) + geom_point() + 
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint=mean(wifi$Distance)) +
  geom_point(data = as.data.frame(t(coef(wifiMod))), aes(x = betaX, y = betaY), size = 5, color = "green")

#get datasets
creditNames <- c("Checking", "Duration", "CreditHistory", "Purpose", "CreditAmount", "Savings",
                 "Employment", "InstallmentRate", "GenderMarital", "OtherDebtors", "YearsAtResidence",
                 "RealEstate", "Age", "OtherInstallments", "Housing", "ExistingCredits", "Job", "NumLiable",
                 "Phone", "Foriegn", "Credit")
theURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
credit <- read.table(theURL, sep = " ", header = FALSE, col.names = creditNames, stringsAsFactors = FALSE)
head(credit)
require(useful)
require(mgcv)
require(rpart)
require(randomForest)
# 20.2 Splines 

# smooth.spline takes in degree of freedom greater than one
# get data
data("diamonds")
#fit a few different dfs, fewer dfs lead to a straighter fit
diaSpline1 <- smooth.spline(x=diamonds$carat, y=diamonds$price) 
diaSpline2 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df = 2) 
diaSpline5 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df = 5)
diaSpline10 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df = 10)
diaSpline20 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df = 20)
diaSpline50 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df = 50)
diaSpline100 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df = 100)
# to plot these, extract the information from the object, build a data.frame, then add a new layer on
# the standard scatterplot of the diamonds data

get.spline.info <- function(object)
{
  data.frame(x = object$x, y = object$y, df = object$df)
}

require(plyr)

# combine the results into one data.frame
splineDF <- ldply(list(diaSpline1, diaSpline2, diaSpline5, diaSpline10, diaSpline20, diaSpline50, diaSpline100),
                  get.spline.info)
head(splineDF)

g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
g + geom_line(data = splineDF, aes(x=x, y=y, color=factor(round(df, 0)), group=df)) + 
                scale_color_discrete("Degrees of\n Freedom")

#making predictions on new data is done with predict
# basis spline creates new predictions based on transformations of original predictors
# cubic spline is generally the best because it crates smoth transitions at interior breakpoints
# and forces linear behavior beyond endpoints
# cubic splines - ns function
require(splines)
head(ns(diamonds$carat, df = 1))
head(ns(diamonds$carat, df = 2))
head(ns(diamonds$carat, df = 3))
head(ns(diamonds$carat, df = 4))
#new predictors can be used in any model just like other predictors more knots means a more intepolating fit
# can plot these ggplot2 

g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
g + stat_smooth(method = "lm", formula = y ~ ns(x, 6), color = "blue") +
  stat_smooth(method = "lm", formula = y ~ ns(x, 3), color = "red")

#20.3 General Additive Models
# another method for fitting non linear models is GAM which fit seperate smoothing function on each predictor
# independently
# uses mcgv similar to glm
# get data: (note data has no header and formated to save space, a )
creditNames <- c("Checking", "Duration", "CreditHistory", "Purpose", "CreditAmount", "Savings",
                 "Employment", "InstallmentRate", "GenderMarital", "OtherDebtors", "YearsAtResidence",
                 "RealEstate", "Age", "OtherInstallments", "Housing", "ExistingCredits", "Job", "NumLiable",
                 "Phone", "Foriegn", "Credit")
theURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
credit <- read.table(theURL, sep = " ", header = FALSE, col.names = creditNames, stringsAsFactors = FALSE)
head(credit)

# now we need to decode variables only doing it on things we care about. This can be painful
# before
head(credit[, c("CreditHistory", "Purpose", "Employment", "Credit")])
creditHistory <- c(A30 = "All Paid", A31 = "All Paid this Bank", A32 = "Up to Date", A33 = "Late Payment",
                   A34 = "Critical Amount")

purpose <- c(A40 = "car (new)", A41 = "car (used)", A42 = "furniture/equip", A43 = "radio/TV", 
             A44 = "domestic appliances", A45 = "repairs", A46 = "education", A47 = "Vacation (doesn't exist",
             A48 = "retraining", A49 = "Business", A410 = "others")

employment <- c(A71 = "unemployed", A72 = "< l year", A73 = "1 - 4 years", A74 = "4 -7 years",
                A75 = ">= 7 years")

credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]
#coode good/bad credit
credit$Credit <- ifelse(credit$Credit == 1, "Good", "Bad")
# make good the base levels
credit$Credit <- factor(credit$Credit, levels = c("Good", "Bad"))
# after
head(credit[, c("CreditHistory", "Purpose", "Employment", "Credit")])

# viewing data in good vs. bad shows non-linear relationship may be appropritate

require(useful)
ggplot(credit, aes(x = CreditAmount, y = Credit)) + geom_jitter(position = position_jitter(height = .2)) +
  facet_grid(CreditHistory ~ Employment) + xlab("Credit Amount") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_x_continuous(labels = multiple)

#color code previous plot by good bad credit and y as age
ggplot(credit, aes(x = CreditAmount, y = Age)) + geom_point(aes(color=Credit)) +
  facet_grid(CreditHistory ~ Employment) + xlab("Credit Amount") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_x_continuous(labels = multiple)

# using the gam is similar to using lm and glm, in that it takes a formula.
# differnence is continuous variables such as Credit Amount and Age anc transformed using non-parametric smooting with 
# spline and tensor product - a way of representing transformation functions of predictors, possibly measured
# on different units
require(mgcv)
# fit a logistic GAM
# apply tensor product on Credit Amount and spline on age 
creditGAM <- gam(Credit ~ te(CreditAmount) + s(Age) + CreditHistory + Employment, data = credit, 
                 family = binomial(link = "logit"))
summary(creditGAM)
# the smoother is fitted automatically in the fitting process and can be viewed after the fact.
# Credit Amount (te) and Age(s). gray shaded regions are the two point standard deviation
plot(creditGAM, select = 1, se = TRUE, shade = TRUE)
plot(creditGAM, select = 2, se = TRUE, shade = TRUE)


# 20.4 Decision Trees
# a relative modern technique for fitting non-linear models is by decision trees
# works on both regression and classification by perfoming binary splits on recursive predictors
require(rpart)
creditTree <- rpart(Credit ~ CreditAmount + Age + CreditHistory + Employment, data = credit)
creditTree
# reading a tree in printed format - each line is a node, first node is the root where 1000 observations are 
# good and 300 are "Bad". Next node is split on creditHistory. This can be tedious to read so we need to plot
require(rpart.plot)
rpart.plot(creditTree, extra = 4)
# nodes split to the left meet the criteria while those on the right do not. Each node is labled by 
# the predicted class, either "good" or "bad". The percentage is read from left to right, with the probabilty
# of being "good" on the left

# while trees are easy to interpret and fit data nicely, they tend to be unstable with high variance due to 
# overfitting. A slight change in the training data can cause a significant difference in the model.
