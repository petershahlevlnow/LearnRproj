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






