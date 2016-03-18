#ch 18 Model Diagnostics

# 18.1 Residuals
# analysis of residuals - difference between the acutal response and the fitted values, values predicted by model
# if the model is appropriately fitted its residuals will be normally distributed
#get housing data
housing <- read.table("data/housing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
head(housing)

#rename columns
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt","SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value", "ValuePerSqFt", "Boro")

#remove outliers
housing <- housing[housing$Units <1000, ]
head(housing)
# fit a linear model
house1 <- lm(housing$ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)
# visualize model
require(coefplot)
coefplot(house1)
# for linear models three important residual plots 1. fitted values vs. residuals 2. Q-Q plot 3. histogram of residuals
# ggplot2 has a fortify function that adds a new column to data for easy plotting
require(ggplot2)
head(fortify(house1))
# plot the .resid(y) and .fitted(x)
h1 <- ggplot(aes(x = .fitted, y = .resid), data = house1) + geom_point() + geom_hline(yintercept = 0) + 
      geom_smooth(se = FALSE) + labs(x = "fitted values", y = "residuals")
h1
# note that the structure of Boro makes the pattern of residuals not look as randomly dispersed as desired
h1 + geom_point(aes(color = Boro))

# plot using built in plotting fuctions 
plot(house1, which = 1)
plot(house1, which = 1, col = as.numeric(factor(house1$model$Boro)))
legend("topright", legend = levels(factor(house1$model$Boro)), pch =1, 
       col = as.numeric(factor(levels(factor(house1$model$Boro)))),
       text.col = as.numeric(factor(levels(factor(house1$model$Boro)))), title="Boro")

# Q-Q plot - if the plot is a good fit the standardized residuals should fall along a straight line when plotted
# against the theoretical quantiles of the normal distribution
plot(house1, which = 2)
ggplot(house1, aes(sample = .stdresid)) + stat_qq() + geom_abline()
# note the tails fall away from the line indicating we don't have the best fit

# antoher diagnostic histogram of the residuals 
ggplot(house1, aes(x = .resid)) + geom_histogram()
# plot shows that the residuals are not entirely normally distributed meaning that the model is not an entirely 
# correct specification

# 18.2 Comparing Models
# you only know how good your model is when comparing it to other models
# fit multiple models
house2 <- lm(housing$ValuePerSqFt ~ Units * SqFt + Boro, data = housing)
house3 <- lm(housing$ValuePerSqFt ~ Units + SqFt * Boro + Class, data = housing)
house4 <- lm(housing$ValuePerSqFt ~ Units + SqFt * Boro + SqFt * Class, data = housing)
house5 <- lm(housing$ValuePerSqFt ~ Boro + Class, data = housing)

# visualize the coefficients
multiplot(house1, house2, house3, house4, house5, pointSize = 2)
# shows that Boro and some condo classes matter

# ANOVA is useful to test multiple models, it returns the residual sum of squares (RSS). The lower the better
anova(house1, house2, house3, house4, house5)
# a problem with the RSS is that when adding a variable to the model it improves the RSS, which can lead to 
# overfitting and complexity
# The AIC penalizes for over complexity. Lower the better
AIC(house1, house2, house3, house4, house5)
# the BIC is the same, except a difference in the BIC formula
BIC(house1, house2, house3, house4, house5)

# when the anova, AIC, and BIC are called on glm the deviance is returned. For every added variable the deviance
# should decrease by 2
# try it now with a few logistic models
# create a binary variable based on whether value per square foot is greater than 150
housing$highValue <- housing$ValuePerSqFt >= 150

high1 <- glm(housing$highValue ~ Units * SqFt + Boro, data = housing, family = binomial(link = "logit"))
high2 <- glm(housing$highValue ~ Units * SqFt + Boro, data = housing, family = binomial(link = "logit"))
high3 <- glm(housing$highValue ~ Units + SqFt * Boro + Class, data = housing, family = binomial(link = "logit"))
high4 <- glm(housing$highValue ~ Units + SqFt * Boro + SqFt * Class, data = housing, family = binomial(link = "logit"))
high5 <- glm(housing$highValue ~ Boro + Class, data = housing, family = binomial(link = "logit"))

anova(high1, high2, high3, high4, high5)
AIC(high1, high2, high3, high4, high5)
BIC(high1, high2, high3, high4, high5)

# 18.3 Cross Validation
# residual and model tests like ANOVA, AIC, and BIC are a bit old fashioned. the preferred method today is
# crass validation - hold out. 
# Data are broken into k non-overlapping sections. then a model is fitted to k-1 sections and tested on the kth 
# section for prediction. This is repeated k times until all sections have been held out.

# cv.glm from boot is required
require(boot)
# can use glm for linear regression as shown below
houseG1 <- glm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing, family = gaussian(link = "identity"))
# verify that coefficients are the same
identical(coef(house1), coef(houseG1))
# run cross validation with k=5
houseCV1 <- cv.glm(housing, houseG1, K=5)
houseCV1$delta
# two numbers in the delta 1. raw cross validation error based on the cost function (MSE) for all folds 
# 2. the adjusted cross validation error - compensates for not leaving not leave one out cv - like k cross validation
# but one section has only one data point

# need to compare the numbers to other models
houseG2 <- glm(ValuePerSqFt  ~ Units * SqFt + Boro, data = housing)
houseG3 <- glm(ValuePerSqFt  ~ Units + SqFt * Boro + Class, data = housing)
houseG4 <- glm(ValuePerSqFt  ~ Units + SqFt * Boro + SqFt * Class, data = housing)
houseG5 <- glm(ValuePerSqFt  ~ Boro + Class, data = housing)

# cross validation
houseCV2 <- cv.glm(housing, houseG2, K=5)
houseCV3 <- cv.glm(housing, houseG3, K=5)
houseCV4 <- cv.glm(housing, houseG4, K=5)
houseCV5 <- cv.glm(housing, houseG5, K=5)

# combine all error values
cvResults <- as.data.frame(rbind(houseCV1$delta, houseCV2$delta, houseCV3$delta, houseCV4$delta, houseCV5$delta))
names(cvResults) <- c("error", "adjusted error")
cvResults$Model <- sprintf("houseG%s", 1:5)
cvResults

#visualize the results
#Anova
cvAnova<- anova(houseG1, houseG2, houseG3, houseG4, houseG5)
cvResults$ANOVA <- cvAnova$`Resid. Dev`
#measure AIC
cvResults$AIC <- AIC(houseG1, houseG2, houseG3, houseG4, houseG5)$AIC
# make a data.frame suitable for pltting
require(reshape2)
cvMelt <- melt(cvResults, id.vars = "Model", variable.name = "Measure", value.name = "Value")
cvMelt
ggplot(cvMelt, aes(x = Model, y = Value)) + geom_line(aes(group = Measure, color = Measure)) +
  facet_wrap(~Measure, scales = "free_y") + theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  guides(color = FALSE)
# model 4 is in fact the best of the 5 models.

# General framework for creating your own cross validation for models other than glm
# this didn't end up working so just moving on.
cv.work <- function(fun, k=5, data, cost = function(y, yhat) mean((y - yhat)^2), response= "y", ...)
{
  #generate folds
  folds <- data.frame(Fold=sample(rep(x=1:k, length.out = nrow(data))), Row=1:nrow(data))
  
  #start error at 0
  error <- 0
  
  ## loop through each of the folds
  ## for each fold
  ## fit the model on the training data
  ## predict the test data
  ## compute the erro and accumlate it
  for(f in 1:max(folds$Fold))
  {
    #rows that are in the test
    theRows <- folds$Row(folds$Fold == f)
    
    ## call fun on data[-theRows]
    mod <- fun(data=data[-theRows, ], ...)
    pred <- predict(mod, data[theRows, ])
    # add new error weighted by the number of rows in this fold
    error <- error + cost(data[theRows, response], pred) * (length(theRows)/nrow(data))
    
  }
  return(error)
}

# now apply this function to various housing models to get cv errors
cv1 <- cv.work(fun = lm, k = 5, data =housing, response = "ValuePerSqFt", 
               formula = ValuePerSqFt ~ Units + SqFt + Boro)
cv2 <- cv.work(fun=lm, k = 5, data =housing, response = "ValuePerSqFt",
               formula = ValuePerSqFt ~ Units * SqFt + Boro)
cv3 <- cv.work(fun=lm, k = 5, data =housing, response = "ValuePerSqFt",
               formula = ValuePerSqFt ~ Units + SqFt * Boro + Class)
cv4 <- cv.work(fun=lm, k = 5, data =housing, response = "ValuePerSqFt",
               formula = ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class)
cv5 <- cv.work(fun=lm, k = 5, data =housing, response = "ValuePerSqFt",
               formula = ValuePerSqFt ~ Boro + Class)

cvResults <- data.frame(Model = sprintf("house%s", 1:5), Error = c(cv1, cv2, cv3, cv4, cv5))
cvResults

#18.4 Bootstrapping
# sometimes there isn't a good analytical solution to a problem. Especially true for measuring uncertainty around
# confidence intervals
# statistic is applied to dataset of n rows. then data is sampled to create a data set with n rows but with repeats
# then the statistic is applied again to that dataset. This process is repeated R times (ex 1200) to create a distribution
# for the statistic
# simple example
require(plyr)

baseball <- baseball[baseball$year >= 1990, ]
head(baseball)
# batting avg and sd. SD is difficult to calculate so we use bootstraping
# batting avg = sum(h)/sum(atbats)

## use boot 
##build a function for calculating batting average
# data is the data
# boot will pass varying sets of indices
# some rows will be represented multiple times
# otehr rows will not be represented at all
# on average about 63% of the rows will be present
# this function is called repeatedly boot
bat.avg <- function(data, indices = 1: NROW(data), hits = "h", at.bats = "ab")
{
  sum(data[indices, hits], na.rm = TRUE)/sum(data[indices, at.bats], na.rm = TRUE)
}

# test the func on original data
bat.avg(baseball)
# now bootstrap it
# call it 1200 times
# pass indices to function
avgBoot <- boot(data = baseball, statistic = bat.avg, R = 1200, stype = "i")
avgBoot
#print the confidence interval
boot.ci(avgBoot, conf = 0.95, type = "norm")
# plot the distribution of replicate results
ggplot() + geom_histogram(aes(x = avgBoot$t), fill = "grey", color = "grey") + 
  geom_vline(xintercept = avgBoot$t0 + c(-1, 1)*2*sqrt(var(avgBoot$t)), linetype = 2)

#18.5 Stepwise Variable Selection
# adding and subtracting variable into a model until an optimal has been found

#define the lowest model
nullModel <- lm(ValuePerSqFt ~ 1, data = housing)
# the largest model
fullModel <- lm(ValuePerSqFt ~ Units + SqFt*Boro + Boro*Class, data = housing)

#try different models using the step function, specify direction to work, in this case both
houseStep <- step(nullModel, scope = list(lower = nullModel, upper = fullModel), direction = "both")
# reveal the chosen model
houseStep
