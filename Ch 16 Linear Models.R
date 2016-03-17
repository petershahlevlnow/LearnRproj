#Ch 16 Linear Models

#16.1 Simple Linear Regression

require(UsingR)
require(ggplot2)
head(father.son)
#plot fathers height to sons height. son's height is the response and father's height is the predictor
ggplot(father.son, aes(x = fheight, y = sheight)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Fathers", y = "Sons")
# lm outputs the intercept and coefficents where sheight is the response and fheight is the predictor
heightsLM <- lm(sheight ~ fheight, data = father.son)
heightsLM
# summary prints all desired information about the LM
summary(heightsLM)

#16.1.1 ANOVA Alternative
data(tips, package = "reshape2")
head(tips)
# adding a -1 indicates to leave out the intercept for the analysis/model
tipsAnova <- aov(tip ~ day -1, data = tips)
tipsLM <- lm(tip ~ day - 1, data =  tips)
summary(tipsAnova)
summary(tipsLM) # F statistic, df and p value are the same as ANOVA

# calculate means and CI manually
require(plyr)
tipsByDay <- ddply(tips, "day", plyr::summarize, tip.mean= mean(tip), tip.sd = sd(tip), Length = NROW(tip),
                   tfrac = qt(p=0.90, df = Length -1), Lower = tip.mean - tfrac*tip.sd/sqrt(Length),
                   Upper = tip.mean + tfrac*tip.sd/sqrt(Length))
# extract from the summary of tipsLM
tipsInfo <- summary(tipsLM)
tipsCoef <- as.data.frame(tipsInfo$coefficients[, 1:2])
tipsCoef <- within(tipsCoef, {Lower <- Estimate - qt(p = 0.9, df = tipsInfo$df[2])*`Std. Error` 
                              Upper <- Estimate + qt(p = 0.9, df = tipsInfo$df[2])*`Std. Error` 
                                day <- row.names(tipsCoef)})
#plot both
ggplot(tipsByDay, aes(x = tip.mean, y = day)) + geom_point() +geom_errorbarh(aes(xmin = Lower,
                                                                                 xmax = Upper), height = 0.3) +
  ggtitle("tips by day calculated manually")

ggplot(tipsCoef, aes(x = Estimate, y = day)) + geom_point() +geom_errorbarh(aes(xmin = Lower,
                                                                                 xmax = Upper), height = 0.3) +
  ggtitle("tips by day calculated from regression model")

#16.2 multiple regression
#get housing data
housing <- read.table("http://www.jaredlander.com/data/housing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
head(housing)

#rename columns
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt","SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value", "ValuePerSqFt", "Boro")
head(housing)
#Exploratory of response variable - ValuePerSqFt through histogram
ggplot(housing, aes(x = ValuePerSqFt)) + geom_histogram(binwidth = 10) + labs(x = "ValuePerSqFt")
# there is a bimodal nature to the above plot, facet and color based on boro explore further
#overlaid
ggplot(housing, aes(x = ValuePerSqFt, fill = Boro)) + geom_histogram(binwidth = 10) + labs(x = "ValuePerSqFt")
#faceted
ggplot(housing, aes(x = ValuePerSqFt, fill = Boro)) + geom_histogram(binwidth = 10) + labs(x = "ValuePerSqFt") +
  facet_wrap(~Boro)
#plot histogram of sqft and units, and with a filter on units
ggplot(housing, aes(x = SqFt)) + geom_histogram()
ggplot(housing, aes(x = Units)) + geom_histogram()
ggplot(housing[housing$Units < 1000, ], aes(x = SqFt)) + geom_histogram()
ggplot(housing[housing$Units < 1000, ], aes(x = Units)) + geom_histogram()

#scatter plot value per sq ft versus sq ft and VPSQFT versus units, apply filter of 1000 units or less

ggplot(housing, aes(x = SqFt, y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = Units, y = ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units < 1000, ], aes(x = SqFt, y = ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units < 1000, ], aes(x = Units, y = ValuePerSqFt)) + geom_point()
# since data in both explorations seem skewed to the right a log transformation may be necessary
require(gridExtra)
p1 <- ggplot(housing, aes(x = SqFt, y = ValuePerSqFt)) + geom_point()
p2 <- ggplot(housing, aes(x = log(SqFt), y = ValuePerSqFt)) + geom_point()
p3 <- ggplot(housing, aes(x = SqFt, y = log(ValuePerSqFt))) + geom_point()
p4 <- ggplot(housing, aes(x = log(SqFt), y = log(ValuePerSqFt))) + geom_point()
grid.arrange(p1, p2, p3, p4, ncol=2)
# taking log of sqft maybe helpful in modeling
#try units
ggplot(housing, aes(x = Units, y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = log(Units), y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = Units, y = log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x = log(Units), y = log(ValuePerSqFt))) + geom_point()

#now run a linear model on predictors units, sqft, boro and response value per sqft
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)
coef(house1)
nrow(housing) - length(coef(house1))
house1$coefficients

require(coefplot)
coefplot(house1)
#interaction terms (units and sqft)
house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing)
summary(house2)
house3 <- lm(ValuePerSqFt ~ Units : SqFt + Boro, data = housing)
summary(house3)
grid.arrange(coefplot(house2), coefplot(house3), ncol = 2)

#three way interactions
house4 <- lm(ValuePerSqFt ~ Units * SqFt * Income, data = housing)
house4$coefficients

#test interaction ratio
house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, housing)
house6$coefficients
#multiplot in coefplot 
multiplot(house1, house2, house3)

#predict function
housingNew <- read.table("http://www.jaredlander.com/data/housingNew.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#make a predition with new data and 95% confidence level, predicting value per sqft 
#on the new data using old data model from house1
housePredict <- predict(house1, newdata = housingNew, se.fit = TRUE, interval = "prediction", level = .95)
head(housePredict$fit)
housePredict$fit
head(housePredict$se.fit)


