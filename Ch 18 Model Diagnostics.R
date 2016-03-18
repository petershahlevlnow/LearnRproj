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