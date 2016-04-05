#Chapter 21 Time series and autocorrelation
# data correlated over time, one observation depends on a previous observation

#21.1 Autoregressive Moving Average - ARIMA
# AR - linear regressions of current value of time series against the previous values
# MA - linear regressions of current value of time series against the current and previous values 
# to illustrate get world bank data 
require(WDI)
# pull data
gdp <- WDI(country = c("US", "CA", "GB", "DE", "CN", "JP", "SG", "IL"), 
           indicator = c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"), start = 1960, end = 2011)
# give it good names
names(gdp) <- c("iso2c", "Country", "Year", "PerCapGDP", "GDP")
head(gdp)
require(ggplot2)
require(scales)
#per capita GDP
ggplot(gdp, aes(Year, PerCapGDP, color = Country, linetype = Country)) + geom_line() + 
  scale_y_continuous(label = dollar)
require(useful)
# absolute GDP
ggplot(gdp, aes(Year, GDP, color = Country, linetype = Country)) + geom_line() + 
  scale_y_continuous(label = multiple_format(extra = dollar, multiple = "M"))

# get US Data
us <- gdp$PerCapGDP[gdp$Country == "United States"]
#convert to a time series
us <- ts(us, start = min(gdp$Year), end = max(gdp$Year))
us
plot(us, ylab = "Per Capita GDP", xlab = "Year")
# another way to assess a time series is to view its autocovariance function (ACF) 
# and partial autocovariance (PACF)
# ACF shows the correlation of the time series with lags of itself. how much is a time series correlated 
# with itself at one lag, two lags, three and so on
# PACF is a little more complicated. the autocorrelation at lag one can have lingering effects on the
# autocorrelation at lag two and onward. the partial correlation is the amount of correlation between a time
# series and lags of itself that is not explained by a previous lag. So, the partial autocorrelation at lag
# two is the correlation between the time series and its second lag that is not explained by the first lag

acf(us)
pacf(us)
# shows time series is not stationary - mean and variance are constant for the whole series

# time series needs a number of transformations before it can be properly modeled. 
# diffing is the process of subtracting one observation from another
# example:
x <- c(1, 4, 8, 2, 6, 6, 5, 3)
# one diff
diff(x, differences = 1)
diff(x, differences = 2) # two iterative diffs
diff(x, lag = 2) # diff elements that are two indices apart

# figuring out the number of diffs can be difficult, but forecast package has the ability to find the optimal
# diffs
require(forecast)
ndiffs(x = us)
plot(diff(us, 2))

# R has ar and ma functions, but a better option is arima, which fits both AR and MA. The right order of 
# AR and MA is determined by analyzing acf and pacf, but fortunately forecast has auto.arima which will figure
# out the best specification.
usBest <- auto.arima(x = us)
usBest
# only an AR1 and MA1 component in this best fit model (ARIMA), which is different from the book which results
# in an ARIMA - AR1, AR2, MA1 
# confirm acf and pacf of residuals resemble white noise. this determines if it is a good fit
acf(usBest$residuals)
pacf(usBest$residuals)

coef(usBest)

# use predict function to predict 5 years into the future
predict(usBest, n.ahead = 5, se.fit = TRUE)
# visualize it using forecast function
theForecast <- forecast(object = usBest, h = 5)
plot(theForecast)

#20.2 VAR - vector autoregressive
# dealing with time series that depend on itself and other time series' past and present
# first convert all the gdp data into a multivariate time series 

# load reshape then cast data into a data.frame
require(reshape2)
#cast the data.frame to wide format
gdpCast <- dcast(Year ~ Country, data = gdp[, c("Country", "Year", "PerCapGDP")], value.var = "PerCapGDP")
head(gdpCast)
# convert to timeseries
gdpTS <- ts(data = gdpCast[, -1], start = min(gdpCast$Year), end = max(gdpCast$Year))
# build a plot and legend using base graphics
plot(gdpTS, plot.type = "single", col = 1:8)
legend("topleft", legend = colnames(gdpTS), ncol = 2, lty = 1, col = 1:8, cex = .9)

# remove germany from the data because of missing data
gdpTS <- gdpTS[ , which(colnames(gdpTS) != "Germany")]

# common method of fitting a model to multiple time series is with vector Autoregressive - VAR
# ar function can do this but has issues with singular matrices when the AR order is high, 
# so use VAR in the vars package.

# need to check if gdpTS needs to be diffed
numDiffs <- ndiffs(gdpTS)
numDiffs
gdpDiffed <- diff(gdpTS, differences = numDiffs)
plot(gdpDiffed, plot.type = "single", col = 1:7)
legend("bottomleft", legend = colnames(gdpDiffed), ncol = 2, lty = 1, col = 1:7, cex = .9)

# data is prepped. use VAR - fits a lm of each time series on the lags of itself and other series
require(vars)
gdpVAR <- VAR(gdpDiffed, lag.max = 12)
gdpVAR$p
names(gdpVAR$varresult) # name of each model
# each model is actually an LM
class(gdpVAR$varresult$Canada)
class(gdpVAR$varresult$Japan)
# each model has its own coeffecients
head(coef(gdpVAR$varresult$Canada))
head(coef(gdpVAR$varresult$Japan))

require(coefplot)
coefplot(gdpVAR$varresult$Canada)
coefplot(gdpVAR$varresult$Japan)
# predict 5 years ahead
predict(gdpVAR, n.ahead = 5)
