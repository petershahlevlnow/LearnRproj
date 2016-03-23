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

# 20.2 Splines 