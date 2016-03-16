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





































