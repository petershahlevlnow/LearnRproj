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














































