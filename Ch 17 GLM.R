#Ch 17 Generalized Linear Models

#17.1 Logistic Regression
acs <- read.table("http://www.jaredlander.com/data/acs_ny.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#create a binary variable for incomes greater than $150K
acs$Income <- with(acs, FamilyIncome >= 150000)
#plot family income to get a sense of what is going on
require(ggplot2)
require(useful)
ggplot(acs, aes(x = FamilyIncome)) + geom_density(fill = "grey", color = "grey") + 
  geom_vline(xintercept = 150000) + scale_x_continuous(label = multiple.dollar, limits = c(0, 1000000))

head(acs)
# use the glm function as Income as the response
income1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType, data = acs, 
               family = binomial(link = "logit"))
summary(income1)
require(coefplot)
coefplot(income1)

#interpreting the coeffcients necessitates taking the inverse logit
invlogit <- function(x)
{
  1/(1 + exp(-x))
}

invlogit(income1$coefficients)

#17.2 poisson regression
# used on count data
# investigate num of children
#first plot the data to investigate, not perfectly poisson but still can fit a possion model to it
ggplot(acs, aes(x = NumChildren)) + geom_histogram(binwidth = 1)
# now fit a poisson model to NumChildren
children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, data = acs, family = poisson(link = "log"))
summary(children1)
coefplot(children1)
# a concern for poission regression is overdispersion - variability in the data is greater than theorized by
# a poission distribution where mean and variance are the same
# calculate the overdispersion
# standardized residuals
z <- (acs$NumChildren - children1$fitted.values)/sqrt(children1$fitted.values)
# over dispersion factor
sum(z^2)/children1$df.residual
# overdispersion p-value
pchisq(sum(z^2), children1$df.residual)
# in general a overdispersion factor greater than 2 indicates overdispersion, while this ratio is less than 2 the
# p-vale is 1 meaning there is statistically significant overdispersion. So we need to refit the model
# to account for overdispersion using the quassipoisson
children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, data = acs, family = quasipoisson(link = "log"))
summary(children2)
multiplot(children1, children2)
#because the overdispersion was not that large there is a little more uncertaintity 

#17.3 other glm 
# glm function supports - gamma, inverse gaussian, quasibinomial, different link functions can be supplied - 
# logit, probit, cauchit, log and cloglog for binomial; inverse, identity and log for gamma; log, idendtity
# and sqrt poision; and 1/mu^2, inverse, identity and log for inverse gaussian

# Multinomial regression - classifying multiple categories: 1. running multiple logistic regressions. 2. using
# polr function 3. multinom function from the nnet package