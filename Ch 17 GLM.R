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



