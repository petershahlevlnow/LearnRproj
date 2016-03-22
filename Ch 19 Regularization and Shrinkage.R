# Ch 19 Regularization and Shrinkage
# Reduction of variables to prevent overfitting 

# 19.1 Elastic Net
# dynamic blending of lasso and ridge regression 
# Lasso - uses L1 penalty to perform variable selection and dimension reduction
# Ridge - uses L2 penalty to shrink coefficients for more stable preditions

# glmnet is the package required for Elastic Net, unlike lm and glm it uses a matrix for predictors and response
# get data, note the data set is not very dimensional but will use it anyways
acs <- read.table("http://www.jaredlander.com/data/acs_ny.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
# buiild a matrix and run model.matrix on it
testframe <- data.frame(First = sample(1:10, 20, replace = TRUE), Second = sample(1:20, 20, replace = TRUE),
                        Third = sample(1:10, 20, replace = TRUE), 
                        Fourth = factor(rep(c("Alice", "Bob", "Charlie", "David"), 5)),
                        Fifth = ordered(rep(c("Edward", "Frank", "Georgia", "Hank", "Issac"), 4)),
                        Sixth = rep(c("a", "b"), 10), stringsAsFactors = F)
head(testframe)
head(model.matrix(First ~ Second + Fourth + Fifth, testframe))
# notice fourth is broken into columns of different levels, fifth is ordered factor where one level is greater 
# or less than another level

# in most linear models it is essential to avoid including the base level of a factor to avoid multicollinearity
# in ElasticNet we want to include all levels of a factor. This can be done with build.x in the useful library
require(useful)
#always use all levels
head(build.x(First ~ Second + Fourth + Fifth, testframe, contrasts = FALSE))
# just use all levels for fourth
head(build.x(First ~ Second + Fourth + Fifth, testframe, contrasts = c(Fourth = FALSE, Fifth = TRUE)))

# make a binary Income variable for building a logistic regression
acs$Income <- with(acs, FamilyIncome >= 150000)
head(acs)
# build a predictor matrix
# do not include the intercept as glmnet will add it automatically
acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + NumVehicles + NumWorkers +
                OwnRent + YearBuilt + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language -1, data = acs,
                contrasts = FALSE)
# check if matix and dim
class(acsX)
dim(acsX)
#view top left and top right of data
topleft(acsX, c=6)
topright(acsX, c = 6)

# Now build a response predictor
acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + NumVehicles + NumWorkers +
                OwnRent + YearBuilt + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language -1, data = acs)
head(acsY)
tail(acsY)

# now use glmnet, lambda controls amount of shrinkage. by default 100 different lambdas are used and the user must
# use cross validation to determine which is best. Fortunately glmnet has cv.glmnet that computes cross validation
# automatically. By default alpha is 1 meaning only lasso is calculated. Selecting the best alpha requires an additional
# level of CV

require(glmnet)
set.seed(1863561)
# run the cross validated glmnet
acsCV1 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5)
acsCV1$lambda.min
acsCV1$lambda.1se
plot(acsCV1) # plot the cross validation path
# extract coeff
coef(acsCV1, s = "lambda.1se")
# some variables are selected and others are not. lasso eliminates variables that are highly correlated 
# view where variables enter the model
# plot path
plot(acsCV1$glmnet.fit, xvar = "lambda")
# add vertical lines for the optimal values of lambda
abline(v = log(c(acsCV1$lambda.min, acsCV1$lambda.1se)), lty = 2)

# setting alpha to 0 causes the results from the ridge, every variable is kept but is shrunk closer to 0
set.seed(71623)
acsCV2 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5, alpha = 0)
# check lambdas
acsCV2$lambda.min
acsCV2$lambda.1se
# look at the coefficients
coef(acsCV2, s = "lambda.1se")
plot(acsCV2) #plot cross validation plot
plot(acsCV2$glmnet.fit, xvar = "lambda")
abline(v = log(c(acsCV2$lambda.min, acsCV2$lambda.1se)), lty = 2)

# to find the optimal value of alpha we need an additional layer of cross-validation. different levels of alpha
# need to be tested. GLMNET doesn't have an automatic way of doing this and doing this sequentially is time consuming
# This is where parallelization can be beneficial. Running code in parallel
require(parallel)
require(doParallel)

# when you run two - layered cross validation an observation needs to fall in the same fold each time
# set the seed for repeatability of random results
set.seed(2834673)
# create folds, we want observations to be in the same fold each time
# it is run
theFolds <- sample(rep(x = 1:5, legth.out = nrow(acsX)))
# make a sequence of alpha values. 
# in general it is better to lean towards lasso rather than ridge so we only consider values of alpha > .5
alphas <- seq(from = 0.5, to = 1, by = 0.05)
# before running a parallel