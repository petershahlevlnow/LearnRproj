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
theFolds <- sample(rep(x = 1:5, length.out = nrow(acsX)))
# make a sequence of alpha values. 
# in general it is better to lean towards lasso rather than ridge so we only consider values of alpha > .5
alphas <- seq(from = 0.5, to = 1, by = 0.05)

# before running a parallel job a cluster must be started and registered with makeCluster and registerDoParallel.
# it must be stopped after the job is complete with stopCluster
# .errorhandling = remove, if error occurs that iteration is skipped
# .inorder = FALSE, the order of combining the results does not matter
# .multicombine = TRUE speed up process for combination function, list
# .packages set to glmnet so it is loaded on each workers
# .export used to explicitly load into the foreach enviornment
# %dopar% operator means do foreach loop in parallel

#set seed for random repeatedly
set.seed(5127151)
#start a cluster with two workers
c3 <- makeCluster(2)
registerDoParallel(c3) # register workers
before <- Sys.time() # keep track of timing
#build foreach loop to run in parallel
acsDouble <- foreach(i = 1:length(alphas), .errorhandling = "remove" , .inorder = FALSE, .multicombine = TRUE,
                    .export = c("acsX", "acsY", "alphas", "theFolds"), .packages = "glmnet") %dopar%
{
  print(alphas[i])
  cv.glmnet(x = acsX, y = acsY, family = "binomial", nfolds = 5, foldid = theFolds,
            alpha = alphas[i])
  #print(alphas[i])
}

# stop timing
after <- Sys.time()
stopCluster(c3) # stop cluster
# time difference
after - before

# use sapply to check classes in acsDouble
sapply(acsDouble, class)

#function for extracting info from cv.glmnet object
extractGlmnetInfo <- function(object)
{
  # find lambdas
  lambdaMin <- object$lambda.min
  lambda1se <- object$lambda.1se
  
  #figure out wher those lambdas fall in the path
  whichMin <- which(object$lambda == lambdaMin)
  which1se <- which(object$lambda == lambda1se)
  
  #build one line data.frame with each of the selected lambdas and its corresponding error figures
  data.frame(lambda.min = lambdaMin, error.min = object$cvm[whichMin], lambda.1se = lambda1se, 
             error.1se = object$cvm[which1se])
}

# apply that function to each element of the list
# combine it all into a data.frame
alphaInfo <- Reduce(rbind, lapply(acsDouble, extractGlmnetInfo))
# also could be done with the ldply from plyr
alphaInfo2 <- plyr::ldply(acsDouble, extractGlmnetInfo)
identical(alphaInfo, alphaInfo2)

#make a column listing the alphas
alphaInfo$Alpha <- alphas
alphaInfo
# plot these numbers from alphaInfo

##prepare the data.frame for plotting multiple pieces of information
require(reshape2)
require(stringr)

# melt the data into long format
alphaMelt <- melt(alphaInfo, id.vars = "Alpha", value.name = "Value", variable.name = "Measure")
# extract 1se or min error types, make new column
alphaMelt$Type <- str_extract(string = alphaMelt$Measure, pattern = "(min)|(1se)") 
# some house keeping 
alphaMelt$Measure <- str_replace(string = alphaMelt$Measure, pattern = "\\.(min|1se)", replacement = "")
# cast lambda and error into 2 seperate columns
alphaCast <- dcast(alphaMelt, Alpha + Type ~ Measure, value.var = "Value")
# plot
ggplot(alphaCast, aes(x=Alpha, y = error)) + geom_line(aes(group = Type)) + 
  facet_wrap(~Type, scales = "free_y", ncol = 1) + geom_point(aes(size=lambda))

# optimal alpha is 0.75. it minimizes error on 1se basis
# refit the model to new alpha
set.seed(5127151)
acsCV3 <- cv.glmnet(x = acsX, y = acsY, family = "binomial", nfold = 5,  
                    alpha = alphaInfo$Alpha[which.min(alphaInfo$error.1se)])
plot(acsCV3)
plot(acsCV3$glmnet.fit, xvar = "lambda")
abline(v = log(c(acsCV3$lambda.min,acsCV3$lambda.1se)), lty = 2)

# create a coefficient plot, glmnet doesn't have coefplots so create one manually
theCoef <- as.matrix(coef(acsCV3, s = "lambda.1se"))
coefDF <- data.frame(Value = theCoef, Coefficients = rownames(theCoef))
coefDF <- coefDF[nonzeroCoef(coef(acsCV3, s = "lambda.1se")),]
ggplot(coefDF, aes(x = X1, y = reorder(Coefficients, X1))) + 
  geom_vline(xintercept = 0, color = "grey", linetype =2 ) + 
  geom_point(color = "blue") + labs(x = "Value", y = "Coefficient", title = "Coefficient Plot")

# num of workers and food stamps are the strongest indicator of income, 
# NumUnitsMobile home and heating fuel wood are the strongest indicator of having lower income

#19.2 Bayesian Shrinkage
# useful when a model buiilt on data that does not have large enough number of rows for some combinations 
# of the variables
# download rdata
download.file(url = "http://www.jaredlander.com/data/ideo.rdata", destfile = "data/ideo.rdata", quiet = TRUE)
load("data/ideo.rdata")
head(ideo)

# to show the need for shrinkage we fit a seperate model for each election year the display resulting 
# coefficients for black level Race
##fit a bunch of models
# figure out the years we will be fitting the models on 
theYears <- unique(ideo$Year)

# create an empty list
# as many elements as years
# it holds the results
# preallocating the object makes the code run faster
results <- vector(mode = "list", length = length(theYears))
names(results) <- theYears
#loop through the years 
# fit model on subset of that year
for(i in theYears)
{
  results[[as.character(i)]] <- glm(Vote ~ Race + Income + Gender + Education, data = ideo, subset = Year ==i,
                                    family = binomial(link = "logit"))
}

# plot coefficients for black level Race. 
require(coefplot)

voteinfo <- multiplot(results, coefficients = "Raceblack", plot = FALSE)
head(voteInfo)
multiplot(results, coefficients = "Raceblack", secret.weapon = TRUE) + coord_flip(xlim = c(-20, 10))
# notice 1964 has a different error than others, something clearly wrong. to fix put a prior the coeffecient model
# bayesglm simplest way to add Cauchy prior
require(arm)

resultsB <- vector(mode = "list", length = length(theYears))
names(resultsB) <- theYears

for(i in theYears)
{
  resultsB[[as.character(i)]] <- bayesglm(Vote ~ Race + Income + Gender + Education, 
                                          data = ideo[ideo$Year == i,],
                                          subset = Year ==i, family = binomial(link = "logit"), 
                                          prior.scale = 2.5, prior.df = 1)
}

multiplot(resultsB, coefficients = "Raceblack", secret.weapon = TRUE)

















  