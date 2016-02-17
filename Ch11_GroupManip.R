#ch 11 - Group Manipulation

# apply function

theMatrix <- matrix(1:9, nrow = 3)
apply(theMatrix, 1, sum)
apply(theMatrix, 2, sum)

#alternatively
rowSums(theMatrix)
colSums(theMatrix)

#missing element, with na.rm
theMatrix[2,1] <- NA
apply(theMatrix, 1, sum)
apply(theMatrix, 1, sum, na.rm = TRUE)

rowSums(theMatrix, na.rm = TRUE)
colSums(theMatrix, na.rm = TRUE)

# lapply and sapply works on lists and vectors, which are technically lists

thelist <- list(A = matrix(1:9, nrow = 3), 
                B = 1:5, C = matrix(1:4, nrow = 2), D = 2)

lapply(thelist, sum)
sapply(thelist, sum)

theNames <- c("jared", "peter", "ellison")
lapply(theNames, nchar)

# mapply
firstList <- list(A = matrix(1:16, nrow = 4), B = matrix(1:16, nrow = 2), C = 1:5)
secondList <- list(A = matrix(1:16, nrow = 4), B = matrix(1:16, nrow = 4), C = 15:5)
mapply(identical, firstList, secondList)

simplefunc <- function(x, y)
{
  NROW(x) + NROW(y)
}

mapply(simplefunc, firstList, secondList)

#aggregate function

require(ggplot2)
data(diamonds)
head(diamonds)

aggregate(price ~ cut, diamonds, mean, na.rm = TRUE)
aggregate(price ~ cut + color, diamonds, mean, na.rm = TRUE)
aggregate(cbind(price, carat) ~ cut, diamonds, mean, na.rm = TRUE)
aggregate(cbind(price, carat) ~ cut + color, diamonds, mean, na.rm = TRUE)

# plyr functions input one type, ouput antother type
# ddplyr

require(plyr)
data("baseball")
head(baseball)

#change sacrafice flies before 1954 from NA to 0
baseball$sf[baseball$year < 1954] <- 0
# check 
any(is.na(baseball$sf))
#keep only players with >= 50 at bats
baseball <- baseball[baseball$ab >= 50, ]
#check
any(baseball$ab < 50)

#calculate on base % (OBP) with vecotorization just for each season
baseball$OBP <- with(baseball, (h + bb + hbp)/(ab + bb + hbp + sf))
tail(baseball)

#calculate on base % per player's career

odp <- function(data)
{
  c(OBP = with(data, sum(h + bb + hbp)/sum(ab + bb + hbp + sf)))
  
}

#use ddplyr here to calculate career OBP and then sort and view top 20 players
careerOBP <- ddply(baseball, .variables = "id", .fun = odp)
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing = TRUE), ]
head(careerOBP, 20)

#llply - identical results as llapply
llply(thelist, sum)
identical(lapply(thelist, sum), llply(thelist, sum))

#laply - similar to sapply except no vector names
laply(thelist, sum)
sapply(thelist, sum)

#plyr helper functions
# each - for multiple functions

aggregate(price ~ cut, diamonds, each(mean, median), na.rm = TRUE)

# idataframe - creates a referece to existing df for faster processing
system.time(dlply(baseball, "id", nrow))
iBaseball <- idata.frame(baseball)
system.time(dlply(iBaseball, "id", nrow))

# data.table - faster data processing due to DB style indexing
require(data.table)
theDF <- data.frame(A = 1:10,
                    B = letters[1:10],
                    C = LETTERS[11:20],
                    D = rep(c("One", "Two", "Three"), length.out = 10))

theDT <- data.table(A = 1:10,
                    B = letters[1:10],
                    C = LETTERS[11:20],
                    D = rep(c("One", "Two", "Three"), length.out = 10))   

#difference between DT and DF - DF turns characters into factors, where DT does not
class(theDF$B)
class(theDT$B )

# turn a DF into a DT
diamondsDT <- data.table(diamonds)
diamondsDT

theDT[1:2, ]
theDT[theDT$A >= 7, ] #not an ideal way to do this

#with DTs columns should be specified as a list (note DFs are with character labels ex: "B"),
#see difference in 2nd and 3rd cmd below
theDT[ , list(A, C)]
theDT[ , B]
theDT[ , list(B)]

#if passing an argument to a function that requires character arguments use with = FALSE
theDT[, "B", with = FALSE]
theDT[, c("A","B"), with = FALSE]

# Keys
tables()
# set key provides additional speed
setkey(theDT, D)
theDT
key(theDT)

theDT["One",]
theDT[c("One", "Two"),] # return >1 column with key

setkey(diamondsDT, cut, color) #set more than one key cut and color
diamondsDT[J("Ideal","E")] #return rows with both keys
diamondsDT[J("Ideal",c("E","D"))] # return rows with both keys and vector of colors

