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
