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
