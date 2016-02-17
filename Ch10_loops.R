for (i in 1:10)
{
  print(i)
}

fruitname <- c("apples", "bananas", "pears")
fruitlength <- rep(NA, length(fruitname))

names(fruitlength) <- fruitname

for (a in fruitname)
{
  fruitlength[a] <- nchar(a)
}

#instead of iterating
fruitlength2 <- nchar(fruitname)
names(fruitlength2) <- fruitname
