# Chapter 10 loops

# for loops most times can use vectorization

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

#instead of iterating with a for loop
fruitlength2 <- nchar(fruitname)
names(fruitlength2) <- fruitname

#While loops
x <- 1
while (x <= 5)
{
  print(x)
  x <- x + 1 
  
}
