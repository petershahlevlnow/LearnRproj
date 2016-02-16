toCheck <- 0

if (toCheck == 1)
{ 
  print("hello")
}

check.bool <- function(x)
{
  if (x == 1)
  {
    print("Hello")
  } else
  {
    print("Goodbye")
  }
}

check.bool <- function(x)
{
  if (x == 1)
  {
    print("Hello")
  } else if (x == 0)
  {
    print("Goodbye")
  } else
  {
    print("confuse")
  }
}

use.switch <- function(x)
{
  switch(x,
         "a" = "first",
         "b" = "second",
         "c" = "third",
         "z" = "last",
         "other")
}

use.switch("6")

use.switch(5)
is.null(use.switch(9))

toTest <- c(1, 3, 4, 1, 0, 1)
ifelse(toTest == 1, "yes", "No")

ifelse(toTest == 1, toTest * 3, toTest * -1)

toTest[2] <- NA

a <- c(1, 1, 0, 1)
b <- c(2, 1, 0, 1)

ifelse(a == 1 &  b == 1, "yes", "no")

ifelse(a == 1 &&  b == 1, "yes", "no")
