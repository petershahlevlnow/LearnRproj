say.hello <- function()
{
  print("Hello World!")
}

hello.person <- function(name)
{
  print(sprintf("Hello %s",name))
}

hello.person("Zort", extra = "meep")

hello.person <- function(fname, lname = "Doe", ...)
{
  print(sprintf("Hello %s %s",fname, lname))
}

double.num <- function(x)
{
  return (x * 2)
}

double.num(2)

do.call("hello.person", args = list(fname = "jared", lname = "lander"))

do.call(hello.person, args = list(fname = "jared", lname = "lander"))

run.this <-function(x, func = mean)
{do.call(func, args = list(x))}

run.this(1:10)
run.this(1:10, mean)
run.this(1:10, sum)
run.this(1:10, sd)
