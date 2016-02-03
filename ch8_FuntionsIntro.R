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

double.num(3)
