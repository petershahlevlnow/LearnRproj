# Ch 13 manipulating strings

# 13.1 paste
# notice spaces automatically added between strings with no third arg defined
paste("hello", "Jared", "and others")
paste("hello", "Jared", "and others", sep = "/")
# can aslo paste vectors, paired
paste(c("hello", "jared", "winters"), c("meep", "zort", "narf"))
paste("hello", c("jared", "winters", "meep"))
paste("hello", c("jared", "winters", "meep"), "goodbye")

# use paste to collapse a vector
vectorOftext <- c("hello", "everyone", "out there", ".")
paste(vectorOftext, collapse = " ")
paste(vectorOftext, collapse = "*")

#13.2 sprintf

person <- "Jared"
partysize <- "eight"
waitTime <- 25
paste("Hello ", person, ", your party of ", partysize, " will be seated in ", waitTime, " minutes.",sep = "")
sprintf("Hello %s, your party of %s will be seated in %s minutes.", person, partysize, waitTime)
#sprintf can also be vectorized, but must be multiples of each other
sprintf("Hello %s, your party of %s will be seated in %s minutes.", c("Jared", "Bob"), c("eight", 16, "four", 10),
        waitTime)
