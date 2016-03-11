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


