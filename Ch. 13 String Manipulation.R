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

#13.3 Extracting Text
require(XML)
load("data/presidents.rdata")
theURL <- "http://www.loc.gov/rr/print/list/057_chron.html"
# use readHMTLTable to parse the url html text
presidents1 <- readHTMLTable(theURL, which =3, as.data.frame = TRUE, skip.rows = 1, header =  TRUE,
                             stringAsFactor = FALSE)

head(presidents1)
tail(presidents1$YEAR)
#Only include first 64 rows because bottom of table is shit
presidents1 <- presidents1[1:64,]
# Create a seperate column for year start and end in pres table
# Use str split to split on hyphen and embed into a list
require(stringr)
yearList <- str_split(string = presidents1$YEAR, pattern = "-")
head(yearList)
#combine the list into one matrix
yearMatrix <- data.frame(Reduce(rbind, yearList))
names(yearMatrix) <- c("Start", "Stop")
#cbind year Matrix into pres table and convert into numeric (must convert to characterfirst)
presidents1  <- cbind(presidents1, yearMatrix)
head(presidents1)
presidents1$Start = as.numeric(as.character(presidents1$Start))
presidents1$Stop = as.numeric(as.character(presidents1$Stop))
tail(presidents1)
#finding a subset of strings str_sub
str_sub(string = presidents1$PRESIDENT, start = 1, end = 3)
str_sub(string = presidents1$PRESIDENT, start = 4, end = 8)

#find presidents that were elected in a year ending in 0, which means they start in a year ending in 1
presidents1[str_sub(string = presidents1$Start, start = 4, end = 4) == 1, 
            c("YEAR", "PRESIDENT", "Start", "Stop")]

#13.4 Regular Expressions
# find a president with the name "John", use str_detect
JohnPos <- str_detect(string = presidents1$PRESIDENT, pattern = "John")
presidents1[JohnPos, c("YEAR", "PRESIDENT", "Start", "Stop")]



