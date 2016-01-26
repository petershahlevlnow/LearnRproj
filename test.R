theURL <- "http://www.jaredlander.com/data/Tomato%20First.csv"
theURL
tomato <- read.table(file = theURL, header = TRUE, sep = ",")
head(tomato)
db <- odbcConnect("QV Training")
customerTable <- sqlQuery(db, "SELECT * FROM customer", stringsAsFactors = FALSE)
head(customerTable)
nameTable <- sqlQuery(db, "SELECT fname, lname 
                            FROM bank.customer 
                            INNER JOIN bank.individual 
                            ON bank.customer.cust_id = bank.individual.cust_id", stringsAsFactors = FALSE)

save(tomato, file= "data/tomato.rdata")
rm(tomato)
head(tomato)
load("data/tomato.rdata")
head(tomato)

n <- 20
r <- 1:10
w <- data.frame(n,r)
n
r
w
save(n,r,w,file = "data/multiple.rdata")
rm(r,n,w)
load("data/multiple.rdata")

require(XML)
theURL_2 <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool"
bowlpool <- readHTMLTable(theURL_2,which = 1, header = FALSE, stringsAsFactors = FALSE)
bowlpool
