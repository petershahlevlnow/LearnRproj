theURL <- "http://www.jaredlander.com/data/Tomato%20First.csv"
theURL
tomato <- read.table(file = theURL, header = TRUE, sep = ",")
head(tomato)
db <- odbcConnect("QV Training")
customerTable <- sqlQuery(db, "SELECT * FROM bank.customer", stringsAsFactors = FALSE)
head(customerTable)
nameTable <- sqlQuery(db, "SELECT fname, lname 
                            FROM bank.customer 
                            INNER JOIN bank.individual 
                            ON bank.customer.cust_id = bank.individual.cust_id", stringsAsFactors = FALSE)
