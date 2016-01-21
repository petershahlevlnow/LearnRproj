theURL <- "http://www.jaredlander.com/data/Tomato%20First.csv"
theURL
tomato <- read.table(file = theURL, header = TRUE, sep = ",")
head(tomato)
