#Chapter 12 - data reshaping

# 12.1 cbind and rbind - combine two or more datasets of identical rows and columns
# make two vectors and combine them as columns in data.frame
sport <- c("hockey", "baseball", "basketball", "football")
leauge <- c("NHL", "MLB", "NBA", "NFL")
trophy <- c("Stanley Cup", "Commissioners Trophy", "Ball Trophy", "Vince Lombardy Trophy")

trophies1 <- cbind(sport, leauge, trophy)
trophies2 <- data.frame(sport = c("basketball", "golf"), leauge = c("DNBA", "PGA"), 
                        trophy = c("Minor league b-ball trophy", "Wanamaker trophy"),
                        stringsAsFactors = FALSE)
trophies <- rbind(trophies1, trophies2)
# can also change column titles with cbind
cbind(Sport = sport, Association = leauge, Prize = trophy)

# 12.2 Joins
# first download and unzip file for url
download.file(url = "http://jaredlander.com/data/US_Foreign_Aid.zip", 
              destfile = "data/ForeignAid.zip")
unzip("data/ForeignAid.zip", exdir = "data")

# load the files programattically with a for loop
require(stringr)
theFiles <- dir("data/", pattern  = "\\.csv")
for(a in theFiles)
{
  #build a good name to assign to the data, in this case "Aid_XXs" thus char 12 - 18 of file name
  nameToUse <- str_sub(string = a, start = 12, end = 18)
  #read in the csv using read.table, 
  #file path is a convienent way to specifya folder and file name
  temp <- read.table(file = file.path("data", a), header = TRUE, sep = ",", 
                     stringsAsFactors = FALSE)
  #assign to the workspace
  assign(x = nameToUse, value = temp)
}