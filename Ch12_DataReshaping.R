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

# 12.2.1 merge 
# merge Aid90s and Aid00s
Aid90s00s <- merge(x = Aid_90s, y = Aid_00s, by.x = c("Country.Name", "Program.Name"),
                   by.y = c("Country.Name", "Program.Name"))
head(Aid90s00s)

# note you can use the merge cmd to combine on different column names (by.x = "AA", by.y = "BB")
# this is slower than the plyr join operation

# 12.2.2 plyr join
# this join cmd can not specify different column titles, they must be the same
require(plyr)
Aid90s00sJoin <- join(x = Aid_90s, y = Aid_00s, by = c("Country.Name", "Program.Name"))
head(Aid90s00sJoin)

# join all eight data.frames using the join, first get everything into a list. Steps below:
# 1. figure out names of the data.frames
frameNames <- str_sub(string = theFiles, start = 12, end = 18)

# 2. build an empty list that is the length of frameNames
frameList <- vector("list", length(frameNames))

# 3. set names from FrameNames into list
names(frameList) <- frameNames

# 4. iterate to add each data.frame into the list
for(a in frameNames)
{
  #use eval parse here. <- operator requires a variable, but the names of data.frames 
  # in frameNames are characters. parse and evaluate the character will realize the actual variable
  frameList[[a]] <- eval(parse(text = a))
}

head(frameList[[1]])
head(frameList[["Aid_00s"]])
head(frameList[[5]])
head(frameList[["Aid_60s"]])

# 5. Now all data.frames are in list you can iterate through the list to join together, 
#    however using the Reduce function is much faster. Reduce passes an object(list vector) into a specified function
#    then successively combine the elements of the given object. ex. vector of 1:10 reduce(sum, vec) 1 and 2 are added
#    then 3 is added then 4 so on until result is 55.
allAids <- Reduce(function(...){ join(..., by = c("Country.Name", "Program.Name")) },
                  frameList)
# 6 check dimension
dim(allAids)

require(useful)
corner(allAids, c = 15)
bottomleft(allAids, c = 15)
