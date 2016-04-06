# Ch 22 Clustering 
# grouping rows that are similar to each other

#22.1 K-means
# clustering based on some distance metric
# get data on wines

download.file(url = "http://www.jaredlander.com/data/wine.csv", destfile = "data/Wine.csv", quiet = TRUE)
wine <- read.table("data/Wine.csv", header = TRUE, sep = ",")
head(wine)

# remove Cultivar column from data, as it will heavily influence clusters
wineTrain <- wine[, which(names(wine) != "Cultivar")]
# set seed for reproducibilty
set.seed(278613)
# k-means function takes two args - data and number of clusters. Must cluster on numeric data not categorical
wineK3 <- kmeans(x = wineTrain, centers = 3)
wineK3
# plot 
require(useful)
plot.kmeans(wineK3, data = wineTrain)
plot(wineK3, data = wineTrain)
# if we pass the original data with Cultivar we can see how well the cluster membership aligns to Cultivar
plot(wineK3, data = wine, class = "Cultivar")

# K means is subject to random starting conditions so it is good practice to run it with a number of 
# random starts
set.seed(278613)
wineK3N25 <- kmeans(wineTrain, centers = 3, nstart = 25)
# see how cluster sizes compare
wineK3N25$size
wineK3$size

# Use Hartigan's rule to determine the right number of clusters.
wineBest <- FitKMeans(wineTrain, max.clusters = 20, nstart = 5, seed = 278613)
wineBest
PlotHartigan(wineBest) # this shows 13 clusters may be optimal
# plot a confusion matrix to check fit between clusters and Cultivar. If this was a good fit the diagnol would
# be the largest number
table(wine$Cultivar, wineK3N25$cluster)
plot(table(wine$Cultivar, wineK3N25$cluster), main = "Confusion Matrix for Wine Clustering", 
     xlab = "Cultivar", ylab = "Cluster")

# an alternative to Haritgan is Gap Statistic with clusGap - measures the gap between reality and expectation. 
# it measures the dissimilarity for a clustering data with that of a bootstrapped sample of data. 
require(cluster)
theGap <- clusGap(wineTrain, FUNcluster = pam, K.max = 20)
gapDF <- as.data.frame(theGap$Tab)
gapDF
# optimal # of clusters is the smallest number producing a gap within one standard deviation of the number
# clusters that minimizes the gap
# logW curves
ggplot(gapDF, aes(x=1:nrow(gapDF))) + geom_line(aes(y=logW), color = "blue") +
  geom_point(aes(y=logW), color = "blue") + geom_line(aes(y=E.logW), color = "green") +
  geom_point(aes(y=E.logW), color = "green")
# blue = observed within-cluster dissimilarity 
# green = expected within cluster dissimilarity
ggplot(gapDF, aes(x=1:nrow(gapDF))) + geom_line(aes(y=gap), color = "red") +
  geom_point(aes(y=gap), color = "red") + geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color = "red") +
  labs(x = "Number of Clusters", y = "Gap")
# red = gap statistic (expected - observed) 
# errorbars are the standard deviation of gap

# 20.2 PAM - Partitioning around mediods 
# K means can't do categorical data
# K mediods 

indicators <- c("BX.KLT.DINV.WD.GD.ZS", "NY.GDP.DEFL.KD.ZG", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG",
                "NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG", "TG.VAL.TOTL.GD.ZS")
require(WDI)
#pull info on these indicators for all countries in our list
# not all countries have info for every indicator
# some countries dont have data
wbInfo <- WDI(country ="all", indicator = indicators, start = 2011, end = 2011, extra = TRUE)
# get rid of aggregated info
wbInfo <- wbInfo[wbInfo$region != "Aggregates",]
# get rid of countries where all indicators are NA
wbInfo <- wbInfo[which(rowSums(!is.na(wbInfo[,indicators])) > 0), ]
wbInfo <- wbInfo[!is.na(wbInfo$iso2c),] # remove rows where the iso is missing

# set rownames so we can know the country without using that for clustering
rownames(wbInfo) <- wbInfo$iso2c
# refactor region, income, lending to account for any changes in the levels
wbInfo$region <- factor(wbInfo$region)
wbInfo$income <- factor(wbInfo$income)
wbInfo$lending <- factor(wbInfo$lending)
# now fit the clustering using PAM
# find which colums to keep
# not those in this vector
keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year", "capital", "iso3c"))
wbPAM <- pam(x = wbInfo[, keep.cols], k =12, keep.diss = TRUE, keep.data = TRUE)
# show the medoid observations
wbPAM$medoids
plot(wbPAM, which.plots = 2, main = "")
