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

# an
