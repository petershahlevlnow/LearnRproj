#Ch 14 Probabilty Distributions

#Ch 14.1 Normal Distributions
# 10 random numbers from a standard normal distribution
rnorm(n = 10)
# 10 random numbers from a  normal distribution with mean 100, sd = 20
rnorm(n = 10, mean = 100, sd = 20)
# dnorm calculates the probabilty of a specific value in a normal dist.
randNorm10 <- rnorm(n = 10)
randNorm10
dnorm(randNorm10)
dnorm(c(-1, 0, 1))
# plot a normal distribution
  # generate random values
randNorm <- rnorm(30000)
  # calculate the their distribution/probabilities
randDensity <- dnorm(randNorm)
require(ggplot2)
ggplot(data.frame(x = randNorm, y = randDensity)) + 
       aes(x = x, y = y) + 
       geom_point() + 
       labs(x = "Random Normal Variables", y = "Density")

# pnorm calculates the cumulative probabilty, by default left-tailed
pnorm(randNorm10)
pnorm(-1)
# to calculate the probabilty that a variable falls into two points, two tailed
pnorm(1) - pnorm(0)
pnorm(1) - pnorm(-1)
# illustration of pnorm, two-tailed, save a plot for later use
p <- ggplot(data.frame(x = randNorm, y = randDensity)) + 
     aes(x = x, y = y) + 
     geom_point() + 
     labs(x = "x", y = "Density")
  # find the area under the curve, with a shaded area under the curve
  # 1. create a sequence of numbers from -1 to 1
neg1seq <- seq(from = min(randNorm), to = -1, by = .1 )
  # 2. build a data.frame of the above seq
lessthanNeg1 <- data.frame(x = neg1seq, y = dnorm(neg1seq))
head(lessthanNeg1)
  # 3. combine with endpoints of left and right
lessthanNeg1 <- rbind(c(min(randNorm), 0), lessthanNeg1, c(max(lessthanNeg1$x), 0))
  # 4. add to p (plot from above)
p + geom_polygon(data = lessthanNeg1, aes(x = x, y = y))
  # 5. create a similar curve for -1 to 1
neg1pos1 <- seq(from = -1, to = 1, by = .1 )
neg1to1 <- data.frame(x = neg1pos1, y = dnorm(neg1pos1))
head(neg1to1)
neg1to1 <- rbind(c(min(neg1to1$x), 0), neg1to1, c(max(neg1to1$x), 0))
p + geom_polygon(data = neg1to1, aes(x = x, y = y))

#cumalative plot
randProb <- pnorm(randNorm)
ggplot(data.frame(x = randNorm, y = randProb)) +
aes(x= x, y = y) +
geom_point() + labs(x = "Random Normal Variables", y = "probability")

# qnorm - given a cumulative probabilty what is the quantile 
qnorm(0.5)
randNorm10
qnorm(pnorm(randNorm10))
all.equal(randNorm10, qnorm(pnorm(randNorm10)))
