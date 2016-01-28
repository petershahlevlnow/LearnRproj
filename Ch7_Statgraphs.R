require(ggplot2)
data("diamonds")
head(diamonds)
hist(diamonds$carat, main = "Carat_histogram", xlab = "Carat")
plot(price ~ carat, data = diamonds)
boxplot(diamonds$carat)
ggplot(data = diamonds) + geom_histogram(aes(x = carat))
ggplot(data = diamonds) + geom_density(aes(x = carat), fill = "grey50")
ggplot(diamonds, aes(x = carat, y = price)) + geom_point()
g <- ggplot(diamonds, aes(x = carat, y = price))
g + geom_point()

#scatters
g + geom_point(aes(color = color))
g + geom_point(aes(color = color)) + facet_wrap(~color)
g + geom_point(aes(color = color)) + facet_grid(cut ~ clarity)
ggplot(diamonds, aes(x = carat)) + geom_histogram() + facet_wrap(~color)

#boxplots
ggplot(diamonds, aes(y = carat, x = 1)) + geom_boxplot()
ggplot(diamonds, aes(y = carat, x = cut)) + geom_boxplot()
ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin()
ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin() + geom_point()
ggplot(diamonds, aes(y = carat, x = cut)) + geom_point() + geom_violin()

#line graph
ggplot(economics, aes(x = date, y = pop)) + geom_line()
head(economics)

require(lubridate)
economics$year <- year(economics$date)
economics$month <- month(economics$date, label = TRUE)
econ2000 <- economics[which(economics$year >= 2000),]

require(scales)
g <- ggplot(econ2000, aes(x = month, y = pop))
g <- g + geom_line(aes(color=factor(year), group = year))
g <- g + scale_color_discrete(name = "Year")
g <- g + scale_y_continuous(labels = comma)
g <- g + labs(title = "Population", x = "Month", y = "Year")
g

#themes
require(ggthemes)

g2 <- ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(color = color))
g2 <- g2 + theme_economist() + scale_colour_economist()
g2 <- g2 + theme_excel() + scale_colour_excel()
g2 <- g2 + theme_tufte()
g2 <- g2 + theme_wsj()
g2
