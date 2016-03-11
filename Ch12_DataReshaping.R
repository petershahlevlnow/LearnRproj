#Chapter 12 - data reshaping

# cbind and rbind - combine two or more datasets of identical rows and columns
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
