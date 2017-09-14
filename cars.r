Brad Simkins
############################################
# Performing various analyses on the cars 
# data set
# Dataset: ("Car Data," submitted by Shonda 
# Kuiper, Grinnell College. Dataset obtained 
# from the Journal of Statistics Education 
# (http://www.amstat.org/publications/jse). 
############################################

mycars <- read.csv("C:/Users/evilc/Documents/GitHub/ds710summer2017assignment3/Cars 2005.csv")
attach(mycars)
install.packages("plotrix")

hist(Price) #histogram of frequency of prices

#compute proportion of cars priced between $10,000 and $20,000
meetCriteria <- which(Price >= 10000 & Price <= 20000)
proportion <- length(meetCriteria)/length(Price)
proportion
mean(Price)
median(Price)
#add verticle line at mean value
abline(v = mean(Price), col = "red", lwd = 2)
#add legend
legend("topright", legend = c("Mean price"), col=c("red"), lwd = 2)

#reduce skew with log
logPrice <- log(Price)
hist(logPrice,prob = T)
#normal density curve
curve(dnorm(x, mean(logPrice), sd(logPrice)), add = T)

#scatterplot of transformed price versus engine size
plot(logPrice,Liter)

#correlation between transformed price and engine size 
cor(logPrice,Liter)

#add leather seats to scatterplot
gph_colors <- rep("green",length(Price))
switcheroo <- which(Leather == "1")
gph_colors[switcheroo] <- "orange"
plot(logPrice,Liter, col = gph_colors)
legend("topright", legend = c("Leather Seats", "No Leather Seats"), col=c("Orange", "Green"), pch = 1)

#barplot of car type
sedans <- which(Type == "Sedan")
how_many <- table(mycars$Type)
barplot(how_many,ylim = c(0,length(sedans)+150))

#stacked barplot of car type that includes leather/no leather
how_many = table(Leather,Type)
barplot(how_many,ylim = c(0,length(sedans)+150), col = c("red", "blue"))
legend("topright", legend = c("Leather Seats", "No Leather Seats"), col=c("Red", "Blue"), lwd = 1)

#Boxplot of price by type
boxplot(Price~Type, las=2)

######two histograms in a vertical stack to allow comparison of price according to whether the car has a leather interior 
hist_break <- seq(0,80000,10000) #define breaks 
switchitup <- which(Leather == "0") #define cars with no leather
#store prices of cars with no leather
no_leather_price <- rep(0,length(switchitup))
no_leather_cnt <- c(1:length(switchitup))
no_leather_price[no_leather_cnt] <- Price[switchitup]
#store prices of cars with leather
leather_price <- rep(0,length(switcheroo))
leather_cnt <- c(1:length(switcheroo))
leather_price[leather_cnt] <- Price[switcheroo]

hist(leather_price,breaks = hist_break, main = "Prices of Cars With and Without Leather Seats", xlab = "Price of Cars", ylim = c(0,400), col = "green")
par(new=TRUE)
hist(no_leather_price, breaks = hist_break, main = NULL, xlab = NULL, ylim = c(0,400), col = "orange")
legend("topright", legend = c("Leather Seats", "No Leather Seats"), col=c("Green", "Orange"), lwd = 1)

#Side by side comparison of histograms
require(plotrix)
side_by_side <- list(leather_price,no_leather_price)
multhist(side_by_side, ylim = c(0,200),col=c("green","orange"), main = "Prices of Cars With and Without Leather Seats", xlab = "Price of Cars", ylab = "Frequency")
legend("topright", legend = c("Leather Seats", "No Leather Seats"), col=c("Green", "Orange"), lwd = 1)

