setwd("C:/Users/dan_9/Desktop/DSO 545/HW/HW3")

library(ggplot2)
library(dplyr)

############################################################
# CASE 01

zillow = read.csv("Zillow2bedroom.csv")
head(zillow,20)

C1Q1 = aggregate( Price~Region , zillow, FUN = "mean")
C1Q1_dplyr = zillow %>% group_by(Region) %>% summarise(avgPrice = mean(Price)) 
# C1Q1order = C1Q1[order(C1Q1$Price),] # doesnt work

ggplot(C1Q1_dplyr, aes(x = avgPrice/1000, y = reorder(Region , avgPrice))) + 
  geom_point() + xlab("Price in Thousand") + ylab("") +
  ggtitle("Average Price for a 2 Bedroom Home in Los Angeles")+
  scale_x_continuous( breaks = seq(250,1250,250))



C1Q2 = aggregate( Price~Year , zillow, FUN = "mean")
C1Q2_dplyr = zillow %>% group_by(Year) %>% summarise( avgPrice = mean(Price))


ggplot(C1Q2_dplyr, aes(x = Year , y = avgPrice)) + 
  geom_line() + xlab("Year") + ylab("Average Price") +
  ggtitle("Average Price for a 2-Bedroom Home in Los Angeles") +
  scale_x_continuous( breaks = seq(2008,2015,1))

# why this doesnt work:
# ggplot(C1Q2, aes(x = factor(Year) , y = Price)) + geom_line()

ggplot(zillow, aes(x = jitter(as.numeric(Year)), y = Price)) + 
  xlab("Year") + ylab("Log of Price") + ggtitle("Prices for a 2-Bedroom Home in Los Angeles") +
  scale_y_log10() + geom_jitter(color = "blue", width = .1) +
  scale_x_continuous( breaks = seq(2008,2015,1))+
  coord_cartesian(xlim = c(2008,2015)) + geom_smooth()

C1Q4_dplyr = zillow %>% mutate(HomePrice = ifelse(Price >= 10^6, "High", "Low"))

ggplot(C1Q4_dplyr, aes(x = jitter(as.numeric(Year)), y = Price, colour = HomePrice)) +
  xlab("Year") + ylab("Log of Price") + ggtitle("Prices for a 2-Bedroom Home in Los Angeles") +
  scale_y_log10() + geom_jitter(width = 0.25) + 
  scale_x_continuous( breaks = seq(2008,2015,1))+
  scale_color_manual(values = c("red","black"))+
  coord_cartesian(xlim = c(2008,2015))+ 
  geom_smooth(se = FALSE,  method = "loess")


##################################################################
# Case 02

# 1. (3 points) Create a subset of only IPAs, i.e. include both Imperial/Double IPA and India Pale Ale
# (IPA). Show the following histagram of Abv. What does the histogram tell you?
beer = read.csv("ratebeer.csv")
case2 = tbl_df(beer)
glimpse(case2)
C2Q1 = case2 %>% filter(grepl("IPA", case2$Style)) %>% filter(Style != "Black IPA")
# or
C2Q1_ = case2 %>% filter(Style == "India Pale Ale (IPA)" |Style == "Imperial/Double IPA")

ggplot(C2Q1, aes(x = Abv, fill = Style, color = Style )) +geom_histogram(alpha = 0.5)+
  xlab("% of Alcohol by Volume") + ylab("Count") + ggtitle("Percentage of Alchohol in IPA's")

# 2. (3 points) Based on the subset you created in the previous question, calculate the total number of ratings
# for each brewer, then reproduce an EXACT graph as the following for the top 10 brewers which has received
# the highest ratings.

C2Q2 = C2Q1 %>% group_by(Brewer) %>% summarise(totalRatings = sum(Ratings)) %>% arrange(desc(totalRatings))

ggplot(C2Q2[1:10,], aes( y = totalRatings, x = reorder(Brewer, totalRatings ))) + geom_bar(stat = "identity") + coord_flip() +  
   ylab("Total Number of Ratings") + xlab("Brewer") + ggtitle("Top 10 IPA Breweries")


# 3. (2.5 points) We're all classy, so we only drink from the top 30 overall rated beers, but we CANNOT drink
# beers with Abv higher than 8% just because we don't want to be wasted!
# Randomly choose one beer for yourself from the top 30 rated beers! Output only the name of beer. (Hint:
# you might need to use sample_n() function from dplyr R package)

C2Q3 = case2 %>% filter(Abv <= 8.0) %>% arrange(desc(Ratings))

RandomBeer = sample_n(C2Q3[1:10,],1)

