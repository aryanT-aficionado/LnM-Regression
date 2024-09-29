#Set the working directory

setwd("/Users/aryantiwari/Documents/ucd material/R/bike sharing dataset")

library(ggplot2)

#Reading the data
bike = read.csv("day.csv")

#Prints the first few rows
head(bike)

#1. Boxplot of rentals per working day: 
table(bike$holiday) 
class(bike$holiday)


bike$workingday = factor(bike$workingday, levels = c(0,1), labels = c("no", "yes"))

#aes= aesthetics=> x=cnt:column, fill=workingday:fill areas of the graph with different colors
#

ggplot(bike, aes(x=workingday, y = cnt, fill = workingday))+geom_boxplot()+
  labs(title = 'Boxplots of rentals per working day')+theme_bw()

ggplot(bike,aes(x=holiday,y=cnt,fill=holiday))+geom_violin()

#Average rentals, per working day:
tapply(bike$cnt,bike$workingday,summary)


#2. Frequency of weather situation
#Factor() for treat variable as categorical and not numeric
bike$weathersit = factor(bike$weathersit, levels =c(1,2,3),
                         labels= c("1:Clear", "2:Cloudy", "3:Rain/Snow"))

ggplot(bike, aes(x = weathersit)) + 
  geom_bar() + labs(x = 'Weather', y = 'Frequency')



#3. Density plot: relative frequency of rentals
#geom_density

ggplot(bike, aes(x = cnt, fill = workingday)) +
  geom_density(alpha = 0.5) +  # alpha adds transparency to see overlaps
  labs(title = "Density Plot of Bike Rentals by Working Day", 
       x = "Number of Rentals", 
       y = "Density") +
  scale_fill_manual(values = c("skyblue", "orange"), 
                    name = "Working Day", 
                    labels = c("No", "Yes")) +
  theme_bw()



#4. Boxplot of rentals by day of the week
bike$weekday=factor(bike$weekday,levels = c(0,1,2,3,4,5,6),
                    labels = c("zero","one","two","three","four","five","six"))

bike$weekday = factor(bike$weekday, levels = c(0,1,2,3,4,5,6),
                      labels = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))

ggplot(bike,aes(x=weekday,y=cnt,fill=weekday))+
  geom_boxplot()+
  labs("Box plot by weekday")+
  theme_bw()

#Variability of rentals, by weekday




#4. Scartterplot of rentals versus temperature, by season
table(bike$season)

bike$season = factor(bike$season, levels =c(1,2,3,4),
                     labels = c("winter", "spring", "summer", "fall"))

ggplot(bike,aes(x= temp, y= cnt, color = season))+
  geom_point()+facet_wrap(~season)

#5. Summary of rentals, per season
summary(bike$season)
#mean(bike$season)
aggregate(bike$season,by=list(bike$weekday),FUN=mean)


#6. Pairs plot
install.packages("GGally")
library(GGally)
bike_subset = bike[,c(10,12,13,14,15)]
ggpairs(bike_subset)  

#7. Correlation
cor(bike_subset)

library(corrplot)
corrplot(cor(bike_subset),method = 'color')


#CLASS PRAC ---------------
summary(bike$casual)
summary(bike$temp)
var(bike$casual)


hist(bike$cnt, breaks = 20, xlab = "No. Rentals",ylab = "frequency",main = "HIstogram",col = "red")

boxplot(bike$temp,main="Temperature",col = "blue")
boxplot(bike$windspeed,main="iwnd speed",col = "green")


#look after
bike$weathersit=factor(bike$weathersit,levels = levels(bike$weathersit),labels = c("1:Clear", "2:Cloudy", "3:Rain/Snow"))
table(bike$weathersit)

prop.table(table(bike$weathersit))
tapply(bike$cnt,bike$weathersit,summary)




#Corelation of the 2 variables
cor(bike$temp,bike$cnt)
cor(bike$windspeed,bike$cnt)




