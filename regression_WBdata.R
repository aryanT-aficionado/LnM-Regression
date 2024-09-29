setwd("/Users/aryantiwari/Documents/ucd material/Modern Regression")
library(ggplot2)

world_bank_data=read.csv("world_bank_data.csv")

head(world_bank_data)

library(naniar)
vis_miss(world_bank_data,sort_miss = TRUE)

world_bank_data=na.omit(world_bank_data)

table(world_bank_data$country)

ggplot(world_bank_data,aes(x=country))+geom_bar()

#EDA on UnemployementRate = Y
summary(world_bank_data$unem_rate)

ggplot(world_bank_data,aes(y=unem_rate))+geom_boxplot()

ggplot(world_bank_data,aes(x=unem_rate))+geom_histogram(bins = 30)+theme_bw()

#Cross Plot: Y and categorical X:country
ggplot(world_bank_data,aes(x=country,y=unem_rate,fill=country))+geom_boxplot()+
  labs(title = "BoxPlot of Umemp by country",y="Unemp Rate", x="Country")

tapply(world_bank_data$unem_rate,world_bank_data$country,summary)
tapply(world_bank_data$unem_rate,world_bank_data$country,var)

#Cross Plot: Y and numerical Y:GDP Growth
ggplot(world_bank_data,aes(x=gdp_growth,y=unem_rate,color=country))+
  geom_point()+
  labs(title = "BoxPlot of Umemp by GDP",y="Unemp Rate", x="GDP")

ggplot(world_bank_data,aes(x=gdp_growth,y=unem_rate,color=country))+
  geom_point()+xlim(0,13)


#Corelations

cor(world_bank_data[,3:6])
install.packages("corrplot")
library(corrplot)

corrplot(cor(world_bank_data[,3:6]),method="number")

usa_data=world_bank_data[world_bank_data$country=="United States",]
spain_data=world_bank_data[world_bank_data$country=="Spain",]

head(usa_data)

y=usa_data$unem_rate
x=usa_data$gdp_growth
n=length(y)

ss_xy=sum(x*y) -n*mean(x)*mean(y)
ss_x=sum(x^2)-n*mean(x)^2
beta_1=ss_xy/ss_x
beta_1
beta_0=mean(y)-beta_1*mean(x)
beta_0

#using lm function
usa_reg<-lm(unem_rate~gdp_growth,data=usa_data)
usa_reg
summary(usa_reg)

#predicted value of 
spain_reg<-lm(unem_rate~gdp_growth,data = spain_data)
spain_reg
summary(spain_reg)

y_fitted_USA<-beta_0+beta_1*usa_data$gdp_growth
y_fitted_USA
head(cbind(usa_data$unem_rate,y_fitted_USA))

sse<-sum((usa_data$unem_rate-y_fitted_USA)^2)
sse


multi_regress_USA<-lm(unem_rate~gdp_growth+inflation,data=usa_data)
multi_regress_USA
summary(multi_regress_USA)

X=cbind(rep(1,nrow(usa_data)),usa_data$gdp_growth,usa_data$inflation)
Y=usa_data$unem_rate

solve(t(X)%*%X)%*%t(X)%*%Y

n=nrow(usa_data)
p=1
sigma2=sse/(n-p)
sigma2
sqrt(sigma2)
summary(multi_regress_USA)
