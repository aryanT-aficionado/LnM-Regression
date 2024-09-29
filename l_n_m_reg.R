mouse.data<-data.frame(
  weight=c(6.7,5.6,8.9,4.5,3.5,7.8,4.6,9.8,2.5),
  size=c(4.3,5.7,6.8,3.9,1.2,4.0,3.7,5.8,8.9)
)
mouse.data

plot(mouse.data$weight,mouse.data$size)

mouse.regression<-lm(size~weight,data=mouse.data)

summary(mouse.regression)


frog.data <- data.frame(
  jump=c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  size=c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3))

frog.data

plot(frog.data$size,frog.data$jump)

frog.regression<-lm(size~jump,data=frog.data)
summary(frog.regression)

abline(frog.regression,col="blue")


mice.data <- data.frame(
  size = c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3),
  weight = c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  tail = c(0.7, 1.3, 0.7, 2.0, 3.6, 3.0, 2.9, 3.9, 4.0))

mice.data
plot(mice.data$weight,mice.data$size)
mice.regression<-lm(size~weight, data = mice.data)

summary(mice.regression)

plot(mice.data)

multiple.regression<-lm(size~weight+tail,data=mice.data)

summary(multiple.regression)
