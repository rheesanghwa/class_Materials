rm(list=ls())
ppp <- read.csv("~/Desktop/Multi Variate/ppp.csv")
library(car)

country = ppp$Country
inflation = ppp$Inflation.difference
fx = ppp$Exchange.rate.change
dm = ppp$Developed
model1 = lm(fx~inflation)
summary(model1)
library(car)

linearHypothesis(ppp,c(0,1),rhs=1)

plot(fx~inflation)
abline(model1)

plot(fx~inflation, xlim=c(-30, 5), ylim=c(-30,5))
pppd = ppp[which( ppp$Developed == 1),]
