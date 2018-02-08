#
# Purchasing power parity, revisited
# Previously created data frame of developed countries
#
attach(developed)
pppb <- lm(Exchange.rate.change ~ Inflation.difference)
library(car)
linearHypothesis(pppb,matrix(c(1,0,0,1),ncol=2,byrow=T),rhs=c(0,1))
#
# Previously created data frame of developing countries without Brazil
#
attach(newdeveloping)
pppd <- lm(Exchange.rate.change ~ Inflation.difference)
linearHypothesis(pppd,matrix(c(1,0,0,1),ncol=2,byrow=T),rhs=c(0,1))
