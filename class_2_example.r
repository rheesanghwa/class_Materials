#
# Getting what you pay for: dinner prices in New York City
#
# Note that I will not necessarily include R code for all of the analyses in this handout (and future
# handouts) if code for those methods has already been illustrated for earlier handouts.
#
zagat1 <- read.csv("c:/class/ascii/zagat1.csv")
attach(zagat1)
summary(zagat1)
plot(Food,Cost)
plot(Decor,Cost)
plot(Service,Cost)
zagata <- lm(Cost ~ Food+Decor+Service)
summary(zagata)
#
# Load John Fox's car package (see http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/)
#
library(car)
vif(zagata)
plot(fitted(zagata),residuals(zagata),xlab="Fitted values",ylab="Residuals")
#
# Normal plot
#
qqnorm(residuals(zagata))
#
# Partial F-test for total service being good enough
# Set up a matrix of coefficients for the contrasts beta1-beta2=0 and beta2-beta3=0
# Below the four entries in each row correspond to beta0, beta1, beta2, beta3, so the
# first row says 0*beta1 + 1*beta1 -1*beta2 + 0*beta3 = 0, or beta1=beta2. By a similar
# argument the second row is beta2=beta3.
#
zcon <- matrix(c(0,1,-1,0,0,0,1,-1),nrow=2,byrow=T)
linearHypothesis(zagata,zcon)
#
# Omit food variable
#
zagatb <- lm(Cost ~ Decor+Service)
summary(zagatb)
vif(zagatb)
#
# Omit outlier
#
zagat1a <- zagat1[-124,]
attach(zagat1a)
zagatc <- lm(Cost ~ Food+Decor+Service)
summary(zagatc)
zagatd <- lm(Cost ~ Decor+Service)
summary(zagatd)
vif(zagatd)
plot(fitted(zagatd),residuals(zagatd),xlab="Fitted values",ylab="Residuals")
qqnorm(residuals(zagatd))
plot(Decor, residuals(zagatd), ylab="Residuals")
plot(Service, residuals(zagatd), ylab="Residuals")
#
# Validate on new data
#
zagat2 <- read.csv("c:/class/ascii/zagat2.csv")
pred1 <- predict(zagatc,zagat2)
pred2 <- predict(zagatd,zagat2)
#
# This prints out the first (restaurant name) and seventh (cost) columns of the validation set
# along with the upper and lower prediction limits
#
cbind(zagat2[,c(1,7)],pred1,pred2)
