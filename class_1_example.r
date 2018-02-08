#
# Purchasing power parity - is it true?
#
#
# Data files that are comma-delimited (csv) files are easy to read into R using the read.csv() command.
# The first line of the file gives the variable names, and then each successive line is an observation.
#
ppp <- read.csv("c:/class/ascii/ppp.csv")
#
# Data frames can be attached within a session, avoiding the need to refer to variables using the data frame name.
# There is, however, a price to be paid for that, as the data frames of the variables being referred to can sometimes
# become confusing, to both the data analyst and sometimes (it seems to me) to R itself. Detaching previously-used
# data frames using the detach() command can help to avoid problems (although I don't tend to do that in these code
# files). You can also avoid the problem altogether by explicitly pointing to the data frame you need in your commands;
# for example, the plot command two command lines down could be
#   plot(ppp$Inflation.difference, ppp$Exchange.rate.change, xlab="Inflation difference", ylab="Exchange rate change")
# thereby making explicit what you want. If you want to refer to the same variables in different data frames it's now
# straightforward; for example 
#   plot(developed$Inflation.difference, developed$Exchange.rate.change, xlab="Inflation difference", ylab="Exchange rate change")
# would be used further down in the commands. If all else fails, simply quit from R and then open it again, as then 
# no data frames will be attached from previous sessions.
#
attach(ppp)
plot(Inflation.difference,Exchange.rate.change)
#
# A simple function that allows you to "brush" a point in a scatter plot is the identify() command.
# After running the command, click on the point(s) you're interested in. Clicking the escape key will
# stop the process, and right-clicking and clicking "Stop" will end it and return a list of the points
# identified.
#
identify(Inflation.difference, Exchange.rate.change)   
#
# Linear regression models are fit using the lm() command
#
pppa <- lm(Exchange.rate.change ~ Inflation.difference)
#
# Note that R labels the standard error of the estimate (the estimate of sigma) "Residual standard error"
#
summary(pppa)
#
# t-test for slope equaling 1. There is no reason to do this test unless a hypothesized specific value for the
# slope is meaningful in your particular situation (as is the case here). The next two lines give the t-test
# and the code to get the p-value
#
tslope1 <- (coef(pppa)[2]-1)/sqrt(diag(vcov(pppa))[2])
2.*(1-pt(abs(tslope1),39))
#
# Load John Fox's car package (see http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/)
# Note that certain functions in this or other packages might be different in different releases of the 
# package; see the package documentation if issues arise
#
# Packages must be downloaded and installed before they can be used. They can be installed from within R 
# by clicking on Package -> Install package(s) and following the instructions. A package only needs 
# to be downloaded and installed once; after that, it will always be available, and you can load it into 
# your current R session using the library() command. 
#
library(car)
#
# Partial F-test for slope coefficient equaling 1; this is equivalent to the t-test given earlier
#
linearHypothesis(pppa,c(0,1),rhs=1)
#
# Function for fitted line plot
#
regplot.confbands.fun <- function(x,y,confidencelevel=.95,CImean=T,PI=T,CIregline=F,legend=F){
      #### Modified from a function written by Sandra McBride, Duke University
	#### For a simple linear regression line, this function
	#### will plot the line, CI for mean response, prediction intervals, 
	#### and (optionally) a simulataneous CI for the regression line.
	xx <- x[order(x)]
	yy <- y[order(x)]
	lm1 <- lm(yy~xx)	
	plot(xx,yy,ylim=c(min(yy),(max(yy)+.2*max(yy))))
	abline(lm1$coefficients)
	#### calculation of components of intervals ####
	n <- length(yy)
	sx2 <- (var(xx))
	shat <- summary(lm1)$sigma
	s2hat <- shat^2
	SEmuhat <- shat*sqrt(1/n+ ((xx-mean(xx))^2)/((n-1)*sx2))
	SEpred <- sqrt(s2hat+SEmuhat^2)
	t.quantile <- qt(confidencelevel,lm1$df.residual)
	####
	if (CImean==T){
		mean.up <- lm1$fitted+t.quantile*SEmuhat
		mean.down <- lm1$fitted-t.quantile*SEmuhat
		lines(xx,mean.up,lty=2)
		lines(xx,mean.down,lty=2)
	}
	if (PI==T){
		PI.up <- lm1$fitted+t.quantile*SEpred
		PI.down <- lm1$fitted-t.quantile*SEpred
		lines(xx,PI.up,lty=3)
		lines(xx,PI.down,lty=3)
	}
	if (CIregline==T){
		HW <- sqrt(2*qf(confidencelevel,n-lm1$df.residual,lm1$df.residual))*SEmuhat	
		CIreg.up <- lm1$fitted+HW
		CIreg.down <- lm1$fitted-HW
		lines(xx,CIreg.up,lty=4)
		lines(xx,CIreg.down,lty=4)
	}	
      if (legend==T){
       	choices <- c(CImean,PI,CIregline)
       	line.type <- c(2,3,4)
             names.line <- c("Pointwise CI for mean resp.","Prediction Int.","Simultaneous conf. region for entire reg. line")
       	legend(max(xx)-.2*max(xx),max(yy)+.2*max(yy),legend=names.line[choices],lty=line.type[choices])
	}
}
regplot.confbands.fun(Inflation.difference,Exchange.rate.change)
#
# Confidence and prediction intervals for new observation
#
newppp <- data.frame(Inflation.difference = c(-1.5))
#
# Exact confidence and prediction intervals
#
predict(pppa,newppp,interval=c("confidence"))
predict(pppa,newppp,interval=c("prediction"))
plot(fitted(pppa),residuals(pppa),xlab="Fitted values",ylab="Residuals")
#
# If this had been time series data and you wanted to contract a plot of the observations in observation order (a time series plot), this
# is how you would do it.
#
plot(c(1:length(residuals(pppa))),residuals(pppa),type="b",xlab="Observation order",ylab="Residuals")
#
# Normal plot. Note that the axes are reversed compared to the plot in Minitab; this means that "stretching out" to the top and/or bottom of the
# plot, rather than to the left and/or right, corresponds to long tail(s).
#
qqnorm(residuals(pppa))
#
# Split data
#
developed <- ppp[Developed==1,]
developing <- ppp[Developed==0,]
attach(developed)
plot(Inflation.difference,Exchange.rate.change)
pppb <- lm(Exchange.rate.change ~ Inflation.difference)
summary(pppb)
linearHypothesis(pppb,c(0,1),rhs=1)
plot(fitted(pppb),residuals(pppb),xlab="Fitted values",ylab="Residuals")
qqnorm(residuals(pppb))
attach(developing)
plot(Inflation.difference,Exchange.rate.change)
pppc <- lm(Exchange.rate.change ~ Inflation.difference)
summary(pppc)
linearHypothesis(pppc,c(0,1),rhs=1)
plot(fitted(pppc),residuals(pppc),xlab="Fitted values",ylab="Residuals")
qqnorm(residuals(pppc))
#
# Omit Brazil
#
newdeveloping <- developing[developing$Country!="Brazil",]
attach(newdeveloping)
plot(Inflation.difference,Exchange.rate.change)
pppd <- lm(Exchange.rate.change ~ Inflation.difference)
summary(pppd)
linearHypothesis(pppd,c(0,1),rhs=1)
plot(fitted(pppd),residuals(pppd),xlab="Fitted values",ylab="Residuals")
qqnorm(residuals(pppd))
