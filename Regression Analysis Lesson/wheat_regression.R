##Wheat Regression
######################
#Wheat
W_Yield <- read.csv("C:/Users/Michael/Google Drive/Teaching Experimental Design/assignments/wheat_yield.csv", header=TRUE) 
names(W_Yield)
str(W_Yield)

library(ggplot2)
ggplot(W_Yield,(aes(x=Year, y=bu_acre))) + geom_point()+geom_smooth(method='lm') + 
stat_smooth(method="loess")

#It is possible to subset the data to llook at different time periods
W_1949<-subset(W_Yield, Year>1948) # get years only 1949-1999

#subset within the regression equation
w_fit_sub<-lm(bu_acre~Year, data=subset(W_Yield, Year>1948))

#Regression on all the data
w_fit_sub<-lm(bu_acre~Year, data=W_Yield)
summary(w_fit)
plot(w_fit$residuals)
plot(density(w_fit$residuals))
shapiro.test(w_fit$residuals)
library(car)
ncvTest(w_fit)

w_fit2<-lm(bu_acre~poly(Year,2), data=W_Yield)
summary(w_fit2)
plot(w_fit2$residuals)
plot(density(w_fit2$residuals))
shapiro.test(w_fit2$residuals)
ncvTest(w_fit2)

w_fit3<-lm(bu_acre~poly(Year,3), data=W_Yield)
summary(w_fit3)
plot(w_fit3$residuals)
plot(density(w_fit3$residuals))
shapiro.test(w_fit3$residuals)
ncvTest(w_fit3)

w_fit4<-lm(bu_acre~poly(Year,4), data=W_Yield)
summary(w_fit4)
plot(w_fit4$residuals)
plot(density(w_fit4$residuals))
shapiro.test(w_fit4$residuals)
ncvTest(w_fit4)


library(rcompanion)
compareLM(w_fit,w_fit2,w_fit3,w_fit4)
anova(w_fit,w_fit2,w_fit3,w_fit4)

plot(W_Yield$Year, W_Yield$bu_acre)
confidence.intervals <- predict(w_fit3,x=W_Yield$Year,interval='confidence',level=0.95)
lines(W_Yield$Year,confidence.intervals[,1],col='black',lwd=3)
lines(W_Yield$Year,confidence.intervals[,2],col='red',lwd=1)
lines(W_Yield$Year,confidence.intervals[,3],col='red',lwd=1)
predicted.intervals <- predict(w_fit3,x=W_Yield$Year,interval='prediction', level=0.95)
lines(W_Yield$Year,predicted.intervals[,2],col='blue',lwd=1)
lines(W_Yield$Year,predicted.intervals[,3],col='blue',lwd=1)

###timeseries
library(forecast) 
library(timeSeries)

ts_wyield<-ts(W_yield$bu_acre, start=c(1909))
plot.ts(ts_wyield)
ts_fit<-auto.arima(ts_wyield) #Auto-regressive model
plot(ts_fit)
acf(ts_wyield)
accuracy(ts_fit)

f <- forecast(ts_fit, 50) 
print(f) 
plot(f)

ts_forecasts <- HoltWinters(ts_wyield, gamma=F) #exponential forcasting fucntion
plot(ts_forecasts)
ts_forecasts2 <-forecast(ts_forecasts, h=50)
plot(ts_forecasts2)
m <- HoltWinters(ts_wyield,gamma=FALSE) #false as there doesn't appear to be periodicity in the data, while this is not entirly true we are not concered with detrending here
p <- predict(m, 50, prediction.interval = TRUE)
plot(m, p)

#rolling regression to look at change in slope over time
library(zoo)
library(roll)
roll.mean<-rollapply(WYRR$bu_acre,5,mean)
roll.sd<-rollapply(WYRR$bu_acre,5,sd)

NY<-W_Yield[-c(1:4),-3]
plot(W_Yield$Year, W_Yield$bu_acre)
lines(NY$Year, roll.mean)
lines(NY$Year, roll.mean + roll.sd, col="red") 
lines(NY$Year, roll.mean-roll.sd, col="red")

#one way to do the regression
roll.fit <- roll_lm(as.matrix(WYRR$Year),as.matrix(WYRR$bu_acre),width=5)
roll.fit$coefficients
roll.fit$r.squared

#another way to do the rolling regression
WY<-W_Yield[,-3]
WY<-as.matrix(WY)
roll_reg<-rollapply(WY, width = 5,
                    function(x) coef(lm( y ~ x, data = as.data.frame(x))),
                    by.column = FALSE, align = "right")
summary(roll_reg)

