library(fpp2)
getwd()
getwd()
setwd("/Users/williams/Documents")
eComm_csv<-read.csv("eComm_US.csv")


setwd("C:/Users/William/OneDrive/Desktop/jo/nci/Statistics for Data analysis/stats_project")
eComm_csv<-read.csv("eComm_US.csv")
ecomm<-ts(eComm_csv$ECOMNSA,start=c(1999,4),frequency = 4)
ecomm
plot(ecomm,xlab="Years",ylab="Sales($Billions)")
class(ecomm)
summary(ecomm) #gives mean of time series analysis

#Seasonal decomposition using decompose() - multiplicative
fit.decmult<-decompose(ecomm,type = "multiplicative")
fit.decmult
plot(fit.decmult)
boxplot(ecomm~cycle(ecomm))
cycle(ecomm)

plot(ecomm)
start(ecomm)
end(ecomm)
frequency(ecomm)



autoplot(ecomm)+
  autolayer(ma(ecomm,7))+
  autolayer(ma(ecomm,3))

monthplot(ecomm)
seasonplot(ecomm)

ecom1<-window(ecomm, start=c(1994, 4), end=c(2016, 4))
ecom1

ecom2<-window(ecomm, start=c(2017))
ecom2





#mean model
fcast.mean<-meanf(ecomm,h=3)
summary(fcast.mean)
plot(fcast.mean)

#naive model
fcast.naive<-naive(ecomm,h=3)
summary(fcast.naive)
plot(fcast.naive)

#seasonal naive model
fcast.seasonalnaive<-snaive(ecomm,h=3)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)



accuracy(fcast.mean)
accuracy(fcast.naive)
accuracy(fcast.seasonalnaive)
#accuracy(fcast.mean,ecomm)
#accuracy(fcast.naive,ecomm)
#accuracy(fcast.seasonalnaive,ecomm)

library(xlab)
library(forecast)
library(ggplot2)
autoplot(ecomm)+
  autolayer(fcast.mean,PI=FALSE,series="mean")+
  autolayer(fcast.naive,PI=FALSE,series="naive")+
  autolayer(fcast.seasonalnaive,PI=FALSE,series="seasonal naive")+
  xlab("Year")+ylab("sales")+
  ggtitle("Retail sales data")+
  guides(colour=guide_legend(title="Forecast"))

#Exp Smothing
#simple
library(forecast)
simplefit<-forecast::ses(ecomm,h=3)
simplefit
plot(simplefit)
autoplot(simplefit)
autoplot(simplefit)+autolayer(fitted(simplefit),series = "fitted")+xlab("Year")+ylab("Sales($Billion)")


accuracy(simplefit)


#using holt window we are using hw beacsue our data has both trend and seasonality
holt2<-hw(ecomm,h=3)
holt2
plot(holt2)
accuracy(holt2)
autoplot(holt2)+autolayer(fitted(holt2),series = "fitted")+xlab("Year")+ylab("Sales($Billion)")

holt2<-hw(ecomm,seasonal="multiplicative",h=3)
holt2
plot(holt2)
accuracy(holt2)
autoplot(holt2)+autolayer(fitted(holt2),series = "fitted")+xlab("Year")+ylab("Sales($Billion)")




#holt
holt1<-holt(ecomm,h=3)
holt
plot(holt1)
accuracy(holt1)

autoplot(holt1)+autolayer(fitted(holt1),series = "fitted")+xlab("Year")+ylab("Sales($Billion)")



expfit1<-ets(ecomm,model = "ANN")
expfit1
plot(expfit1)

expfit2<-ets(ecomm,model = "AAA")
expfit2
plot(expfit2)
accuracy(expfit2)




expfit4<-ets(ecomm,model = "ZZZ")
expfit4
plot(expfit4)
accuracy(expfit4)


#ARIMA modelling
#first we use the ndiff function to find if our data needs to be differneced or not
library(tseries)
adf.test(ecomm)
logecomm<-log(ecomm)
adf.test(logecomm)
plot(logecomm)

par(mfrow=c(1,2))
ts.plot(ecomm,main="Data: ecomm",ylab="sales")
ts.plot(logecomm,main="Data: logecomm",ylab="sales")

par(mfrow=c(1,1))






Acf(logecomm)
Pacf(logecomm)
ggtsdisplay(logecomm)


library(forecast)
plot(logecomm)
ndiffs(logecomm)
dlogecomm<-diff(logecomm)
plot(dlogecomm)
adf.test(dlogecomm)

spectrum(dlogecomm)

sdiff_dlogecomm<-diff(dlogecomm,lag = 4)
plot(sdiff_dlogecomm)


adf.test(sdiff_dlogecomm)

plot(sdiff_dlogecomm)
ggtsdisplay(sdiff_dlogecomm,main="Log Time series data after non-seasonal and seasonal difference ")
$
  
  qqnorm(fit6$residuals)
qqline(fit6$residuals)
Box.test(fit6$residuals,type = "Ljung")
checkresiduals(fit6)
accuracy(fit6)



fit6<-auto.arima(sdiff_dlogecomm)
fit6

sdiff_dlogecomm

Acf(sdiff_dlogecomm)
Pacf(sdiff_dlogecomm)
fit_arima<-arima(sdiff_dlogecomm,order=c(4,0,0))
fit_arima
accuracy(fit_arima)
fc1<-forecast(fit_arima,h=3)
fc1
fc1[7]
plot(fc1$fitted)

fit_arima

fc1$mean<-exp(fc1$mean)
fc1$upper<-exp(fc1$upper)
fc1$lower<-exp(fc1$lower)
fc1$x<-exp(fc1$x)
fc1$fitted<-exp(fc1$fitted)
rmse<-sqrt(mean((fc1$fitted-fc1$x)^2))
rmse

fc1$mean

qqnorm(fit6$residuals)
qqline(fit6$residuals)
Box.test(fit6$residuals,type = "Ljung")
checkresiduals(fit6)





slogdiff_ecomm<-diff(logecomm,lag = 4,differences = 1)
dslogdiff_ecomm<-diff(slogdiff_ecomm)
ggtsdisplay(dslogdiff_ecomm)
pp.test(slogdiff_ecomm)


Acf(dlogecomm)
pacf(dlogecomm)

logd_fit<-auto.arima(dlogecomm)
logd_fit

qqnorm(logd_fit$residuals)
qqline(logd_fit$residuals)
Box.test(logd_fit$residuals,type = "Ljung")
checkresiduals(logd_fit)
accuracy(logd_fit)



ndiffs(ecomm)
plot(ecomm)

pp.test(ecomm)
#pp test shows p value of .6343 >.05 so accept null hypothesis hence conclude that the time series should be atleastt differneced once

decomm<-diff(ecomm)
plot(decomm)

pp.test(decomm)
#p value is less than .05 hence reject null hypothesis showing this is a stationary process

#its shows that we need to difference the data once
#next we will plot the ACF
Acf(decomm)
Pacf(decomm)

#now we check for seasonality for that we use spectral anaysis

spectrum(ecomm)
spectrum(decomm)
#this shows a strong seasonal trend with a frequecy of 1 with 95% confidenc inteval


#now we need to remove seasonality...for that we have three methods
sdiff_decomm<-diff(decomm,lag = 4,differences = 1)
par(mfrow=c(2,2))
ts.plot(decomm,main="Data: ecomm",ylab="sales")
acf(decomm,main="Sample ACF of ecomm",ylab="")
ts.plot(sdiff_decomm,main=" Data: sdiff_ecomm",ylab= "increase in number of sales")
acf(sdiff_decomm,main="Sample ACF of sdiff_ecomm",ylab="")
par(mfrow=c(1,1))

acf(sdiff_decomm)
pacf(sdiff_decomm)

ggtsdisplay(sdiff_decomm)

#we try to fit non-seasonal AR(0) and seasonal AR(1)
fit1<-arima(ecomm,order = c(4,1,0),seasonal = c(1,1,0))
fit1
qqnorm(fit1$residuals)
qqline(fit1$residuals)
Box.test(fit1$residuals,type = "Ljung")
checkresiduals(fit1)
accuracy(fit1)

#we try to fit non-seasonal AR(1) and seasonal AR(1)
fit2<-arima(ecomm,order = c(1,1,0),seasonal = c(1,1,0))
fit2
qqnorm(fit2$residuals)
qqline(fit2$residuals)
Box.test(fit2$residuals,type = "Ljung")
checkresiduals(fit2)
accuracy(fit2)





ggtsdisplay(dsdiff_ecomm)
ndiffs(sdiff_ecomm)
ggtsdisplay(dsdiff_ecomm)


ggtsdisplay(decomm)

plot(sdiff_ecomm)
ndiffs(sdiff_ecomm)
pp.test(sdiff_ecomm)
#this shows that we need to diff the data again

dsdiff_ecomm<-diff(sdiff_ecomm)
plot(dsdiff_ecomm)
acf(dsdiff_ecomm)
fit5<-auto.arima(dsdiff_ecomm)
fit5
accuracy(fit5)






library(tseries)
adf.test(decomm)

#this shows that it is SARIMA
fitArima<-auto.arima(ecomm)
fitArima

qqnorm(fitArima$residuals)
qqline(fitArima$residuals)
Box.test(fitArima$residuals,type = "Ljung")
checkresiduals(fitArima)
accuracy(fitArima)