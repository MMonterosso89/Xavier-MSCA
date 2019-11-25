library(forecast)
library(TTR)
library(data.table)
library(fpp2)

DeptSale <- fread("C:\\Users\\Matth\\Desktop\\MSCA SUMM 19\\MSCA Fall19\\2DepartmentStoreSales.csv")


ds.ts <- ts(DeptSale$Sales, start = c(1992,1), end = c(2016,10), freq = 12)


autoplot(ds.ts)

acf(DeptSale$Sale, lag.max = 12) %>% autplot()

##large ACF at lags 1 and 2 indicate strong trend and seasonality at lag 12

###Subset of model

DeptSale$Period <- lubridate::mdy(DeptSale$Period)

library(tidyverse)

DeptSale_small <- DeptSale %>% filter(Period >= "1997-01-01" & Period <= "2007-01-01")

acf(DeptSale_small$Sales)





#Holt-Winters Exponential Smoothing (Triple Exponential Smoothing)
#Generate Holt-Winters Exponential Smoothing Model
ets(ds.ts)
ds.ts.etsMAM <- ets(ds.ts, model = "MAM")

##test if all unknown yields better, oof

ds.ts.etsUnknown <- ets(ds.ts, model = "ZZZ")


plot(ds.ts.etsMAM)

ds.ts.etsMAM
ds.ts.etsUnknown
#same AIC, so we have called out our error, trends, and seasonality correctly

AIC_Scores <- NULL

AIC_Scores$HoltWinterES <- ds.ts.etsMAM$mse

#Forecast 5 years into the future
ds.ts.etsMAM.forecast <- forecast(ds.ts.etsMAM, h=5, level = c(80,95))
plot(ds.ts.etsMAM.forecast, ylab="Sales", xlab = "Year", flty = 2)
lines(ds.ts.etsMAM$fitted, col = "red")
ds.ts.etsMAM.forecast
#test for auto-correlations
checkresiduals(ds.ts.etsMAM.forecast, df=NULL, lag = 12)

Box.test(ds.ts.etsMAM.forecast$residuals, lag = 12, type = "Ljung-Box")

##Residuals are NOT independently distributed


accuracy(ds.ts.etsMAM)




#######Regression

ggplot(DeptSale, aes(x = Period, y = Sales)) +
  ylab("Sales") +
  xlab("Years") +
  geom_point() + geom_smooth(method="lm", se=FALSE)

#estimate the equation with tslm() function

#TSLM DOES NOT WORK ON OUR DATASET ITS NOT NAMED PROPERLY


DeptSale$Sales


DeptSale


ds.ts


str(ds.ts_df)

ds.ts_reg <- tslm(ds.ts ~ trend)


ds.ts_reg

plot(ds.ts, xlab = "Years", ylab = "Sales")
lines(ds.ts_reg$fitted.values)


summary(ds.ts_reg)
##try exponential trend fit

ds.ts_reg_exp <- tslm(ds.ts ~ trend, lambda = 0)


ds.ts_reg_exp

plot(ds.ts, xlab = "Years", ylab = "Sales")
lines(ds.ts_reg_exp$fitted.values)

summary(ds.ts_reg_exp)

fcast.et <- forecast(ds.ts_reg_exp, h = 10)

autoplot(fcast.et)

#Why is this r^2 1? The fit is horrible??


#check the model output
summary(uschange1.ts)
#Plot the fitted values vs. the original time series data
autoplot(ds.ts, series = "Data") +
  autolayer(fitted(ds.ts_reg_exp), series = "Fitted")

##Not strong enough curve with this method.


ds.ts_reg_poly <- tslm(ds.ts ~ trend + I(trend^2))

plot(ds.ts, xlab = "Years", ylab = "Sales")
lines(ds.ts_reg_poly$fitted.values)

summary(ds.ts_reg_poly)




#autocorrelation & normality
checkresiduals(ds.ts_reg_exp)

#Still have massive annual seasonality
#Add season page 412

ds.ts_reg_poly_season <- tslm(ds.ts ~ trend + I(trend^2) + season)

plot(ds.ts, xlab = "Years", ylab = "Sales")
lines(ds.ts_reg_poly_season$fitted.values, col = "blue")

summary(ds.ts_reg_poly_season)

accuracy(ds.ts_reg_poly_season)


#autocorrelation & normality
checkresiduals(ds.ts_reg_poly_season)

####Try Fourier

ds.ts_fourier <- tslm(ds.ts ~ trend + fourier(ds.ts, K = 2))

plot(ds.ts, xlab = "Years", ylab = "Sales")
lines(ds.ts_fourier$fitted.values, col = "blue")

summary(ds.ts_fourier)

accuracy(ds.ts_fourier)

##It's bad

#autocorrelation & normality
checkresiduals(ds.ts_reg_poly_season)

Box.test(ds.ts_reg_poly_season$residuals, lag = 12, type = "Ljung-Box")

fcast.reg_poly_s <- forecast(ds.ts_reg_poly_season, h = 10)

autoplot(fcast.reg_poly_s)


#still an issue with acf, not sure how to fix?


###Try on subet of data






######Expontial Trend
###Generate holt exp smooth model

ds.ts.etsAAN <- ets(ds.ts, model = "AAN")

plot(ds.ts.etsAAN)


#Forecast 5 years into future

ds.ts.etsAAN.forecast <- forecast(ds.ts.etsAAN, h = 5, level = c(80,95))

plot(ds.ts.etsAAN.forecast, ylab = "Sales", xlab = "Year", flty = 2)

lines(ds.ts.etsAAN.forecast$fitted, col = "red")

##check for autocorr 
checkresiduals(ds.ts.etsAAN.forecast, df = NULL, lag = 20)

#Still Massive annual seaonality, idk

accuracy(ds.ts.etsAAN.forecast)


###Cubic Spline

#Smoothing Spline (equivalent to an ARIMA(0,2,2) with restricted parameter space)
fit.spline <- splinef(ds.ts, lambda = 0, h=10)
autoplot(fit.spline)

autoplot(ds.ts) +
  autolayer(fitted(fit.spline), series = "Spline")

#forecasts
fcast.spline <- forecast(fit.spline, h = 10)


autoplot(fcast.spline)

accuracy(fcast.spline)


###ARIMA

summary(ur.kpss(ds.ts))
#data is stationary

ndiffs(ds.ts)

ggAcf(diff(ds.ts))

ggPacf(diff(ds.ts))


DS_ARIMA <- auto.arima(ds.ts, seasonal = TRUE)

autoplot(forecast(DS_ARIMA))



#Check model results
checkresiduals(DS_ARIMA)

accuracy(DS_ARIMA)
