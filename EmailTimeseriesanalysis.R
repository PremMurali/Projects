#install.packages("plyr")
library(plyr)
var1="D:/R_code/export_dataframeTime.csv"
#data=read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)

data <- read.csv(var1)
View(data)

data$date=as.Date(data$date,format = "%d-%m-%Y")
data$date

library(plyr)
count(data$date)
newframe=count(data$date)
library(xts)
library(zoo)
library(tseries)
library(forecast)
#install.packages("fpp")
library(fpp)
timeseries=xts(newframe$freq,order.by = as.POSIXct(newframe$x))
plot(timeseries)
#ARIMA Model
arima=auto.arima(timeseries)
arimaforecast=forecast(arima,h=2)
arimaforecast
plot(arimaforecast)
accuracy(arimaforecast)
#Holt Winters
Holt_Winters=HoltWinters(timeseries,beta = FALSE,gamma = FALSE)
Holt_WintersPred=forecast(Holt_Winters,h=2)
plot(Holt_WintersPred)
accuracy(Holt_WintersPred)
#Trend
#Trend=HoltWinters(timeseries,gamma = F)
#PredTrend=forecast(Trend,h=2)
#plot(PredTrend)
#accuracy(PredTrend)
#Seasonal
#Seasonal=HoltWinters(timeseries)
#PredSeasonal=forecast(Seasonal,h=2)
#plot(PredSeasonal)
#accuracy(PredSeasonal)
