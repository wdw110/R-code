library(xts)
library(readxl)
library(tseries)
library(forecast)

data <- read_excel("/Users/wdw/Desktop/electricity_consumption.xls")
length <- dim(data)[1]-93
d <- c(t(data[c(94:823),3])) 
v <- ts(d,start=c(2013,1,1),frequency=365)
plot(v)
adf.test(d) #数据仍然有非常好的平稳性
par(mfrow=c(2,1))  
acf(d)  
pacf(d)

d1<-diff(d,lag=1)#从acf来看，一阶差分是有必要的  
adf.test(d1)#stationary  
par(mfrow=c(2,1))  
acf(d1)  
pacf(d1)

#从acf来看，差分后的数据应该是一个ar（2）模型，给出模型估计  
ar2<-arima(d1,order=c(2,0,0))  
ar2  

#建模确定为arima（2,1,0）  
d.fit1<-arima(d,order=c(2,1,0)) 
d.forecast1 <- forecast(d.fit1,120)  
plot.forecast(d.forecast1)  

s <- HoltWinters(v)  
d2 <- forecast.HoltWinters(s, h=343)  
plot.forecast(d2)

write.csv(d2,"/Users/wdw/Desktop/result_forecast.csv")
result_fitted <- d2$fitted

result_forecast <- read.csv("/Users/wdw/Desktop/result_forecast.csv")[2]

data_forecast <- c(c(t(d2$fitted)),c(result_forecast)$Point.Forecast)
data_true <- c(t(data[94:1166,3])) 

x <- c(1:length)

plot(data_true~x,type="l",ylab='data')
lines(data_forecast~x[366:length],lty=2,col='2')
legend(x=0,y=170000,legend=c('True','Forecast'), col=c(1,2), lty=c(1,2))