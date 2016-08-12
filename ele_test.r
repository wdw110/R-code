#######用电模型#######

library(plyr)
library(dplyr)
library(ggplot2)
library(tseries)
library(forecast)
library(xts)

data = read.table('/Users/wdw/Desktop/test/data/electricity/data/ele_sell.txt',quote='\t',header=T)

data_df = tbl_df(data)

type = c('大工业','非普工业','农业生产','商业照明','城镇居民生活照明','农村居民生活照明','其它照明')

D_nj = data.frame()

model = data.frame()
i = 1
for(word in type){
	print(word)
	dd = filter(data_df,分类==word)
	print(dd)
}

nj = ts(matrix(data_df$南京, 37, 7), start = c(2013, 1), frequency = 12)
colnames(nj) = c('大工业','城镇居民生活照明','非普工业','农村居民生活照明','农业生产','其它照明','商业照明')

random = trend = seasonal = matrix(0,37,7)
fit = res = matrix(0,37,7)
ff = matrix(0,7,4,dimnames=list(c('大工业','城镇居民生活照明','非普工业','农村居民生活照明','农业生产','其它照明','商业照明'),c('月用电量','标准差','预测下界','预测上界')))
for(i in 1:7){
	f = decompose(nj[,i])
	seasonal[,i] = as.matrix(f$seasonal)
	trend[,i] = as.matrix(f$trend)
	random[,i] = as.matrix(f$random)
	fun = auto.arima(nj[,i])
	fore = forecast(fun, h=1)
	bound = c(fore$lower[2],fore$upper[2])
	std = sqrt(sum((nj[,i]-fore$fit)^2)/37)
	ff[i,] = c(c(fore$mean),std,bound)
	fit[,i]=c(fore$fitted)
	res[,i]=c(fore$residuals)
}

write.csv(random,'/Users/wdw/Desktop/test/data/electricity/model/random.csv')
write.csv(seasonal,'/Users/wdw/Desktop/test/data/electricity/model/seasonal.csv')
write.csv(trend,'/Users/wdw/Desktop/test/data/electricity/model/trend.csv')
write.csv(ff,'/Users/wdw/Desktop/test/data/electricity/model/ff.csv')
write.csv(fit,'/Users/wdw/Desktop/test/data/electricity/model/fit.csv')
write.csv(res,'/Users/wdw/Desktop/test/data/electricity/model/res.csv')


f = auto.arima(nj[,1])

ff <- forecast(f, h=12)





d1 = filter(data_df,分类=='大工业')

nj = ts(d1$南京[1:36],frequency=12,start=c(2013,01))

f = decompose(nj) #趋势分析 

plot(f)

adf.test(nj)  #数据平稳性检验

njl = log(nj)

njl_d = diff(njl, lag=1)

adf.test(njl_d)

par(family='STXihei')  ####图形中文乱码
par(mrfow=c(2,1))

acf(njl_d)
pacf(njl_d)

f1 <- auto.arima(nj)
f2 <- auto.arima(njl)
f3 <- auto.arima(njl_d)

ff1 <- forecast(f1, h=12)
ff2 <- forecast(f2, h=12)
ff3 <- forecast(f3, h=12)



