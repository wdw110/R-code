D<-read.table('/Users/wdw/Desktop/大伟收/kkk.txt',header=T)

colnames(D)<-c('tem','speed','angle1','angle2')

#1.求拟合函数

mod<-nls(speed~a*sin(b*angle2+c)+d,start=list(a=14.13,b=0.0175,c=-1.514,d=0.4001),data=D,trace=T)

summary(mod)

#2.判断拟合效果

library(ggplot2)
p <- ggplot(D,aes(angle2, speed))
p+geom_point(size=3)+geom_line(aes(angle2,fitted(mod)),col='red')

#3.残差判断
plot(fitted(mod) , resid(mod),type='b')