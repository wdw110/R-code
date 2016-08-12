#这个问题所需要的数据存放在logit数据集中。在这个数据集中有五个变量，其中四个自变量x1、x2、x3、x4，一个响应变量y。
library(mcmc)

data(logit)

#频率学派的处理方法是利用广义线性模型进行参数估计

out<-glm(y~x1+x2+x3+x4, data=logit, family=binomial(), x=T)
summary(out)

#对于Bayesian分析而言，我们假定数据模型(即广义线性模型)与频率学派一致。
#同时假定，五个参数(回归系数)相互独立，并服从均值为0、标准差为2的先验正态分布。

x <- out$x
y <- out$y
lupost <- function(beta, x, y){
	eta <- as.numeric(x %*% beta)
	logp <- ifelse(eta < 0, eta-log1p(exp(eta)), -log1p(exp(-eta)))
	logq <- ifelse(eta < 0, -log1p(exp(eta)), -eta-log1p(exp(-eta)))
	logl <- sum(logp[y == 1]) + sum(logq[y == 0])
	return (logl - sum(beta^2)/8)
}

set.seed(42)
beta.init <- as.numeric(coefficients(out))
out <- metrop(lupost, beta.init, 1000, x = x, y = y)
names(out)