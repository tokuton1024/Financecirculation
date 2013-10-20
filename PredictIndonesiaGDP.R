#ミラーサイト呼び出し
chooseCRANmirror()

library("urca")

#GDP統計の輸出などEXへのARIMAモデルの当てはめ
xdata = read.csv("data/TryArima/IndonesiaGDP.csv",header=TRUE,sep=";")
xdata2 <- xdata[1:29,]
y = ts(log(xdata2$GDP),start=c(1980,1),frequency=1)
(suitei<-summary(ur.df(y,type="trend",lag=2)))
(suitei2<-summary(ur.df(y,type="drift",lag=2)))
(suitei3<-summary(ur.df(y,type="none",lag=2)))
y = diff(y)
(suitei<-summary(ur.df(y,type="trend",lag=2)))
(suitei2<-summary(ur.df(y,type="drift",lag=2)))
(suitei3<-summary(ur.df(y,type="none",lag=2)))
(arima213=arima(y,order=c(2,2,3),transform.pars=FALSE))
arima.pred=predict(arima213,n.ahead=5)
yhat=arima.pred$pred
sig = arima.pred$se
EXhat=exp(yhat)
EX=exp(y)
EXL=exp(yhat)
EXU=exp(yhat+2*sig)
ttl="Result"
xl=c(1980,2013)
yl=c(min(EX,EXL),max(EX,EXU))
plot(EX,type="l",main=ttl,xlim=xl,ylim=yl)
lines(EXhat,lty=1,col=2)
lines(EXL,lty=2,col=4)
lines(EXU,lty=2,col=4)
