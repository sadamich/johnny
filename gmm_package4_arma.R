### 3 4 AR ARMA
t <-400
set.seed(345)
x5<-arima.sim(n=t,list(ar=c(1.4,-0.6),ma=c(0.6,-0.3)))
x5t<-cbind(x5)
for(i in 1:6) x5t<-cbind(x5t,lag(x5,-i))
x5t<-na.omit(x5t)
g4<-x5t[,1]~x5t[,2]+x5t[,3]
res<-gmm(g4,x5t[,4:7])
summary(res)

res2 <-gmm(g4,x=x5t[,4:7],kernel="Truncated")
coef(res2)

diag(vcov(res2))^.5
plot(res,which=2)
plot(res,which=3)