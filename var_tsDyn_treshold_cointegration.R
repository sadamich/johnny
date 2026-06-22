https://cran.r-project.org/web/packages/tsDyn/vignettes/ThCointOverview.pdf

library(tsDyn)
data(lynx)
grid<-selectSETAR(lynx, m=1, thDelay=0, trim=0.15, criterion="SSR")
print(grid)

set<-setar(lynx,m=1,thDelay=0,th=grid$th)
summary(set)

plot(grid)


selectSETAR(lynx, m=1, thDelay=0, trim=0.15, criterion="SSR", nthresh=2)

selectSETAR(lynx, m=6, thDelay=0, trim=0.15, criterion="AIC", same.lags=TRUE)

data(zeroyld)
tvecm<-TVECM(zeroyld, nthresh=2,lag=1, ngridBeta=60, ngridTh=30, plot=TRUE,trim=0.05, beta=list(int=c(0.7, 1.1)))

data(IIPUs)
set<-setar(IIPUs, m=16, thDelay=5, th=0.23)

Hansen.Test<-setarTest(lynx, m=1, nboot=1000)

sun<-(sqrt(sunspot.year+1)-1)*2
lin<-linear(sun, m=11)
set1<-setar(sun, m=11, th=7.4, thDelay=1, nested=TRUE)
set2<-setar(sun, m=11, th=c(5.3,8),nthresh=2, thDelay=1, nested=TRUE)
matrix(c(AIC(lin),AIC(set1),AIC(set2),BIC(lin),BIC(set1),BIC(set2)),ncol=2,dimnames=list(c("lin","set1", "set2"),c

data(zeroyld)
dat<-zeroyld
testSeo<-TVECM.SeoTest(dat, lag=1, beta=1, nboot=1000)
summary(testSeo)