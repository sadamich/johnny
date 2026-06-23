https://cran.r-project.org/web/packages/tsDyn/vignettes/ThCointOverview.pdf

library(tsDyn)
data(lynx)
str(lynx)
x<- ts(lynx, freq=1, start=1821)
plot(x, main="Time series", ylab="lynx")
grid<-selectSETAR(lynx, m=1, thDelay=0, trim=0.15, criterion="SSR")
### Using maximum autoregressive order for low regime: mL = 1 
### Using maximum autoregressive order for high regime: mH = 1 
### Searching on 75 possible threshold values within regimes with sufficient ( 15% ) number of observations
### Searching on 75 combinations of thresholds (75) and thDelay (1)
print(grid)
### Results of the grid search for 1 threshold
         Conditional on m=  1 
       thDelay   th       SSR
   1        0 1388 123102676
   2        0 1307 123951941
   3        0 1475 124388924
   4        0 1676 124516444


set<-setar(lynx,m=1,thDelay=0,th=grid$th)
summary(set)
### Non linear autoregressive model
### SETAR model ( 2 regimes)
    Coefficients:
    Low regime:
        const.L      phiL.1 
    -150.298119    1.997857 

    High regime:
        const.H      phiH.1 
    984.5047382   0.5595309 

### Threshold:
-Variable: Z(t) = + (1) X(t)
-Value: 1388 (fixed)
Proportion of points in low regime: 59.29%       High regime: 40.71% 
Residuals:
      Min        1Q    Median        3Q       Max 
-2677.749  -471.918    90.273   327.865  4067.721 
Fit:
residuals variance = 1079848,  AIC = 1592, MAPE = 119.8%
Coefficient(s):

          Estimate  Std. Error  t value  Pr(>|t|)    
const.L -150.29812   220.45996  -0.6817   0.49683    
phiL.1     1.99786     0.39437   5.0659 1.651e-06 ***
const.H  984.50474   385.10377   2.5565   0.01194 *  
phiH.1     0.55953     0.11439   4.8915 3.442e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Threshold
Variable: Z(t) = + (1) X(t) 
Value: 1388 (fixed)

plot(grid)


selectSETAR(lynx, m=1, thDelay=0, trim=0.15, criterion="SSR", nthresh=2)
selectSETAR(lynx, m=6, thDelay=0, trim=0.15, criterion="AIC", same.lags=TRUE)

### VECM 
data(zeroyld)
str(zeroyld)
tvecm<-TVECM(zeroyld, nthresh=2,lag=1, ngridBeta=60, ngridTh=30, plot=TRUE,trim=0.05, beta=list(int=c(0.7, 1.1)))

data(IIPUs)
str(IIPUs)
Time-Series [1:453] from 1960 to 1998: -8.83 -8.06 -6.55 -3.72 -2.08 
y<- ts(IIPUs, freq=12, start = 1960)
plot(y, main="Time series", ylab="IIPUs")
set<-setar(IIPUs, m=16, thDelay=5, th=0.23)
summary(set)
### Test 
Hansen.Test<-setarTest(lynx, m=1, nboot=1000)
summary(Hansen.Test)
Test of linearity against setar(2) and setar(3)
         Test  Pval
1vs2 20.05733 0.004
1vs3 42.81466 0.001
Critical values:
          0.9     0.95    0.975     0.99
1vs2 10.31477 12.73625 14.48475 17.73924
1vs3 20.87515 23.96439 28.08779 32.64306
SSR of original series:
               SSR
AR       137160013
SETAR(2) 116484232
SETAR(3)  99471266
Threshold of original series:
          th1  th2
SETAR(2) 3465   NA
SETAR(3) 1676 3465
Number of bootstrap replications:  1000 



sun<-(sqrt(sunspot.year+1)-1)*2
lin<-linear(sun, m=11)
set1<-setar(sun, m=11, th=7.4, thDelay=1, nested=TRUE)
set2<-setar(sun, m=11, th=c(5.3,8),nthresh=2, thDelay=1, nested=TRUE)
matrix(c(AIC(lin),AIC(set1),AIC(set2),BIC(lin),BIC(set1),BIC(set2)),ncol=2,dimnames=list(c("lin","set1", "set2"),c

data(zeroyld)
dat<-zeroyld
testSeo<- TVECM.SeoTest(dat, lag=1, beta=1, nboot=1000)
summary(testSeo)
