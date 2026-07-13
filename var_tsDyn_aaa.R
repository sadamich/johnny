### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

library(tsDyn)
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
detach(xm722)
x<- ts(AAA, freq=12, start=1948)
plot(x, main="Time series", ylab="AAA")
### 4 1 1 The one threshold case
grid<-selectSETAR(AAA, m=1, thDelay=0, trim=0.15, criterion="SSR")
### Using maximum autoregressive order for low regime: mL = 1 
### Using maximum autoregressive order for high regime: mH = 1 
### Searching on 290 possible threshold values within regimes with sufficient ( 15% ) number of observations
### Searching on 290 combinations of thresholds (290) and thDelay (1) 
###  1 T: Trim not respected:  0.8507223 0.1492777 from th: 9.57> 
set<-setar(AAA,m=1,thDelay=0,th=grid$th)
### Warning message:
### Possible unit root in the low  regime. Roots are: 0.9891 
summary(set)
Non linear autoregressive model
SETAR model ( 2 regimes)
Coefficients:
Low regime:
    const.L      phiL.1 
-0.02780505  1.01099171 
High regime:
   const.H     phiH.1 
0.06766316 0.99236629 
Threshold:
-Variable: Z(t) = + (1) X(t)
-Value: 7.14 (fixed)
Proportion of points in low regime: 47.83%       High regime: 52.17% 
Residuals:
       Min         1Q     Median         3Q        Max 
-1.1301040 -0.0640743 -0.0034114  0.0686355  1.3069947 
Fit: residuals variance = 0.04418,  AIC = -1939, MAPE = 1.614%
Coefficient(s):
          Estimate  Std. Error  t value Pr(>|t|)    
const.L -0.0278050   0.0400836  -0.6937   0.4881    
phiL.1   1.0109917   0.0088151 114.6885   <2e-16 ***
const.H  0.0676632   0.0556380   1.2161   0.2244    
phiH.1   0.9923663   0.0058167 170.6061   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Threshold
Variable: Z(t) = + (1) X(t) 
Value: 7.14 (fixed)

### 4 1 2 The two threshold case
selectSETAR(AAA, m=1, thDelay=0, trim=0.15, criterion="SSR", nthresh=2)
Using maximum autoregressive order for low regime: mL = 1 
Using maximum autoregressive order for high regime: mH = 1 
Using maximum autoregressive order for middle regime: mM = 1 
Searching on 290 possible threshold values within regimes with sufficient ( 15% ) number of observations
Searching on 290 combinations of thresholds (290) and thDelay (1) 

 1 T: Trim not respected:  0.8507223 0.1492777 from th: 9.57Result of the one threshold search:
 -Thresh:  7.14         -Delay:  0      - SSR 27.56687 
Second best: 9.3 (conditionnal on th= 7.14 and Delay= 0 )        SSR/AIC: 27.46431
Second best: 7.53 (conditionnal on th= 9.3 and Delay= 0 )        SSR/AIC: 27.42975
Results of the grid search for 1 threshold
         Conditional on m=  1 
  thDelay   th      SSR
1       0 7.14 27.56687
2       0 7.10 27.56707
3       0 6.99 27.57016
4       0 7.08 27.57203
5       0 7.20 27.57205

Results of the grid search for 2 thresholds
         Conditional on thDelay =  0  and m = 1 
   th1 th2      SSR
1 7.53 9.3 27.42975

Overall best results:
 thDelay      th1      th2      SSR 
 0.00000  7.53000  9.30000 27.42975 
With lags:
        -ML: 1 
        -MM: 1 
        -MH: 1 

### 4 1 3 Distribution of the estimator 
### Estimation of the number of lags 
selectSETAR(AAA, m=6, thDelay=0, trim=0.15, criterion="AIC", same.lags=TRUE)
ive order for low regime: mL = 6 
Using maximum autoregressive order for high regime: mH = 6 
Searching on 288 possible threshold values within regimes with sufficient ( 15% ) number of observations
Searching on  1728  combinations of thresholds ( 288 ), thDelay ( 1 ) and m ( 6 ) 
Results of the grid search for 1 threshold
   thDelay m   th       AIC
1        0 6 9.10 -2082.969
2        0 6 9.09 -2082.935
3        0 6 9.22 -2082.633
4        0 6 9.20 -2082.632
5        0 6 9.04 -2082.580
6        0 6 9.16 -2082.529
7        0 6 9.13 -2082.468
8        0 6 9.05 -2082.465
9        0 6 9.23 -2082.395
10       0 6 9.24 -2081.940

set<-setar(AAA, m=3, thDelay=1, th=0.3)???
summary(set)

### 4 2 Estimation and inference in the TVECM representation
var_data<- data.frame(AAA[25:624],US3MTBIL[25:624])
tvecm<-TVECM(var_data, nthresh=2,lag=1, ngridBeta=60, ngridTh=30, plot=TRUE,trim=0.05, beta=list(int=c(0.7, 1.1)))


### 5 Testing 
### 5.1 The problem of the unidentified parameter
### 5.2 Cointegration vs. threshold cointegration tests
### 5.2.1 Test based on the long-run relationship

data(IIPUs)
str(IIPUs)
set<-setar(AAA, m=16, thDelay=5, th=0.23)
Hansen.Test<-setarTest(AAA, m=1, nboot=1000)
### Criterion based approaches ????
sun<-(sqrt(AAA+1)-1)*2
lin<-linear(sun, m=11)
set1<-setar(sun, m=11, th=7.4, thDelay=1, nested=TRUE)
set2<-setar(sun, m=11, th=c(5.3,8),nthresh=2, thDelay=1, nested=TRUE)
matrix(c(AIC(lin),AIC(set1),AIC(set2),BIC(lin),BIC(set1),BIC(set2)),ncol=2,dimnames=list(c("lin","set1", "set2"),c

var_data<- data.frame(AAA[25:624],US3MTBIL[25:624])
testSeo<- TVECM.SeoTest(var_data, lag=1, beta=1, nboot=100)???
summary(testSeo)