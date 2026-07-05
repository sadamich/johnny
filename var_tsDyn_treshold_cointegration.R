https://cran.r-project.org/web/packages/tsDyn/vignettes/ThCointOverview.pdf
library(tsDyn)
### The one threshold case
data(lynx)
str(lynx)
Time-Series [1:114] from 1821 to 1934: 269 321 585 871 147
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

### a sharp U-shaped grid search
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

### The result of the grid search                                         ###
plot(grid)

### The two threshold case
selectSETAR(lynx, m=1, thDelay=0, trim=0.15, criterion="SSR", nthresh=2)
Using maximum autoregressive order for low regime: mL = 1 
Using maximum autoregressive order for high regime: mH = 1 
Using maximum autoregressive order for middle regime: mM = 1 
Searching on 75 possible threshold values within regimes with sufficient ( 15% ) number of observations
Searching on 75 combinations of thresholds (75) and thDelay (1) 
Result of the one threshold search:
 -Thresh:  1388         -Delay:  0      - SSR 123102676 
Second best: 2577 (conditionnal on th= 1388 and Delay= 0 )       SSR/AIC: 114452658
Second best: 1000 (conditionnal on th= 2577 and Delay= 0 )       SSR/AIC: 113310032
Results of the grid search for 1 threshold
         Conditional on m=  1 
  thDelay   th       SSR
1       0 1388 123102676
2       0 1307 123951941
3       0 1475 124388924
4       0 1676 124516444
5       0 1638 124557228
Results of the grid search for 2 thresholds
         Conditional on thDelay =  0  and m = 1 
   th1  th2       SSR
1 1000 2577 113310032
Overall best results:
  thDelay       th1       th2       SSR 
        0      1000      2577 113310032 
With lags:
        -ML: 1 
        -MM: 1 
        -MH: 1 

### Distribution of the estimator 
### Estimation of the number of lags 
selectSETAR(lynx, m=6, thDelay=0, trim=0.15, criterion="AIC", same.lags=TRUE)
Using maximum autoregressive order for low regime: mL = 6 
Using maximum autoregressive order for high regime: mH = 6 
Searching on 70 possible threshold values within regimes with sufficient ( 15% ) number of observations
Searching on  420  combinations of thresholds ( 70 ), thDelay ( 1 ) and m ( 6 ) 
Results of the grid search for 1 threshold
   thDelay m   th      AIC
1        0 2 1388 1528.278
2        0 2 1307 1528.471
3        0 2  808 1529.596
4        0 2 1000 1529.765
5        0 2 1033 1529.830
6        0 2 1292 1529.882
7        0 2 1132 1529.940
8        0 2  957 1530.249
9        0 2  784 1530.425
10       0 2  758 1530.807

### Estimation and inference in the TVECM representation
data(zeroyld)
str(zeroyld)
'data.frame':   482 obs. of  2 variables:
 $ short.run: num  2.18 2.25 2.31 2.4 2.56 ...
 $ long.run : num  1.57 1.54 1.76 1.77 1.81
tvecm<-TVECM(zeroyld, nthresh=2,lag=1, ngridBeta=60, ngridTh=30, plot=TRUE,trim=0.05, beta=list(int=c(0.7, 1.1)))

### Testing
### Cointegration vs. threshold cointegration tests
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

### Criterion based approaches 
sun<-(sqrt(sunspot.year+1)-1)*2
lin<-linear(sun, m=11)
set1<-setar(sun, m=11, th=7.4, thDelay=1, nested=TRUE)
set2<-setar(sun, m=11, th=c(5.3,8),nthresh=2, thDelay=1, nested=TRUE)
matrix(c(AIC(lin),AIC(set1),AIC(set2),BIC(lin),BIC(set1),BIC(set2)),ncol=2,dimnames=list(c("lin","set1", "set2"),c

### Test based on the TVECM representation
data(zeroyld)
dat<-zeroyld
testSeo<- TVECM.SeoTest(dat, lag=1, beta=1, nboot=1000)
summary(testSeo)
