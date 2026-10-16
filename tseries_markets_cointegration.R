library(tseries)
### DAX 
str(x)
Time-Series [1:50, 1:4] from 46260 to 46309: 26255 26354 26495 NA NA ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "Open" "High" "Low" "Close"
DAX<- data.frame(x)
attach(DAX)
DAX_c<- Close
DAX_c<- na.omit(DAX_c)
plot(DAX_c, type="l")
str(DAX_c)
 num [1:36] 26286 26367 26570 26258 25970 ...
 - attr(*, "na.action")= 'omit' int [1:14] 4 5 11 12 18 19 25 26 32 33 ...

### DJ
DJ<- data.frame(x)
attach(DJ)
DJ_c<- Close
DJ_c<- na.omit(DJ_c)
plot(DJ_c, type = "l")
str(DJ_c)
num [1:35] 53464 53569 53560 53186 52767 ...
 - attr(*, "na.action")= 'omit' int [1:15] 4 5 11 12 13 18 19 25 26 32 ...

### FTSE
FTSE<- data.frame(x)
attach(FTSE)
FTSE_c<- Close
FTSE_c<- na.omit(FTSE_c)
plot(FTSE_c, type="l")
str(FTSE_c)
 num [1:35] 10878 10792 10824 10789 10756 ...
 - attr(*, "na.action")= 'omit' int [1:15] 4 5 6 11 12 18 19 25 26 32 ...
> 
### N225
N225<- data.frame(x)
attach(N225)
N225_c<- Close
N225_c<- na.omit(N225_c)
plot(N225_c, type ="l")
str(N225_c)
num [1:33] 66262 66132 66406 66312 66215 ...
 - attr(*, "na.action")= 'omit' int [1:18] 4 5 11 12 18 19 25 26 27 28 ...

library(tsDyn)
library(urca)
var_data<- data.frame(DAX_c[1:33],DJ_c[1:33],FTSE_c[1:33], N225_c)
eq<- ca.jo(var_data, type = c("trace"), ecdet = "trend", K = 2,
spec= "longrun", season = NULL, dumvar = NULL)
summary(eq)
###################### 
# Johansen-Procedure # 
###################### 

Test type: trace statistic , with linear trend in cointegration 
Eigenvalues (lambda):
[1] 6.593227e-01 5.215807e-01 2.294721e-01 1.839865e-01 1.332837e-17
Values of teststatistic and critical values of test:

          test 10pct  5pct  1pct
r <= 3 |  6.30 10.49 12.25 16.26
r <= 2 | 14.38 22.76 25.32 30.45
r <= 1 | 37.24 39.06 42.44 48.45
r = 0  | 70.62 59.14 62.99 70.05

Eigenvectors, normalised to first column:
(These are the cointegration relations)

                DAX_c.1.33..l2 DJ_c.1.33..l2 FTSE_c.1.33..l2  N225_c.l2
DAX_c.1.33..l2      1.00000000     1.0000000      1.00000000  1.0000000
DJ_c.1.33..l2      -0.10118462    -2.6785421     -0.63975567 -0.4176643
FTSE_c.1.33..l2    -1.33649174    11.3857429    -16.82650353 -3.2617949
N225_c.l2          -0.05308728     0.4117393      0.03229098 -0.2436447
trend.l2           18.22775958   -96.0671944   -162.03497093  9.1636068
                    trend.l2
DAX_c.1.33..l2    1.00000000
DJ_c.1.33..l2     0.30562572
FTSE_c.1.33..l2   0.84266623
N225_c.l2        -0.04425578
trend.l2        115.15454839

Weights W:
(This is the loading matrix)

               DAX_c.1.33..l2 DJ_c.1.33..l2 FTSE_c.1.33..l2    N225_c.l2
DAX_c.1.33..d     -0.88744884  -0.004716114    -0.007049879 -0.009180603
DJ_c.1.33..d      -0.20271634   0.128501126     0.099325640  0.261228345
FTSE_c.1.33..d    -0.09646881  -0.031310617     0.015844214  0.058325274
N225_c.d          -0.50616257   0.213353015    -0.404687767  0.754020873
                    trend.l2
DAX_c.1.33..d   1.861096e-13
DJ_c.1.33..d   -1.394183e-12
FTSE_c.1.33..d  3.020694e-13
N225_c.d       -3.002038e-12

eq2<- ca.jo(var_data, type = c("trace"), ecdet = "none", K = 2,
spec= "longrun", season = NULL, dumvar = NULL)
summary(eq2)
###################### 
# Johansen-Procedure # 
###################### 

Test type: trace statistic , with linear trend 

Eigenvalues (lambda):
[1] 0.5837692 0.4487440 0.1879887 0.1629428

Values of teststatistic and critical values of test:

          test 10pct  5pct  1pct
r <= 3 |  5.51  6.50  8.18 11.65
r <= 2 | 11.97 15.66 17.95 23.52
r <= 1 | 30.43 28.71 31.52 37.22
r = 0  | 57.60 45.23 48.28 55.43

Eigenvectors, normalised to first column:
(These are the cointegration relations)

                DAX_c.1.33..l2 DJ_c.1.33..l2 FTSE_c.1.33..l2  N225_c.l2
DAX_c.1.33..l2      1.00000000     1.0000000       1.0000000  1.0000000
DJ_c.1.33..l2      -0.34777665     0.5821211      -0.4163546 -1.0229847
FTSE_c.1.33..l2    -0.64692866    -8.3646168      -4.3830002  6.5011853
N225_c.l2          -0.01033696    -0.1841891      -0.2011432 -0.9919098

Weights W:
(This is the loading matrix)

               DAX_c.1.33..l2 DJ_c.1.33..l2 FTSE_c.1.33..l2    N225_c.l2
DAX_c.1.33..d     -0.58645225   -0.09743005    -0.006214979 -0.002384443
DJ_c.1.33..d       0.49402821   -0.15833024     0.410843620 -0.022333622
FTSE_c.1.33..d    -0.17178462    0.05502802     0.075776159 -0.001096373
N225_c.d           0.01333618   -0.83938637     0.090352748  0.162650180





eq_vecm<- VECM(var_data,lag=2, estim="ML")
summary(eq_vecm)
#############
###Model VECM 
#############
Full sample size: 33    End sample size: 30
Number of variables: 4  Number of estimated slope parameters 40
AIC 1318.63     BIC 1378.881    SSR 21646925
Cointegrating vector (estimated by ML):
   DAX_c.1.33. DJ_c.1.33. FTSE_c.1.33.      N225_c
r1           1 -0.7647226     2.123027 -0.01845631


                      ECT                 Intercept                
Equation DAX_c.1.33.  -0.1097(0.1213)     747.6792(873.8065)       
Equation DJ_c.1.33.   0.5801(0.3247).     -4297.7697(2339.3012).   
Equation FTSE_c.1.33. -0.0232(0.0670)     150.6919(483.0243)       
Equation N225_c       2.9403(0.9706)**    -21122.3168(6993.8438)** 
                      DAX_c.1.33. -1      DJ_c.1.33. -1      
Equation DAX_c.1.33.  -0.1782(0.1942)     0.0896(0.0917)     
Equation DJ_c.1.33.   -0.5616(0.5200)     0.2341(0.2456)     
Equation FTSE_c.1.33. -0.1390(0.1074)     -0.0080(0.0507)    
Equation N225_c       -0.2399(1.5547)     1.5922(0.7341)*    
                      FTSE_c.1.33. -1     N225_c -1          
Equation DAX_c.1.33.  2.1656(0.4003)***   0.0766(0.0267)**   
Equation DJ_c.1.33.   -1.5710(1.0715)     -0.1395(0.0714).   
Equation FTSE_c.1.33. -0.3180(0.2213)     -0.0066(0.0147)    
Equation N225_c       -6.1757(3.2036).    -0.1857(0.2135)    
                      DAX_c.1.33. -2      DJ_c.1.33. -2      
Equation DAX_c.1.33.  -0.1621(0.1260)     -0.0536(0.0972)    
Equation DJ_c.1.33.   -0.7461(0.3373)*    0.0564(0.2603)     
Equation FTSE_c.1.33. -0.1136(0.0696)     0.0430(0.0537)     
Equation N225_c       0.3854(1.0084)      1.8062(0.7782)*    
                      FTSE_c.1.33. -2     N225_c -2          
Equation DAX_c.1.33.  0.6027(0.5180)      0.0194(0.0299)     
Equation DJ_c.1.33.   -1.5944(1.3867)     -0.0814(0.0800)    
Equation FTSE_c.1.33. -0.0443(0.2863)     -0.0234(0.0165)    
Equation N225_c       -6.2847(4.1458)     -0.2077(0.2390)    
> 


