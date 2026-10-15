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
var_data<- data.frame(DAX_c[1:33],DJ_c[1:33],FTSE_c[1:33], N225_c)
eq<- VECM(var_data,lag=2, estim="ML")
summary(eq)
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


