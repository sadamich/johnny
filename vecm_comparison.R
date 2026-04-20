### Compare with the Example 7 27 Interest and bond rates p.674            ###
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
detach(xm722)
library(tsDyn)
var_data<- data.frame(AAA[25:624],US3MTBIL[25:624])
eq<- VECM(var_data,lag=2, estim="ML")
summary(eq)
panel05<- VECM(
  data = var_data,
  lag = 2,
  r = 1,
  include = "const",
  beta = NULL,
  estim = "ML",
  LRinclude = "both",
  exogen = NULL
)
panel05
coefA(panel05)
                                  ECT
Equation AAA.25.624.      -0.02305473
Equation US3MTBIL.25.624.  0.03823438

coefB(panel05)
                           r1
AAA.25.624.       1.000000000
US3MTBIL.25.624. -1.050296180
const            -1.152990971
trend            -0.002093609
coefPI(panel05)

summary(panel05)
#############
###Model VECM 
#############
Full sample size: 600   End sample size: 597
Number of variables: 2  Number of estimated slope parameters 10
AIC -3202.448   BIC -3154.137   SSR 131.116
Cointegrating vector (estimated by ML):
   AAA.25.624. US3MTBIL.25.624.     const        trend
r1           1        -1.050296 -1.152991 -0.002093609


                          ECT                AAA.25.624. -1   
Equation AAA.25.624.      -0.0231(0.0066)*** 0.5144(0.0479)***
Equation US3MTBIL.25.624. 0.0382(0.0148)*    0.7745(0.1078)***
                          US3MTBIL.25.624. -1 AAA.25.624. -2    
Equation AAA.25.624.      -0.0450(0.0212)*    -0.3313(0.0492)***
Equation US3MTBIL.25.624. 0.1718(0.0478)***   -0.5229(0.1107)***
                          US3MTBIL.25.624. -2
Equation AAA.25.624.      0.0345(0.0214)     
Equation US3MTBIL.25.624. -0.0306(0.0482)    



eq<- ca.jo(var_data, type = c("eigen", "trace"), ecdet = "trend", K = 2,
spec= "longrun", season = NULL, dumvar = NULL)
summary(eq)
###################### 
# Johansen-Procedure # 
###################### 

Test type: maximal eigenvalue statistic (lambda max) , with linear trend in cointegration 

Eigenvalues (lambda):
[1] 7.736274e-02 8.810866e-03 4.737016e-19

Values of teststatistic and critical values of test:

          test 10pct  5pct  1pct
r <= 1 |  5.29 10.49 12.25 16.26
r = 0  | 48.15 16.85 18.96 23.65

Eigenvectors, normalised to first column:
(These are the cointegration relations)

                    AAA.25.624..l2 US3MTBIL.25.624..l2    trend.l2
AAA.25.624..l2         1.000000000         1.000000000  1.00000000
US3MTBIL.25.624..l2   -1.074707188        -0.191661721 -0.46517893
trend.l2              -0.002127598        -0.007228131 -0.02929441

Weights W:
(This is the loading matrix)

                   AAA.25.624..l2 US3MTBIL.25.624..l2      trend.l2
AAA.25.624..d         -0.01348030         -0.00978560 -1.852282e-17
US3MTBIL.25.624..d     0.06043515         -0.01830475  7.238774e-17


panel06<- VECM(
  data = var_data,
  lag = 2,
  r = 1,
  include = "none",
  beta = NULL,
  estim = "ML",
  LRinclude = "const",
  exogen = NULL
)
panel06

coefA(panel06)
                                  ECT
Equation AAA.25.624.      -0.01884191
Equation US3MTBIL.25.624.  0.03382030
coefB(panel06)
                       r1
AAA.25.624.       1.000000
US3MTBIL.25.624. -1.150557
const            -1.278046

coefPI(panel06)

library(urca)
panel05_eigen<- ca.jo(var_data, type = "eigen", ecdet = "trend", K = 2,
spec= "longrun", season = NULL, dumvar = NULL)
summary(panel05_eigen)

cajolst(var_data, trend = TRUE, K = 2, season = NULL)

panel06_eigen<- ca.jo(var_data, type = "eigen", ecdet = "none", K = 2,
spec= "longrun", season = NULL, dumvar = NULL)
summary(panel06_eigen)


eq2<- ca.jo(var_data, type = "eigen", ecdet = "none", K = 2,
spec="longrun", season = NULL, dumvar = NULL)
summary(eq2)
###################### 
# Johansen-Procedure # 
###################### 

Test type: maximal eigenvalue statistic (lambda max) , with linear trend 

Eigenvalues (lambda):
[1] 0.074542828 0.006908705

Values of teststatistic and critical values of test:

          test 10pct  5pct  1pct
r <= 1 |  4.15  6.50  8.18 11.65
r = 0  | 46.33 12.91 14.90 19.19

Eigenvectors, normalised to first column:
(These are the cointegration relations)

                    AAA.25.624..l2 US3MTBIL.25.624..l2
AAA.25.624..l2             1.00000           1.0000000
US3MTBIL.25.624..l2       -1.18375          -0.1225018

Weights W:
(This is the loading matrix)

                   AAA.25.624..l2 US3MTBIL.25.624..l2
AAA.25.624..d         -0.01014892        -0.005955544
US3MTBIL.25.624..d     0.05312947        -0.010730552


eq3<- cajools(eq2, reg.number = NULL)
summary(eq3)

Response AAA.25.624..d :

Call:
lm(formula = AAA.25.624..d ~ constant + AAA.25.624..dl1 + US3MTBIL.25.624..dl1 + 
    AAA.25.624..l2 + US3MTBIL.25.624..l2 - 1, data = data.mat)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.18626 -0.06613 -0.00920  0.07656  1.10823 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)    
constant              0.053418   0.021977   2.431   0.0154 *  
AAA.25.624..dl1       0.417530   0.048985   8.524   <2e-16 ***
US3MTBIL.25.624..dl1 -0.045707   0.021913  -2.086   0.0374 *  
AAA.25.624..l2       -0.016104   0.006324  -2.547   0.0111 *  
US3MTBIL.25.624..l2   0.012743   0.006574   1.939   0.0530 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1987 on 593 degrees of freedom
Multiple R-squared:  0.1578,    Adjusted R-squared:  0.1507 
F-statistic: 22.22 on 5 and 593 DF,  p-value: < 2.2e-16


Response US3MTBIL.25.624..d :

Call:
lm(formula = US3MTBIL.25.624..d ~ constant + AAA.25.624..dl1 + 
    US3MTBIL.25.624..dl1 + AAA.25.624..l2 + US3MTBIL.25.624..l2 - 
    1, data = data.mat)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.7158 -0.1463  0.0015  0.1358  2.5051 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
constant              0.02024    0.04885   0.414  0.67885    
AAA.25.624..dl1       0.70963    0.10889   6.517 1.53e-10 ***
US3MTBIL.25.624..dl1  0.06197    0.04871   1.272  0.20379    
AAA.25.624..l2        0.04240    0.01406   3.016  0.00267 ** 
US3MTBIL.25.624..l2  -0.06158    0.01461  -4.214 2.90e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.4418 on 593 degrees of freedom
Multiple R-squared:  0.1517,    Adjusted R-squared:  0.1446 
F-statistic: 21.21 on 5 and 593 DF,  p-value: < 2.2e-16



### Spread                                                                 ###
spread<- AAA - US3MTBIL
spread<- ts(spread, freq = 12, start = 1948)
### Exhibit 7 32 b (p.677)                                                 ###
plot(spread, main = "Time series", ylab = "Spread")
library(tseries)
### Panel 7 (p.677)                                                        ###
adf.test(spread)
  Augmented Dickey-Fuller Test
data:  spread
Dickey-Fuller = -4.109, Lag order = 8, p-value = 0.01
alternative hypothesis: stationary
Warning message:
In adf.test(spread) : p-value smaller than printed p-value
spread_50<- spread[25:624]
adf.test(spread_50)


library("tsDyn")
library(urca)
### Example 7 28 (p.679)                                                   ###
xm728<- read.csv("xm728.csv", header = TRUE)
attach(xm728)
str(xm728)
detach(xm728)
VAR_panel01<- data.frame(T_3M, T_1Y, T_10Y)

panel02<- ca.jo(VAR_panel01, type = "eigen", ecdet = "none", K = 3,
spec= "longrun", season = NULL, dumvar = NULL)
summary(panel02)

panel03<- VECM(
  data = VAR_panel01,
  lag = 2,
  r = 2,
  include = "none",
  beta = NULL,
  estim = "ML",
  LRinclude = "const",
  exogen = NULL
)
panel03
  ECT1       ECT2     T_3M -1    T_1Y -1  T_10Y -1
Equation T_3M  -0.13859193  0.1245596  0.04849447 0.36277090 0.1560301
Equation T_1Y   0.05258054 -0.0926937 -0.11402554 0.40174110 0.3226561
Equation T_10Y  0.11611018 -0.1154270 -0.08252206 0.07001954 0.3982854
                  T_3M -2    T_1Y -2   T_10Y -2
Equation T_3M  0.01196916 -0.1438947 -0.2217947
Equation T_1Y  0.17077471 -0.2917727 -0.1852074
Equation T_10Y 0.13614063 -0.1347949 -0.1881653
                    
coefA(panel03)
                      ECT1       ECT2
Equation T_3M  -0.13859193  0.1245596
Equation T_1Y   0.05258054 -0.0926937
Equation T_10Y  0.11611018 -0.1154270

coefB(panel03)
              r1            r2
T_3M   1.0000000  5.551115e-17
T_1Y   0.0000000  1.000000e+00
T_10Y -0.8829703 -9.726232e-01
const  0.5747333  5.958906e-01
coefPI(panel03)

summary(panel03)
#############
###Model VECM 
#############
Full sample size: 480   End sample size: 477
Number of variables: 3  Number of estimated slope parameters 24
AIC -4178.604   BIC -4070.248   SSR 221.232
Cointegrating vector (estimated by ML):
           T_3M T_1Y      T_10Y     const
r1 1.000000e+00    0 -0.8829703 0.5747333
r2 5.551115e-17    1 -0.9726232 0.5958906


               ECT1               ECT2                T_3M -1            
Equation T_3M  -0.1386(0.0668)*   0.1246(0.0771)      0.0485(0.1180)     
Equation T_1Y  0.0526(0.0659)     -0.0927(0.0760)     -0.1140(0.1163)    
Equation T_10Y 0.1161(0.0411)**   -0.1154(0.0475)*    -0.0825(0.0726)    
               T_1Y -1            T_10Y -1           T_3M -2           
Equation T_3M  0.3628(0.1636)*    0.1560(0.1436)     0.0120(0.1181)    
Equation T_1Y  0.4017(0.1613)*    0.3227(0.1416)*    0.1708(0.1164)    
Equation T_10Y 0.0700(0.1007)     0.3983(0.0884)***  0.1361(0.0727).   
               T_1Y -2             T_10Y -2           
Equation T_3M  -0.1439(0.1658)     -0.2218(0.1447)    
Equation T_1Y  -0.2918(0.1634).    -0.1852(0.1426)    
Equation T_10Y -0.1348(0.1021)     -0.1882(0.0891)*   
