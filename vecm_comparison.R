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

library("tsDyn")
library(urca)
### Example 7 28 (p.679)                                                   ###
xm728<- read.csv("xm728.csv", header = TRUE)
attach(xm728)
str(xm728)
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


