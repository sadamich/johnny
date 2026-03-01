### Compare with the Example 7 27 Interest and bond rates p.674            ###
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
library(tsDyn)
var_data<- data.frame(AAA,US3MTBIL)
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
                          ECT    AAA -1 US3MTBIL -1     AAA -2 US3MTBIL -2
Equation AAA      -0.02307544 0.5142587 -0.04493965 -0.3313776  0.03443276
Equation US3MTBIL  0.03794585 0.7739789  0.17161767 -0.5223374 -0.03099130

coefA(panel05)
coefB(panel05)
coefPI(panel05)

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
                          ECT    AAA -1 US3MTBIL -1     AAA -2 US3MTBIL -2
Equation AAA      -0.01873009 0.5137194  -0.0436500 -0.3332185  0.03641944
Equation US3MTBIL  0.03391977 0.7776206   0.1699271 -0.5166337 -0.03244040
coefA(panel06)
coefB(panel06)
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
coefB(panel03)
 r1            r2
T_3M   1.0000000  5.551115e-17
T_1Y   0.0000000  1.000000e+00
T_10Y -0.8829703 -9.726232e-01
const  0.5747333  5.958906e-01
coefPI(panel03)


