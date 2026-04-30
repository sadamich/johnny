https://cran.r-project.org/web/packages/plm/refman/plm.html#plm
library(plm)

data("Produc", package = "plm")
str(Produc)
attach(Produc)
'data.frame':   816 obs. of  11 variables:
 $ state : Factor w/ 48 levels "ALABAMA","ARIZONA",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ year  : int  1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 ...
 $ region: Factor w/ 9 levels "1","2","3","4",..: 6 6 6 6 6 6 6 6 6 6 ...
 $ pcap  : num  15033 15502 15972 16406 16763 ...
 $ hwy   : num  7326 7526 7765 7908 8026 ...
 $ water : num  1656 1721 1765 1742 1735 ...
 $ util  : num  6051 6255 6442 6756 7002 ...
 $ pc    : num  35794 37300 38670 40084 42057 ...
 $ gsp   : int  28418 29375 31303 33430 33749 33604 35764 37463 39964 40979 ...
 $ emp   : num  1010 1022 1072 1136 1170 ...
 $ unemp : num  4.7 5.2 4.7 3.9 5.5 7.7 6.8 7.4 6.3 7.1 ...


index = c("state","year")
48 states (units) * 17 years (observations)
m= 48               n= 17
48*17 = 816
str(pcap)
num [1:816]
str(hwy)
num [1:816] 
str(water)
num [1:816]
str(util)
num [1:816]
str(pc)
num [1:816] 
str(gsp)
int [1:816]
str(emp)
num [1:816] 
str(unemp)
num [1:816] 
str(state)
Factor w/ 48 levels "ALABAMA","ARIZONA",..: 1 1
str(year)
int [1:816]
str(region)
Factor w/ 9 levels "1","2",

zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(zz)
Oneway (individual) effect Within Model
Call:
plm(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    data = Produc, index = c("state", "year"))

Balanced Panel: n = 48, T = 17, N = 816
Residuals:
     Min.   1st Qu.    Median   3rd Qu.      Max. 
-0.120456 -0.023741 -0.002041  0.018144  0.174718 

Coefficients:
             Estimate  Std. Error t-value  Pr(>|t|)    
log(pcap) -0.02614965  0.02900158 -0.9017    0.3675    
log(pc)    0.29200693  0.02511967 11.6246 < 2.2e-16 ***
log(emp)   0.76815947  0.03009174 25.5273 < 2.2e-16 ***
unemp     -0.00529774  0.00098873 -5.3582 1.114e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares:    18.941
Residual Sum of Squares: 1.1112
R-Squared:      0.94134
Adj. R-Squared: 0.93742
F-statistic: 3064.81 on 4 and 764 DF, p-value: < 2.22e-16
# replicates some results from Baltagi (2013), table 3.1

data("Grunfeld", package = "plm")
str(Grunfeld)
'data.frame':   200 obs. of  5 variables:
 $ firm   : int  1 1 1 1 1 1 1 1 1 1 ...
 $ year   : int  1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 ...
 $ inv    : num  318 392 411 258 331 ...
 $ value  : num  3078 4662 5387 2792 4313 ...
 $ capital: num  2.8 52.6 156.9 209.2 203.4
attach(Grunfeld)
str(firm)            firms 10 
int [1:200] 1 
str(inv)
num [1:200]
str(value)
num [1:200]
str(capital)
 num [1:200]
str(year)             year: 1935-1954
int [1:200]

10 firms * 20 years 
m= 10      n= 20
       
p <- plm(inv ~ value + capital,
         data = Grunfeld, model = "pooling")
summary(p)
Pooling Model

Call:
plm(formula = inv ~ value + capital, data = Grunfeld, model = "pooling")
Balanced Panel: n = 10, T = 20, N = 200
Residuals:
     Min.   1st Qu.    Median   3rd Qu.      Max. 
-291.6757  -30.0137    5.3033   34.8293  369.4464 

Coefficients:
               Estimate  Std. Error t-value  Pr(>|t|)    
(Intercept) -42.7143694   9.5116760 -4.4907 1.207e-05 ***
value         0.1155622   0.0058357 19.8026 < 2.2e-16 ***
capital       0.2306785   0.0254758  9.0548 < 2.2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares:    9359900
Residual Sum of Squares: 1755900
R-Squared:      0.81241
Adj. R-Squared: 0.8105
F-statistic: 426.576 on 2 and 197 DF, p-value: < 2.22e-16


wi <- plm(inv ~ value + capital,
          data = Grunfeld, model = "within", effect = "twoways")
summary(wi)
Twoways effects Within Model

Call:
plm(formula = inv ~ value + capital, data = Grunfeld, effect = "twoways", 
    model = "within")
Balanced Panel: n = 10, T = 20, N = 200
Residuals:
     Min.   1st Qu.    Median   3rd Qu.      Max. 
-162.6094  -19.4710   -1.2669   19.1277  211.8420 
Coefficients:
        Estimate Std. Error t-value  Pr(>|t|)    
value   0.117716   0.013751  8.5604 6.653e-15 ***
capital 0.357916   0.022719 15.7540 < 2.2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares:    1615600
Residual Sum of Squares: 452150
R-Squared:      0.72015
Adj. R-Squared: 0.67047
F-statistic: 217.442 on 2 and 169 DF, p-value: < 2.22e-16


swar <- plm(inv ~ value + capital,
            data = Grunfeld, model = "random", effect = "twoways")
summary(swar)
Twoways effects Random Effect Model 
   (Swamy-Arora's transformation)

Call:
plm(formula = inv ~ value + capital, data = Grunfeld, effect = "twoways", 
    model = "random")

Balanced Panel: n = 10, T = 20, N = 200
Effects:
                  var std.dev share
idiosyncratic 2675.43   51.72 0.274
individual    7095.25   84.23 0.726
time             0.00    0.00 0.000
theta: 0.864 (id) 0 (time) 0 (total)
Residuals:
     Min.   1st Qu.    Median   3rd Qu.      Max. 
-177.1700  -19.7576    4.6048   19.4676  252.7596 
Coefficients:
              Estimate Std. Error z-value Pr(>|z|)    
(Intercept) -57.865377  29.393359 -1.9687  0.04899 *  
value         0.109790   0.010528 10.4285  < 2e-16 ***
capital       0.308190   0.017171 17.9483  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares:    2376000
Residual Sum of Squares: 547910
R-Squared:      0.7694
Adj. R-Squared: 0.76706
Chisq: 657.295 on 2 DF, p-value: < 2.22e-16


amemiya <- plm(inv ~ value + capital,
               data = Grunfeld, model = "random", random.method = "amemiya",
               effect = "twoways")
summary(amemiya)
Twoways effects Random Effect Model 
   (Amemiya's transformation)

Call:
plm(formula = inv ~ value + capital, data = Grunfeld, effect = "twoways", 
    model = "random", random.method = "amemiya")

Balanced Panel: n = 10, T = 20, N = 200
Effects:
                  var std.dev share
idiosyncratic 2644.13   51.42 0.256
individual    7452.02   86.33 0.721
time           243.78   15.61 0.024
theta: 0.868 (id) 0.2787 (time) 0.2776 (total)
Residuals:
     Min.   1st Qu.    Median   3rd Qu.      Max. 
-176.9062  -18.0431    3.2697   17.1719  234.1735 
Coefficients:
              Estimate Std. Error z-value Pr(>|z|)    
(Intercept) -63.767791  29.851537 -2.1362  0.03267 *  
value         0.111386   0.010909 10.2102  < 2e-16 ***
capital       0.323321   0.018772 17.2232  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares:    2066800
Residual Sum of Squares: 518200
R-Squared:      0.74927
Adj. R-Squared: 0.74673
Chisq: 588.717 on 2 DF, p-value: < 2.22e-16


walhus <- plm(inv ~ value + capital,
              data = Grunfeld, model = "random", random.method = "walhus",
              effect = "twoways")
summary(walhus)
# summary and summary with a furnished vcov (passed as matrix, 
# as function, and as function with additional argument)
summary(wi)
summary(wi, vcov = vcovHC(wi))
summary(wi, vcov = vcovHC)
summary(wi, vcov = function(x) vcovHC(x, method = "white2"))
Twoways effects Random Effect Model 
   (Wallace-Hussain's transformation)

Call:
plm(formula = inv ~ value + capital, data = Grunfeld, effect = "twoways", 
    model = "random", random.method = "walhus")

Balanced Panel: n = 10, T = 20, N = 200
Effects:
                  var std.dev share
idiosyncratic 3188.06   56.46 0.359
individual    5685.23   75.40 0.641
time             0.00    0.00 0.000
theta: 0.8349 (id) 0 (time) 0 (total)
Residuals:
     Min.   1st Qu.    Median   3rd Qu.      Max. 
-181.7595  -22.3727    5.9119   18.7001  254.1129 
Coefficients:
              Estimate Std. Error z-value Pr(>|z|)    
(Intercept) -57.522213  25.012301 -2.2998  0.02146 *  
value         0.109703   0.010147 10.8113  < 2e-16 ***
capital       0.307286   0.017283 17.7795  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares:    2438400
Residual Sum of Squares: 559450
R-Squared:      0.77057
Adj. R-Squared: 0.76824
Chisq: 661.637 on 2 DF, p-value: < 2.22e-16


## nested random effect model
# replicate Baltagi/Song/Jung (2001), p. 378 (table 6), columns SA, WH
# == Baltagi (2013), pp. 204-205
data("Produc", package = "plm")
pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
form <- log(gsp) ~ log(pc) + log(emp) + log(hwy) + log(water) + log(util) + unemp
summary(plm(form, data = pProduc, model = "random", effect = "nested"))
Nested effects Random Effect Model 
   (Swamy-Arora's transformation)

Call:
plm(formula = form, data = pProduc, effect = "nested", model = "random")

Balanced Panel: n = 48, T = 17, N = 816

Effects:
                   var  std.dev share
idiosyncratic 0.001352 0.036765 0.191
individual    0.004278 0.065410 0.604
group         0.001455 0.038148 0.205
theta:
            Min.   1st Qu.     Median       Mean    3rd Qu.       Max.
id    0.86492676 0.8649268 0.86492676 0.86492676 0.86492676 0.86492676
group 0.03960556 0.0466931 0.05713605 0.05577645 0.06458029 0.06458029
Residuals:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-1.06e-01 -2.48e-02 -1.82e-03 -5.43e-05  1.98e-02  1.83e-01 
Coefficients:
               Estimate  Std. Error z-value  Pr(>|z|)    
(Intercept)  2.08921088  0.14570204 14.3389 < 2.2e-16 ***
log(pc)      0.27412419  0.02054440 13.3430 < 2.2e-16 ***
log(emp)     0.73983766  0.02575046 28.7311 < 2.2e-16 ***
log(hwy)     0.07273624  0.02202509  3.3024 0.0009585 ***
log(water)   0.07645327  0.01385767  5.5170 3.448e-08 ***
log(util)   -0.09437398  0.01677289 -5.6266 1.838e-08 ***
unemp       -0.00616304  0.00090331 -6.8227 8.933e-12 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares:    43.035
Residual Sum of Squares: 1.1245
R-Squared:      0.97387
Adj. R-Squared: 0.97368
Chisq: 20213.5 on 6 DF, p-value: < 2.22e-16


summary(plm(form, data = pProduc, model = "random", effect = "nested",
            random.method = "walhus"))
Nested effects Random Effect Model 
   (Wallace-Hussain's transformation)

Call:
plm(formula = form, data = pProduc, effect = "nested", model = "random", 
    random.method = "walhus")
Balanced Panel: n = 48, T = 17, N = 816
Effects:
                   var  std.dev share
idiosyncratic 0.001415 0.037617 0.163
individual    0.004507 0.067131 0.520
group         0.002744 0.052387 0.317
theta:
            Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
id    0.86533240 0.86533240 0.86533240 0.86533240 0.86533240 0.86533240
group 0.05409908 0.06154491 0.07179372 0.07023704 0.07867007 0.07867007
Residuals:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-1.05e-01 -2.47e-02 -1.88e-03 -5.57e-05  1.99e-02  1.82e-01 
Coefficients:
               Estimate  Std. Error z-value  Pr(>|z|)    
(Intercept)  2.08165186  0.15034855 13.8455 < 2.2e-16 ***
log(pc)      0.27256322  0.02093384 13.0202 < 2.2e-16 ***
log(emp)     0.74164483  0.02607167 28.4464 < 2.2e-16 ***
log(hwy)     0.07493204  0.02234932  3.3528 0.0008001 ***
log(water)   0.07639159  0.01386702  5.5089 3.611e-08 ***
log(util)   -0.09523031  0.01677247 -5.6778 1.365e-08 ***
unemp       -0.00614840  0.00090786 -6.7724 1.267e-11 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares:    40.423
Residual Sum of Squares: 1.1195
R-Squared:      0.97231
Adj. R-Squared: 0.9721
Chisq: 19579.7 on 6 DF, p-value: < 2.22e-16

## Instrumental variable estimations
# replicate Baltagi (2013/2021), p. 133/162, table 7.1
data("Crime", package = "plm")
FE2SLS <- plm(lcrmrte ~ lprbarr + lpolpc + lprbconv + lprbpris + lavgsen +
                ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed +
                lwsta + lwloc + lpctymle + lpctmin + region + smsa + factor(year)
              | . - lprbarr - lpolpc + ltaxpc + lmix,
              data = Crime, model = "within")
G2SLS <- update(FE2SLS, model = "random", inst.method = "bvk")
EC2SLS <- update(G2SLS, model = "random", inst.method = "baltagi")

## Hausman-Taylor estimator and Amemiya-MaCurdy estimator
# replicate Baltagi (2005, 2013), table 7.4; Baltagi (2021), table 7.5
data("Wages", package = "plm")
ht <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
              bluecol + ind + union + sex + black + ed |
              bluecol + south + smsa + ind + sex + black |
              wks + married + union + exp + I(exp ^ 2), 
          data = Wages, index = 595,
          random.method = "ht", model = "random", inst.method = "baltagi")
summary(ht)

am <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
              bluecol + ind + union + sex + black + ed |
              bluecol + south + smsa + ind + sex + black |
              wks + married + union + exp + I(exp ^ 2), 
          data = Wages, index = 595,
          random.method = "ht", model = "random", inst.method = "am")
summary(am)

