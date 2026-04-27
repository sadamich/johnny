https://cran.r-project.org/web/packages/plm/refman/plm.html#plm

library(plm)

data("Produc", package = "plm")
str(Produc)
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
p <- plm(inv ~ value + capital,
         data = Grunfeld, model = "pooling")
summary(p)

wi <- plm(inv ~ value + capital,
          data = Grunfeld, model = "within", effect = "twoways")
summary(wi)
swar <- plm(inv ~ value + capital,
            data = Grunfeld, model = "random", effect = "twoways")
summary(swar)
amemiya <- plm(inv ~ value + capital,
               data = Grunfeld, model = "random", random.method = "amemiya",
               effect = "twoways")
summary(amemiya)
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


## nested random effect model
# replicate Baltagi/Song/Jung (2001), p. 378 (table 6), columns SA, WH
# == Baltagi (2013), pp. 204-205
data("Produc", package = "plm")
pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
form <- log(gsp) ~ log(pc) + log(emp) + log(hwy) + log(water) + log(util) + unemp
summary(plm(form, data = pProduc, model = "random", effect = "nested"))
summary(plm(form, data = pProduc, model = "random", effect = "nested",
            random.method = "walhus"))

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

