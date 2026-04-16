### VECM: Package urca###
### Source: https://cran.r-project.org/web/packages/urca/refman/urca.html#ca.jo ###
### Example 1 ###
library(urca)
data(denmark)
### period	Time index from 1974:Q1 until 1987:Q3 ###
### LRM: Logarithm of real money, M2              ###
### LRY: Logarithm of real income                 ###
### LPY: Logarithm of price deflator              ###
### IBO: Bond rate                                ###
### IDE: Bank deposit rate                        ###
head(denmark)
sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
str(sjd)
sjd.vecm <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
summary(sjd.vecm)

###################### 
# Johansen-Procedure # 
###################### 
Test type: maximal eigenvalue statistic (lambda max) , without linear trend and constant in cointegration 
Eigenvalues (lambda):
[1] 4.331654e-01 1.775836e-01 1.127905e-01 4.341130e-02 6.668049e-16
Values of teststatistic and critical values of test:

          test 10pct  5pct  1pct
r <= 3 |  2.35  7.52  9.24 12.97
r <= 2 |  6.34 13.75 15.67 20.20
r <= 1 | 10.36 19.77 22.00 26.81
r = 0  | 30.09 25.56 28.14 33.24

Eigenvectors, normalised to first column:
(These are the cointegration relations)

            LRM.l2     LRY.l2     IBO.l2     IDE.l2   constant
LRM.l2    1.000000  1.0000000  1.0000000   1.000000  1.0000000
LRY.l2   -1.032949 -1.3681031 -3.2266580  -1.883625 -0.6336946
IBO.l2    5.206919  0.2429825  0.5382847  24.399487  1.6965828
IDE.l2   -4.215879  6.8411103 -5.6473903 -14.298037 -1.8951589
constant -6.059932 -4.2708474  7.8963696  -2.263224 -8.0330127

Weights W:
(This is the loading matrix)

           LRM.l2      LRY.l2       IBO.l2        IDE.l2      constant
LRM.d -0.21295494 -0.00481498  0.035011128  2.028908e-03  2.000335e-13
LRY.d  0.11502204  0.01975028  0.049938460  1.108654e-03 -2.479945e-13
IBO.d  0.02317724 -0.01059605  0.003480357 -1.573742e-03 -1.732570e-14
IDE.d  0.02941109 -0.03022917 -0.002811506 -4.767627e-05  9.675930e-15
 

HD1 <- matrix(c(1, -1, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 1), c(5,3))
DA <- matrix(c(1,0,0,0, 0, 1, 0, 0, 0, 0, 0, 1), c(4,3))

https://cran.r-project.org/web/packages/urca/refman/urca.html#ablrtest
summary(ablrtest(sjd.vecm, H=HD1, A=DA, r=1))
###################### 
# Johansen-Procedure # 
###################### 
Estimation and testing under linear restrictions on alpha and beta 
The VECM has been estimated subject to: 
beta=H*phi and/or alpha=A*psi
     [,1] [,2] [,3]
[1,]    1    0    0
[2,]   -1    0    0
[3,]    0    1    0
[4,]    0   -1    0
[5,]    0    0    1


     [,1] [,2] [,3]
[1,]    1    0    0
[2,]    0    1    0
[3,]    0    0    0
[4,]    0    0    1

Eigenvalues of restricted VAR (lambda):
[1] 0.4100 0.0090 0.0053

The value of the likelihood ratio test statistic:
2.13 distributed as chi square with 2 df.
The p-value of the test statistic is: 0.35 
Eigenvectors, normalised to first column
of the restricted VAR:

        [,1]
[1,]  1.0000
[2,] -1.0000
[3,]  5.9508
[4,] -5.9508
[5,] -6.2162

Weights W of the restricted VAR:

        [,1]
[1,] -0.1519
[2,]  0.0992
[3,]  0.0000
[4,]  0.0288

https://cran.r-project.org/web/packages/urca/refman/urca.html#alphaols
summary(alphaols(sjd.vecm))
Response R0.LRM.d :

Call:
lm(formula = R0.LRM.d ~ V.RK.LRM.l2 + V.RK.LRY.l2 + V.RK.IBO.l2 + 
    V.RK.IDE.l2 + V.RK.constant - 1, data = data.mat)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.039482 -0.014437 -0.005498  0.013169  0.051973 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
V.RK.LRM.l2   -2.130e-01  6.039e-02  -3.526 0.000938 ***
V.RK.LRY.l2   -4.815e-03  4.028e-02  -0.120 0.905341    
V.RK.IBO.l2    3.501e-02  2.184e-02   1.603 0.115453    
V.RK.IDE.l2    2.029e-03  2.815e-03   0.721 0.474633    
V.RK.constant -1.175e-12  3.123e-02   0.000 1.000000    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.02001 on 48 degrees of freedom
Multiple R-squared:  0.2446,    Adjusted R-squared:  0.1659 
F-statistic: 3.108 on 5 and 48 DF,  p-value: 0.01647


Response R0.LRY.d :

Call:
lm(formula = R0.LRY.d ~ V.RK.LRM.l2 + V.RK.LRY.l2 + V.RK.IBO.l2 + 
    V.RK.IDE.l2 + V.RK.constant - 1, data = data.mat)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.03448 -0.01507 -0.00100  0.01115  0.05666 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)  
V.RK.LRM.l2    1.150e-01  6.189e-02   1.859   0.0692 .
V.RK.LRY.l2    1.975e-02  4.128e-02   0.478   0.6345  
V.RK.IBO.l2    4.994e-02  2.238e-02   2.231   0.0304 *
V.RK.IDE.l2    1.109e-03  2.885e-03   0.384   0.7025  
V.RK.constant -3.405e-13  3.200e-02   0.000   1.0000  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.0205 on 48 degrees of freedom
Multiple R-squared:  0.1551,    Adjusted R-squared:  0.06707 
F-statistic: 1.762 on 5 and 48 DF,  p-value: 0.1387


Response R0.IBO.d :

Call:
lm(formula = R0.IBO.d ~ V.RK.LRM.l2 + V.RK.LRY.l2 + V.RK.IBO.l2 + 
    V.RK.IDE.l2 + V.RK.constant - 1, data = data.mat)

Residuals:
       Min         1Q     Median         3Q        Max 
-0.0232722 -0.0044453 -0.0000303  0.0048527  0.0176373 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)
V.RK.LRM.l2    2.318e-02  2.403e-02   0.965    0.340
V.RK.LRY.l2   -1.060e-02  1.602e-02  -0.661    0.512
V.RK.IBO.l2    3.480e-03  8.688e-03   0.401    0.691
V.RK.IDE.l2   -1.574e-03  1.120e-03  -1.405    0.166
V.RK.constant  2.625e-14  1.242e-02   0.000    1.000
Residual standard error: 0.00796 on 48 degrees of freedom
Multiple R-squared:  0.068,     Adjusted R-squared:  -0.02908 
F-statistic: 0.7004 on 5 and 48 DF,  p-value: 0.6258


Response R0.IDE.d :
Call:
lm(formula = R0.IDE.d ~ V.RK.LRM.l2 + V.RK.LRY.l2 + V.RK.IBO.l2 + 
    V.RK.IDE.l2 + V.RK.constant - 1, data = data.mat)
Residuals:
       Min         1Q     Median         3Q        Max 
-0.0091249 -0.0028761 -0.0000153  0.0024579  0.0148999 
Coefficients:
                Estimate Std. Error t value Pr(>|t|)   
V.RK.LRM.l2    2.941e-02  1.524e-02   1.930  0.05949 . 
V.RK.LRY.l2   -3.023e-02  1.016e-02  -2.975  0.00458 **
V.RK.IBO.l2   -2.812e-03  5.510e-03  -0.510  0.61222   
V.RK.IDE.l2   -4.768e-05  7.104e-04  -0.067  0.94677   
V.RK.constant  1.314e-13  7.879e-03   0.000  1.00000   
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.005048 on 48 degrees of freedom
Multiple R-squared:  0.211,     Adjusted R-squared:  0.1288 
F-statistic: 2.568 on 5 and 48 DF,  p-value: 0.03881

summary(alphaols(sjd.vecm, reg.number=1))
DA <- matrix(c(1,0,0,0), c(4,1))
https://cran.r-project.org/web/packages/urca/refman/urca.html#alrtest
summary(alrtest(sjd.vecm, A=DA, r=1))
###################### 
# Johansen-Procedure # 
###################### 
Estimation and testing under linear restrictions on beta 
The VECM has been estimated subject to: 
beta=H*phi and/or alpha=A*psi

     [,1]
[1,]    1
[2,]    0
[3,]    0
[4,]    0
Eigenvalues of restricted VAR (lambda):
[1] 0.3573 0.0000 0.0000 0.0000 0.0000
The value of the likelihood ratio test statistic:
6.66 distributed as chi square with 3 df.
The p-value of the test statistic is: 0.08 

Eigenvectors, normalised to first column
of the restricted VAR:

               [,1]
RK.LRM.l2    1.0000
RK.LRY.l2   -0.9585
RK.IBO.l2    4.7641
RK.IDE.l2   -2.5708
RK.constant -6.5825

Weights W of the restricted VAR:

        [,1]
[1,] -0.2543
[2,]  0.0000
[3,]  0.0000
[4,]  0.0000


HD0 <- matrix(c(-1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1), c(5,4))
https://cran.r-project.org/web/packages/urca/refman/urca.html#blrtest
summary(blrtest(sjd.vecm, H=HD0, r=1))
###################### 
# Johansen-Procedure # 
###################### 
Estimation and testing under linear restrictions on beta 
The VECM has been estimated subject to: 
beta=H*phi and/or alpha=A*psi

     [,1] [,2] [,3] [,4]
[1,]   -1    0    0    0
[2,]    1    0    0    0
[3,]    0    1    0    0
[4,]    0    0    1    0
[5,]    0    0    0    1

Eigenvalues of restricted VAR (lambda):
[1] 0.4327 0.1722 0.0436 0.0056

The value of the likelihood ratio test statistic:
0.04 distributed as chi square with 1 df.
The p-value of the test statistic is: 0.84 

Eigenvectors, normalised to first column
of the restricted VAR:

        [,1]    [,2]     [,3]    [,4]
[1,]  1.0000  1.0000   1.0000  1.0000
[2,] -1.0000 -1.0000  -1.0000 -1.0000
[3,]  5.3004  0.2293  99.7012  1.5513
[4,] -4.2904  8.9614 -51.8139 -2.0206
[5,] -6.2645 -6.6551 -13.4082 -5.8223

Weights W of the restricted VAR:

         [,1]    [,2]   [,3]    [,4]
LRM.d -0.2120 -0.0195  4e-04  0.0131
LRY.d  0.1075 -0.0061  2e-04  0.0196
IBO.d  0.0226 -0.0097 -4e-04  0.0003
IDE.d  0.0297 -0.0226  0e+00 -0.0021

https://cran.r-project.org/web/packages/urca/refman/urca.html#lttest
lttest(sjd.vecm, r=1)
LR-test for no linear trend
H0: H*2(r<=1)
H1: H2(r<=1)
Test statistic is distributed as chi-square
with 3 degress of freedom
        test statistic p-value
LR test           1.98    0.58

plot(sjd.vecm)
plotres(sjd.vecm)

### Example 2 ###
data(finland)
### A data frame with 106 observations on the following ###
### 4 variables,ranging from 1958:Q2 until 1984:Q3      ###
### lrm1: Logarithm of real money, M1                   ###
### lny: Logarithm of real income                       ###
### lnmr: Marginal rate of interest                     ###
### difp: Inflation rate                                ###
head(finland)
sjf <- finland
str(sjf)
sjf.vecm <- ca.jo(sjf, ecdet = "none", type="eigen", K=2,
spec="longrun", season=4)
summary(sjf.vecm)
###################### 
# Johansen-Procedure # 
###################### 

Test type: maximal eigenvalue statistic (lambda max) , with linear trend 

Eigenvalues (lambda):
[1] 0.30932660 0.22599561 0.07308056 0.02946699

Values of teststatistic and critical values of test:

          test 10pct  5pct  1pct
r <= 3 |  3.11  6.50  8.18 11.65
r <= 2 |  7.89 12.91 14.90 19.19
r <= 1 | 26.64 18.90 21.07 25.75
r = 0  | 38.49 24.78 27.14 32.14

Eigenvectors, normalised to first column:
(These are the cointegration relations)

           lrm1.l2    lny.l2    lnmr.l2    difp.l2
lrm1.l2  1.0000000  1.000000  1.0000000   1.000000
lny.l2  -0.9763252 -1.323191 -0.9199865   1.608739
lnmr.l2 -7.0910749 -2.016033  0.2691516  -1.375342
difp.l2 -7.0191097 22.740851 -1.8223931 -15.686927

Weights W:
(This is the loading matrix)

           lrm1.l2       lny.l2      lnmr.l2      difp.l2
lrm1.d 0.033342108 -0.020280528 -0.129947614 -0.002561906
lny.d  0.022544782 -0.005717446  0.012949130 -0.006265406
lnmr.d 0.053505000  0.046876449 -0.007367715  0.002173242
difp.d 0.005554849 -0.017353903  0.014561151  0.001531004

lttest(sjf.vecm, r=3)
LR-test for no linear trend
H0: H*2(r<=3)
H1: H2(r<=3)
Test statistic is distributed as chi-square
with 1 degress of freedom
        test statistic p-value
LR test           4.78    0.03

### Example 3 ###
data(UKpppuip)
### A data frame of quarterly data ranging from 1971:Q1      ###
### until 1987:Q2. All variables are expressed in logarithms.###
### p1: UK wholesale price index                             ###
### p2: Trade weighted foreign whole sale price index        ###
### e12: UK effective exchange rate                          ###
### i1: Three-month treasury bill rate in the UK             ###
### i2: Three-month Eurodollar interest rate                 ###
### dpoil0: World oil price at period t                      ###
### dpoil1: World oil price at period t-1                    ###
head(UKpppuip)
attach(UKpppuip)
dat1 <- cbind(p1, p2, e12, i1, i2)
dat2 <- cbind(doilp0, doilp1)
H1 <- ca.jo(dat1, type='trace', K=2, season=4, dumvar=dat2)
H51 <- c(1, -1, -1, 0, 0)
H52 <- c(0, 0, 0, 1, -1)
summary(bh5lrtest(H1, H=H51, r=2))
summary(bh5lrtest(H1, H=H52, r=2))
H1 <- ca.jo(dat1, type='trace', K=2, season=4, dumvar=dat2)
H6 <- matrix(c(1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0), c(5,3))
bh6lrtest(z=H1, H=H6, r=2, r1=1, conv.val=0.0001, max.iter=50)
