### https://cran.r-project.org/web/packages/urca/refman/urca.html ###
library(urca) 
## qunitroot -
   # Asymptotic quantile of t-statistic
   qunitroot(0.95, trend = "nc", statistic = "t")

## qunitroot -
   # Finite sample quantile of n-statistic
   qunitroot(0.95, N = 100, trend = "nc", statistic = "n") 
   
## punitroot -
   # Asymptotic cumulative probability of t-statistic
   punitroot(1.2836, trend = "nc", statistic = "t")

## punitroot -
   # Finite sample cumulative probability of n-statistic
   punitroot(1.2836, N = 100, trend = "nc", statistic = "n")
   
## Mac Kinnon's unitrootTable -
   unitrootTable(trend = "nc")

data(denmark)
period	Time index from 1974:Q1 until 1987:Q3.
LRM	Logarithm of real money, M2.
LRY	Logarithm of real income.
LPY	Logarithm of price deflator.
IBO	Bond rate.
IDE	Bank deposit rate.
str(denmark)
head(denmark)
sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
sjd.vecm <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
summary(sjd.vecm)

HD1 <- matrix(c(1, -1, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 1), c(5,3))
HD1
     [,1] [,2] [,3]
[1,]    1    0    0
[2,]   -1    0    0
[3,]    0    1    0
[4,]    0   -1    0
[5,]    0    0    1
DA <- matrix(c(1,0,0,0, 0, 1, 0, 0, 0, 0, 0, 1), c(4,3))
DA
     [,1] [,2] [,3]
[1,]    1    0    0
[2,]    0    1    0
[3,]    0    0    0
[4,]    0    0    1
summary(ablrtest(sjd.vecm, H=HD1, A=DA, r=1))

summary(alphaols(sjd.vecm))
summary(alphaols(sjd.vecm, reg.number=1))

DA <- matrix(c(1,0,0,0), c(4,1))
summary(alrtest(sjd.vecm, A=DA, r=1))

plotres(sjd.vecm)

https://cran.r-project.org/web/packages/urca/refman/urca.html#ca.jo
data(denmark)
sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
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

> 
