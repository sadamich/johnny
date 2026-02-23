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
str(denmark)
head(denmark)
sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
sjd.vecm <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
summary(sjd.vecm)

sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
sjd.vecm <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
HD1 <- matrix(c(1, -1, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 1), c(5,3))
DA <- matrix(c(1,0,0,0, 0, 1, 0, 0, 0, 0, 0, 1), c(4,3))
summary(ablrtest(sjd.vecm, H=HD1, A=DA, r=1))

sjd.vecm1 <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
summary(alphaols(sjd.vecm1))
summary(alphaols(sjd.vecm1, reg.number=1))

DA <- matrix(c(1,0,0,0), c(4,1))
summary(alrtest(sjd.vecm, A=DA, r=1))