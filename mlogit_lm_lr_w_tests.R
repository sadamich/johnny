https://cran.r-project.org/web/packages/mlogit/refman/mlogit.html#three.tests
library(mlogit)
library("lmtest")
data("TravelMode", package = "AER")
ml <- mlogit(choice ~ wait + travel + vcost, TravelMode,
             shape = "long", chid.var = "individual", alt.var = "mode")
hl <- mlogit(choice ~ wait + travel + vcost, TravelMode,
             shape = "long", chid.var = "individual", alt.var = "mode",
             method = "bfgs", heterosc = TRUE)
lrtest(ml, hl)
Likelihood ratio test

Model 1: choice ~ wait + travel + vcost
Model 2: choice ~ wait + travel + vcost
  #Df  LogLik Df  Chisq Pr(>Chisq)
1   6 -192.89                     
2   9 -190.18  3 5.4203     0.1435

waldtest(hl)
 Wald test
data:  homoscedasticity
chisq = 39.944, df = 3, p-value = 1.095e-08

scoretest(ml, heterosc = TRUE)
  score test
data:  heterosc = TRUE
chisq = 26.028, df = 3, p-value = 9.41e-06
alternative hypothesis: heteroscedastic model

