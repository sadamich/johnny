https://cran.r-project.org/web/packages/car/refman/car.html#dfbetaPlots
library(car)
dfbetaPlots(lm(prestige ~ income + education + type, data=Duncan))

dfbetasPlots(glm(partic != "not.work" ~ hincome + children, 
  data=Womenlf, family=binomial))
attach(Womenlf)
str(Womenlf)

'data.frame':   263 obs. of  4 variables:
 $ partic  : Factor w/ 3 levels "fulltime","not.work",..: 2 2 2 2 2 2 2 1 2 2 ...
 $ hincome : int  15 13 45 23 19 7 15 7 15 23 ...
 $ children: Factor w/ 2 levels "absent","present": 2 2 2 2 2 2 2 2 2 2 ...
 $ region  : Factor w/ 5 levels "Atlantic","BC",..: 3 3 3 3 3 3 3 3 3 3 ...
