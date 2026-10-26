https://cran.r-project.org/web/packages/car/refman/car.html#influencePlot
library(car)
attach(Duncan)
str(Duncan)
'data.frame':   45 obs. of  4 variables:
 $ type     : Factor w/ 3 levels "bc","prof","wc": 2 2 2 2 2 2 2 2 3 2 ...
 $ income   : int  62 72 75 55 64 21 64 80 67 72 ...
 $ education: int  86 76 92 90 86 84 93 100 87 86 ...
 $ prestige : int  82 83 90 76 90 87 93 90 52 88 .
influencePlot(lm(prestige ~ income + education, data=Duncan))
## Not run:  # requires user interaction to identify points
influencePlot(lm(prestige ~ income + education, data=Duncan), 
    id=list(method="identify"))