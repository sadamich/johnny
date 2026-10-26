https://cran.r-project.org/web/packages/car/refman/car.html#leveneTest
library(car)
attach(Moore)
str(Moore)
'data.frame':   45 obs. of  4 variables:
 $ partner.status: Factor w/ 2 levels "high","low": 2 2 2 2 2 2 2 2 2 2 ...
 $ conformity    : int  8 4 8 7 10 6 12 4 13 12 ...
 $ fcategory     : Factor w/ 3 levels "high","low","medium": 2 1 1 2 2 2 3 3 2 2 ...
 $ fscore        : int  37 57 65 20 36 18 51 44 31 36 ...
with(Moore, leveneTest(conformity, fcategory))
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  2   0.046 0.9551
      42   
with(Moore, leveneTest(conformity, interaction(fcategory, partner.status)))
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  5  1.4694 0.2219
      39   
leveneTest(conformity ~ fcategory*partner.status, data=Moore)
leveneTest(lm(conformity ~ fcategory*partner.status, data=Moore))
leveneTest(conformity ~ fcategory*partner.status, data=Moore, center=mean)
leveneTest(conformity ~ fcategory*partner.status, data=Moore, center=mean, trim=0.1)