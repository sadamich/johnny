library(MASS)
# conditional random-effects analysis
library(survival)
https://cran.r-project.org/web/packages/MASS/refman/MASS.html#bacteria
str(bacteria)
attach(bacteria)
'data.frame':   220 obs. of  6 variables:
 $ y   : Factor w/ 2 levels "n","y": 2 2 2 2 2 2 1 2 2 2 ...
 $ ap  : Factor w/ 2 levels "a","p": 2 2 2 2 1 1 1 1 1 1 ...
 $ hilo: Factor w/ 2 levels "hi","lo": 1 1 1 1 1 1 1 1 2 2 ...
 $ week: int  0 2 4 11 0 2 6 11 0 2 ...
 $ ID  : Factor w/ 50 levels "X01","X02","X03",..: 1 1 1 1 2 2 2 2 3 3 ...
 $ trt : Factor w/ 3 levels "placebo","drug",..: 

bacteria$Time <- rep(1, nrow(bacteria))
coxph(Surv(Time, unclass(y)) ~ week + strata(ID),
      data = bacteria, method = "exact")
coxph(Surv(Time, unclass(y)) ~ factor(week) + strata(ID),
      data = bacteria, method = "exact")
coxph(Surv(Time, unclass(y)) ~ I(week > 2) + strata(ID),
      data = bacteria, method = "exact")

https://cran.r-project.org/web/packages/survival/refman/survival.html#coxph
unclass(  )
str(y)
 Factor w/ 2 levels "n","y": 2 2 2 2 2 2 1 2 2 2
str(unclass(y))
int [1:220] 2 2 2 2 2 2 1 2 2 2 ...
 - attr(*, "levels")= chr [1:2] "n" "y"
strata {survival}
Description
This is a special function used in the context of the Cox survival model. 
It identifies stratification variables when they appear on the right hand side
of a formula.
str(strata(ID))
I( )
Description
Change the class of an object to indicate that it should be treated ‘as is’