https://cran.r-project.org/web/packages/MASS/refman/MASS.html#huber
library(MASS)
str(chem)
num [1:24] 2.9 3.1 3.4 3.4 3.7 3.7 2.8 2.5 2.4 2.4 ...
hist(chem,freq=FALSE)
summary(chem)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.200   2.775   3.385   4.280   3.700  28.950 
huber(chem)
$mu
[1] 3.206724
$s
[1] 0.526323
hubers(chem, mu=3.68)
$mu
[1] 3.68
$s
[1] 0.9409628

### rlm 
https://cran.r-project.org/web/packages/MASS/refman/MASS.html#rlm
str(stackloss)
stackloss is a data frame with 21 observations on 4 variables.
[,1]	Air Flow	Flow of cooling air
[,2]	Water Temp	Cooling Water Inlet Temperature
[,3]	Acid Conc.	Concentration of acid [per 1000, minus 500]
[,4]	stack.loss	Stack loss
'data.frame':   21 obs. of  4 variables:
 $ Air.Flow  : num  80 80 75 62 62 62 62 62 58 58 ...
 $ Water.Temp: num  27 27 25 24 22 23 24 24 23 18 ...
 $ Acid.Conc.: num  89 88 90 87 87 87 93 93 87 80 ...
 $ stack.loss: num  42 37 37 28 18 18 19 20 15 14 ..

summary(rlm(stack.loss ~ ., stackloss))
z<- rlm(stack.loss ~ ., stackloss, psi = psi.hampel, init = "lts")
summary(z)
fit<- fitted(z)
plot(fit, type="l")
rlm(stack.loss ~ ., stackloss, psi = psi.bisquare)

psi:	the psi function is specified by this argument. It must give (possibly
 by name) a function g(x, ..., deriv) that for deriv=0 returns psi(x)/x and
 for deriv=1 returns psi'(x). Tuning constants will be passed in via
