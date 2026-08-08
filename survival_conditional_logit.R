https://cran.r-project.org/web/packages/survival/refman/survival.html#clogit
library(survival)

## Not run: clogit(case ~ spontaneous + induced + strata(stratum), data=infert)
# A multinomial response recoded to use clogit
#  The revised data set has one copy per possible outcome level, with new
#  variable tocc = target occupation for this copy, and case = whether
#  that is the actual outcome for each subject.
# See the reference below for the data.
str(infert)
'data.frame':   248 obs. of  8 variables:
 $ education     : Factor w/ 3 levels "0-5yrs","6-11yrs",..: 1 1 1 1 2 2 2 2 2 2 ...
 $ age           : num  26 42 39 34 35 36 23 32 21 28 ...
 $ parity        : num  6 1 6 4 3 4 1 2 1 2 ...
 $ induced       : num  1 1 2 2 1 2 0 0 0 0 ...
 $ case          : num  1 1 1 1 1 1 1 1 1 1 ...
 $ spontaneous   : num  2 0 0 0 1 1 0 0 1 0 ...
 $ stratum       : int  1 2 3 4 5 6 7 8 9 10 ...
 $ pooled.stratum: num  3 1 4 2 32 36 6 22 5 19 ...

attach(infert)
str(logan)
'data.frame':   838 obs. of  4 variables:
 $ occupation: Factor w/ 5 levels "farm","operatives",..: 4 3 4 3 2 5 3 4 5 3 ...
 $ focc      : Factor w/ 5 levels "farm","operatives",..: 5 4 5 4 5 2 3 5 5 5 ...
 $ education : int  14 13 16 16 14 14 12 16 17 16 ...
 $ race      : Factor w/ 2 levels "non-black","black": 1 1 
resp <- levels(logan$occupation)
### the number of observations
n <- nrow(logan)
indx <- rep(1:n, length(resp))
logan2 <- data.frame(logan[indx,],
                     id = indx,
                     tocc = factor(rep(resp, each=n)))
logan2$case <- (logan2$occupation == logan2$tocc)
clogit(case ~ tocc + tocc:education + strata(id), logan2)
### Call: clogit(case ~ tocc + tocc:education + strata(id), logan2)
                                 coef  exp(coef)   se(coef)       z        p
toccfarm                   -1.8964629  0.1500986  1.3807822  -1.373  0.16961
toccoperatives              1.1667502  3.2115388  0.5656465   2.063  0.03914
toccprofessional           -8.1005492  0.0003034  0.6987244 -11.593  < 2e-16
toccsales                  -5.0292297  0.0065438  0.7700862  -6.531 6.54e-11
tocccraftsmen:education    -0.3322842  0.7172835  0.0568682  -5.843 5.13e-09
toccfarm:education         -0.3702858  0.6905370  0.1164100  -3.181  0.00147
toccoperatives:education   -0.4222188  0.6555906  0.0584328  -7.226 4.98e-13
toccprofessional:education  0.2782469  1.3208122  0.0510212   5.454 4.94e-08
toccsales:education                NA         NA  0.0000000      NA       NA

Likelihood ratio test=665.5  on 8 df, p=< 2.2e-16
n= 4190, number of events= 838 
