### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm301<- read.csv("xm301.csv",header=TRUE)
attach(xm301)
str(xm301)
library(car)
https://cran.r-project.org/web/packages/car/refman/car.html#boxCox
### Compare with the panel 2 (p.300) 
with(xm301, boxCox(SALARY/10000 ~ EDUC+GENDER+MINORITY, data = xm301,
         lambda = seq(-0.25, 0.25, length = 10)))




https://cran.r-project.org/web/packages/car/refman/car.html#boxCoxVariable

eq<- lm(SALARY/10000 ~ EDUC+GENDER+MINORITY)
eq_bc<- update(eq, . ~ .+ boxCoxVariable(SALARY/10000 +1))
summary(eq_bc)

eq_bc2<- lm(SALARY/10000 ~ GENDER+MINORITY+boxCoxVariable(EDUC -1)+
boxCoxVariable(SALARY/10000 +1))
summary(eq_bc2)

Coefficients:
                                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)                      11.59386    0.94981   12.21  < 2e-16 ***
GENDER                            0.98901    0.07456   13.27  < 2e-16 ***
MINORITY                         -0.48667    0.08768   -5.55 4.78e-08 ***
boxCoxVariable(EDUC - 1)          0.10843    0.08403    1.29    0.198    
boxCoxVariable(SALARY/10000 + 1)  1.86343    0.05665   32.89  < 2e-16 ***

https://cran.r-project.org/web/packages/car/refman/car.html#powerTransform
summary(p1 <- powerTransform(SALARY/10000 ~ EDUC+GENDER+MINORITY, xm301))
bcPower Transformation to Normality 
   Est Power Rounded Pwr Wald Lwr Bnd Wald Upr Bnd
Y1   -0.8359          -1      -1.0085      -0.6633

Likelihood ratio test that transformation parameter is equal to 0
 (log transformation)
                           LRT df       pval
LR test, lambda = (0) 97.14303  1 < 2.22e-16

Likelihood ratio test that no transformation is needed
                           LRT df       pval
LR test, lambda = (1) 479.1352  1 < 2.22e-16
> 


