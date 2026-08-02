https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-OOP.pdf
### 5.2. Probit and tobit models
library(sandwich)
library(AER)
data("Affairs", package = "AER")
str(Affairs)
attach(Affairs)
'data.frame':   601 obs. of  9 variables:
 $ affairs      : num  0 0 0 0 0 0 0 0 0 0 ...
 $ gender       : Factor w/ 2 levels "female","male": 2 1 1 2 2 1 1 2 1 2 ...
 $ age          : num  37 27 32 57 22 32 22 57 32 22 ...
 $ yearsmarried : num  10 4 15 15 0.75 1.5 0.75 15 15 1.5 ...
 $ children     : Factor w/ 2 levels "no","yes": 1 1 2 2 1 1 1 2 2 1 ...
 $ religiousness: int  3 4 1 5 2 2 2 2 4 4 ...
 $ education    : num  18 14 12 18 17 17 12 14 16 14 ...
 $ occupation   : int  7 6 1 6 6 5 1 4 1 4 ...
 $ rating       : int  4 4 4 5 3 5 3 4 2 5 

eq<- lm(affairs ~ age + yearsmarried + religiousness + occupation +rating)
summary(eq)
### OLS estimate Call:
lm(formula = affairs ~ age + yearsmarried + religiousness + occupation + 
    rating)
Residuals:
    Min      1Q  Median      3Q     Max 
-5.0382 -1.7076 -0.7780  0.2086 12.8134 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)    5.60816    0.79660   7.040 5.31e-12 ***
age           -0.05035    0.02211  -2.278   0.0231 *  
yearsmarried   0.16185    0.03690   4.387 1.36e-05 ***
religiousness -0.47632    0.11131  -4.279 2.18e-05 ***
occupation     0.10601    0.07110   1.491   0.1365    
rating        -0.71224    0.11829  -6.021 3.03e-09 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 3.087 on 595 degrees of freedom
Multiple R-squared:  0.1314,    Adjusted R-squared:  0.1241 
F-statistic:    18 on 5 and 595 DF,  p-value: < 2.2e-16
### bias ~ OLS  Tobit                     
5.60816   /8.174197 
-0.05035  / -0.179333
0.16185   /0.554142
-0.47632  /-1.686220
 0.10601  / 0.326053 
-0.71224  /-2.284973
### tobit(): AER          
### no affairs is not interesting : censored : left censored : y = 0       ###

fm_tobit2 <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
  right = 4, data = Affairs)
coeftest(fm_tobit2)
z test of coefficients:
               Estimate Std. Error z value  Pr(>|z|)    
(Intercept)    7.900980   2.803855  2.8179 0.0048339 ** 
age           -0.177598   0.079906 -2.2226 0.0262441 *  
yearsmarried   0.532302   0.141168  3.7707 0.0001628 ***
religiousness -1.616336   0.424397 -3.8085 0.0001398 ***
occupation     0.324186   0.253878  1.2769 0.2016238    
rating        -2.207007   0.449832 -4.9063 9.281e-07 ***
Log(scale)     2.072319   0.110396 18.7717 < 2.2e-16 ***
### cf) Package sampleSelection 
library(sampleSelection)

fm_tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + 
                  rating, data = Affairs)
coeftest(fm_tobit)
z test of coefficients:

               Estimate Std. Error z value  Pr(>|z|)    
(Intercept)    8.174197   2.741446  2.9817  0.002866 ** 
age           -0.179333   0.079093 -2.2674  0.023368 *  
yearsmarried   0.554142   0.134518  4.1195 3.798e-05 ***
religiousness -1.686220   0.403752 -4.1764 2.962e-05 ***
occupation     0.326053   0.254425  1.2815  0.200007    
rating        -2.284973   0.407828 -5.6028 2.109e-08 ***
Log(scale)     2.109859   0.067098 31.4444 < 2.2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


coeftest(fm_tobit, vcov= sandwich)
z test of coefficients:

               Estimate Std. Error z value  Pr(>|z|)    
(Intercept)    8.174197   3.077933  2.6557  0.007913 ** 
age           -0.179333   0.088915 -2.0169  0.043706 *  
yearsmarried   0.554142   0.137162  4.0400 5.344e-05 ***
religiousness -1.686220   0.399854 -4.2171 2.475e-05 ***
occupation     0.326053   0.245978  1.3255  0.184993    
rating        -2.284973   0.393479 -5.8071 6.356e-09 ***
Log(scale)     2.109859   0.054837 38.4754 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



fm_probit <- glm(I(affairs > 0) ~ age + yearsmarried + religiousness + occupation + rating,
data = Affairs, family = binomial(link = "probit"))
coeftest(fm_probit)

coeftest(fm_probit, vcov= sandwich)

