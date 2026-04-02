### https://cran.r-project.org/web/packages/lmtest/vignettes/lmtest-intro.pdf ###
library(lmtest)
data(jocci)
str(jocci)
eq<- lm(dy ~ 1, data = jocci)
summary(eq)
### OLS estimate Call: lm(formula = dy ~ 1, data = jocci)                    ###
Residuals:
      Min        1Q    Median        3Q       Max 
-0.045893 -0.008181 -0.000439  0.007646  0.049951 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept) 0.0012751  0.0006335   2.013   0.0448 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.01284 on 410 degrees of freedom

dwtest(dy ~ 1, data = jocci)
        Durbin-Watson test
data:  dy ~ 1
DW = 1.0581, p-value < 2.2e-16
alternative hypothesis: true autocorrelation is greater than 0

ar6.model <- dy ~ dy1 + dy2 + dy3 + dy4 + dy5 +dy6
bgtest(ar6.model, data = jocci)
 Breusch-Godfrey test for serial correlation of order up to 1
data:  ar6.model
LM test = 0.19999, df = 1, p-value = 0.6547

var.model <- ~ I(dy1^2) + I(dy2^2) + I(dy3^2) + I(dy4^2) + I(dy5^2) + I(dy6^2)
bptest(ar6.model, var.model, data = jocci)
 studentized Breusch-Pagan test
data:  ar6.model
BP = 22.377, df = 6, p-value = 0.001034


data(Mandible)
str(Mandible)
mandible <- log(Mandible)
harvtest(length ~ age, order.by = ~ age, data = mandible)
 Harvey-Collier test
data:  length ~ age
HC = 5.0176, df = 164, p-value = 1.35e-06
raintest(length ~ age, order.by = ~ age, data = mandible)
  Rainbow test
data:  length ~ age
Rain = 2.4042, df1 = 84, df2 = 81, p-value = 4.829e-05


resettest(length ~ age, data = mandible)
    RESET test
data:  length ~ age
RESET = 25.637, df1 = 2, df2 = 163, p-value = 2.086e-10

raintest(length ~ age + I(age^2), order.by = ~ age, data = mandible)
 Rainbow test
data:  length ~ age + I(age^2)
Rain = 1.579, df1 = 84, df2 = 80, p-value = 0.02033
