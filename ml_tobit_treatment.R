### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 6 8 student learning (p.507)                                   ###
xm608micro<- read.csv("xm608micro.csv", header =TRUE)
str(xm608micro)
attach(xm608micro)
xm608macro<- read.csv("xm608macro.csv", header =TRUE)
str(xm608macro)
attach(xm608macro)
library(sampleSelection)
sat<- SATMATH/100
verb<- SATVERB/100
panel <- treatReg(MATHHIGH~sat+FEMALE+MAJORESH+MAJORNAT+ADVMATH1
                    +ADVMATH2+ADVMATH3+PHYSICS+CHEMISTRY,
              GRINTERMICRO ~ SELCORMICRO+MATHHIGH+GRADELOW+
              GRADEHIGH+GRDFINTERMICRO+GRMACRO1+GRMICRO1+FRESHMAN+FEMALE
              +sat+verb)
summary(panel)
### Compare with the panel 1 (p.509)                                       ###
Tobit treatment model (switching regression model)
Maximum Likelihood estimation
Newton-Raphson maximisation, 5 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -992.7169 
609 observations: 224 non-participants (selection 0) and 385 participants (selection 1)

24 free parameters (df = 585)
Probit selection equation:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.243113   0.697136  -0.349 0.727416    
sat         -0.058584   0.094460  -0.620 0.535368    
FEMALE      -0.006212   0.114755  -0.054 0.956844    
MAJORESH    -0.183621   0.152065  -1.208 0.227720    
MAJORNAT     0.401941   0.177388   2.266 0.023823 *  
ADVMATH1     0.244229   0.274140   0.891 0.373355    
ADVMATH2     1.027700   0.268477   3.828 0.000143 ***
ADVMATH3     1.027694   0.632589   1.625 0.104790    
PHYSICS      0.403379   0.144938   2.783 0.005558 ** 
CHEMISTRY    0.157183   0.216184   0.727 0.467467    
Outcome equation:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -0.66105    0.39660  -1.667 0.096088 .  
SELCORMICRO     0.17871    0.25188   0.709 0.478306    
MATHHIGH       -0.05476    0.16505  -0.332 0.740167    
GRADELOW        0.07504    0.04151   1.808 0.071157 .  
GRADEHIGH       0.16046    0.04419   3.631 0.000307 ***
GRDFINTERMICRO  0.80721    0.10334   7.811 2.63e-14 ***
GRMACRO1        0.17261    0.04858   3.553 0.000411 ***
GRMICRO1        0.29285    0.04707   6.221 9.39e-10 ***
FRESHMAN        0.30780    0.09421   3.267 0.001149 ** 
FEMALE          0.09983    0.06072   1.644 0.100688    
sat             0.11935    0.05488   2.175 0.030045 *  
verb            0.03873    0.04531   0.855 0.393101    
   Error terms:
      Estimate Std. Error t value Pr(>|t|)    
sigma  0.69526    0.03409  20.396   <2e-16 ***
rho   -0.17716    0.40847  -0.434    0.665    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
panel01<- glm(formula = MATHHIGH ~ sat+FEMALE+MAJORESH+MAJORNAT+ADVMATH1
+ADVMATH2+ADVMATH3+PHYSICS+CHEMISTRY,family = binomial(link = "probit"))
summary(panel01)

Call:
glm(formula = MATHHIGH ~ sat + FEMALE + MAJORESH + MAJORNAT + 
    ADVMATH1 + ADVMATH2 + ADVMATH3 + PHYSICS + CHEMISTRY, family = binomial(link = "probit"))

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.1890876  0.6791948  -0.278   0.7807    
sat         -0.0536274  0.0932916  -0.575   0.5654    
FEMALE      -0.0009193  0.1136772  -0.008   0.9935    
MAJORESH    -0.2139702  0.1364860  -1.568   0.1169    
MAJORNAT     0.4123882  0.1781167   2.315   0.0206 *  
ADVMATH1     0.1972496  0.2462011   0.801   0.4230    
ADVMATH2     0.9897653  0.2504030   3.953 7.73e-05 ***
ADVMATH3     0.9554815  0.6184408   1.545   0.1223    
PHYSICS      0.3634772  0.1164343   3.122   0.0018 ** 
CHEMISTRY    0.1533989  0.2169521   0.707   0.4795    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 801.18  on 608  degrees of freedom
Residual deviance: 710.40  on 599  degrees of freedom
AIC: 730.4

Number of Fisher Scoring iterations: 4

> 






### https://cran.r-project.org/web/packages/sampleSelection/vignettes/treatReg.pdf ###
library(sampleSelection)
N <- 2000
sigma <- 1
rho <- 0.8
Sigma <- matrix(c(1, rho*sigma, rho*sigma, sigma^2), 2, 2)
### variance-covariance matrix                                               ###
uv <- mvtnorm::rmvnorm(N, mean=c(0,0), sigma=Sigma)
### bivariate normal RV
u <- uv[,1]
v <- uv[,2]
x <- rnorm(N)
z <- rnorm(N)
ySX <--1 + x + z + u
yS <- ySX > 0
# normal covariates
# unobserved participation tendency
# observed participation

yO <- x + yS + v
dat <- data.frame(yO, yS, x, z)

m <- lm(yO ~ x + yS, data=dat)
print(summary(m))
Call: lm(formula = yO ~ x + yS, data = dat)
Residuals:
    Min      1Q  Median      3Q     Max 
-3.6356 -0.5979 -0.0035  0.5945  3.0261 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.26538    0.02522  -10.52   <2e-16 ***
x            0.81480    0.02332   34.94   <2e-16 ***
ySTRUE       1.95937    0.05190   37.75   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9369 on 1997 degrees of freedom
Multiple R-squared:  0.7009,    Adjusted R-squared:  0.7006 
F-statistic:  2340 on 2 and 1997 DF,  p-value: < 2.2e-16

tm <- treatReg(yS ~ x + z, yO ~ x + yS, data=dat)
print(summary(tm))

### Labor traning 
install.packages("Ecdat")
library(Ecdat)
data(Treatment, package="Ecdat")
er <- treatReg(treat~poly(age,2) + educ + u74 + u75 + ethn,
log(re78)~treat + poly(age,2) + educ + ethn,
data=Treatment)
print(summary(er))

noer <- treatReg(treat~poly(age,2) + educ + u74 + u75 + ethn,
log(re78)~treat + poly(age,2) + educ + u74 + u75 + ethn,
data=Treatment)
print(summary(noer))