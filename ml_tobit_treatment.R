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