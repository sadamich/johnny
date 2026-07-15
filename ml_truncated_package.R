https://cran.r-project.org/web/packages/truncreg/refman/truncreg.html
library(truncreg)
### Yves Croissant [aut, cre], Achim Zeileis [aut]

## simulate a data.frame
set.seed(1071)
n <- 10000
sigma <- 4
alpha <- 2
beta <- 1
x <- rnorm(n, mean = 0, sd = 2)
eps <- rnorm(n, sd = sigma)
y <- alpha + beta * x + eps
d <- data.frame(y = y, x = x)

str(d)
data.frame':   10000 obs. of  2 variables:
 $ y: num  5.436 -0.569 14.537 8.094 7.384 ...
 $ x: num  0.754 -1.869 4.86 2.639 0.901 


## truncated response
d$yt <- ifelse(d$y > 1, d$y, NA)
plot(d$yt)
hist(d$yt)

## binary threshold response
d$yb <- factor(d$y > 0)
plot(d$yb)

## censored response
d$yc <- pmax(1, d$y)
plot(d$yc)
hist(d$yc)


## compare estimates for full/truncated/censored/threshold response
fm_full <- lm(y ~ x, data = d)
fm_trunc <- truncreg(yt ~ x, data = d, point = 1, direction = "left")
summary(fm_trunc)
Call:truncreg(formula = yt ~ x, data = d, point = 1, direction = "left")
BFGS maximization method
43 iterations, 0h:0m:0s 
g'(-H)^-1g = 6.18E-08 
Coefficients :
            Estimate Std. Error t-value  Pr(>|t|)    
(Intercept) 1.963593   0.177457  11.065 < 2.2e-16 ***
x           0.981670   0.046088  21.300 < 2.2e-16 ***
sigma       3.980776   0.081134  49.064 < 2.2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -13474 on 3 Df

fm_thresh <- glm(yb ~ x, data = d, family = binomial(link = "probit"))
summary(fm_thresh)
Call:glm(formula = yb ~ x, family = binomial(link = "probit"), data = d)
Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) 0.524201   0.013882   37.76   <2e-16 ***
x           0.251448   0.007494   33.55   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 12512  on 9999  degrees of freedom
Residual deviance: 11253  on 9998  degrees of freedom
AIC: 11257
Number of Fisher Scoring iterations: 4


library("survival")
fm_cens <- survreg(Surv(yc, yc > 1, type = "left") ~ x, data = d, dist = "gaussian")
summary(fm_cens)

survreg(formula = Surv(yc, yc > 1, type = "left") ~ x, data = d, 
    dist = "gaussian")
              Value Std. Error     z      p
(Intercept) 2.05480    0.04560  45.1 <2e-16
x           0.97726    0.02237  43.7 <2e-16
Log(scale)  1.37063    0.00988 138.8 <2e-16
Scale= 3.94 
Gaussian distribution
Loglik(model)= -19549.2   Loglik(intercept only)= -20505.9
        Chisq= 1913.5 on 1 degrees of freedom, p= 0 
Number of Newton-Raphson Iterations: 4 
n= 10000 

## compare scaled regression coefficients
cbind(
  "True"      = c(alpha, beta) / sigma,
  "Full"      = coef(fm_full) / summary(fm_full)$sigma,
  "Truncated" = coef(fm_trunc)[1:2] / coef(fm_trunc)[3],
  "Censored"  = coef(fm_cens) / fm_cens$scale,
  "Threshold" = coef(fm_thresh)
)

            True      Full Truncated  Censored Threshold
(Intercept) 0.50 0.5071291 0.4932690 0.5218093 0.5242008
x           0.25 0.2465092 0.2466027 0.2481720 0.2514478


################################
## Tobin's durable goods data ##
################################

## Tobit model (Tobin 1958)
data("tobin", package = "survival")
str(tobin)
'data.frame':   20 obs. of  3 variables:
 $ durable: num  0 0.7 0 0 0 0 0 3.7 0 3 ...
 $ age    : num  57.7 50.9 48.5 41.7 47.7 59.8 44.3 45.1 51.7 50 ...
 $ quant  : int  236 283 207 220 238 216 284 221 275 269 
attach(tobin)
hist(durable)
y_tr<- durable[durable==0]
str(y_tr)
num [1:13] 0 0 0 0 0 0 0 0 0 0 .
tobit <- survreg(Surv(durable, durable > 0, type = "left") ~ age + quant,
  data = tobin, dist = "gaussian")
summary(tobit)
Call:survreg(formula = Surv(durable, durable > 0, type = "left") ~ 
    age + quant, data = tobin, dist = "gaussian")
              Value Std. Error     z       p
(Intercept) 15.1449    16.0795  0.94    0.35
age         -0.1291     0.2186 -0.59    0.55
quant       -0.0455     0.0583 -0.78    0.43
Log(scale)   1.7179     0.3103  5.54 3.1e-08
Scale= 5.57 
Gaussian distribution
Loglik(model)= -28.9   Loglik(intercept only)= -29.5
        Chisq= 1.1 on 2 degrees of freedom, p= 0.58 
Number of Newton-Raphson Iterations: 3 
n= 20 


## Two-part model (Cragg 1971)
## (see "mhurdle" package for a combined solution)
cragg_probit <- glm(factor(durable > 0) ~ age + quant,
  data = tobin, family = binomial(link = "logit"))
cragg_trunc <- truncreg(durable ~ age + quant, data = tobin, subset = durable > 0)

## Scaled coefficients
cbind(
  "Tobit"     = coef(tobit) / tobit$scale,
  "Binary"    = coef(cragg_probit),
  "Truncated" = coef(cragg_trunc)[1:3] / coef(cragg_trunc)[4])
                   Tobit      Binary   Truncated
(Intercept)  2.717767303  2.07631898  7.62674516
age         -0.023159868 -0.05284724  0.26094645
quant       -0.008172515 -0.00078640 -0.07190702


## likelihood ratio test and BIC
ll <- c("Tobit" = tobit$loglik[1],
        "Two-Part" = as.vector(logLik(cragg_probit) + logLik(cragg_trunc)))
df <- c(4, 3 + 4)
pchisq(2 * diff(ll), diff(df), lower.tail = FALSE)
 Two-Part 
0.01600393 
-2 * ll + log(nrow(tobin)) * df
  Tobit Two-Part 
70.96733 69.63057 