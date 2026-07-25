https://cran.r-project.org/web/packages/mlogit/vignettes/c3.rum.html
library(mlogit)
data(NOx)
str(NOx)
attach(NOx)

NOx$kdereg <- with(NOx, kcost * (env == "deregulated"))
NOxml <- dfidx(NOx, idx = c(id = "chid", "alt"))
ml.pub <- mlogit(choice ~ post + cm + lnb + vcost +
                     kcost + kcost:age | - 1,
                 subset = available & env == "public",
                 data = NOxml)
ml.reg <- update(ml.pub, subset = available & env == "regulated")
ml.dereg <- update(ml.pub, subset = available & env == "deregulated")
ml.pool <- ml.dereg
ml.pool <- mlogit(choice ~ post + cm + lnb + vcost + kcost +
                      kcost:age + kdereg | - 1 | 0 | env,
                  subset = available == 1, data = NOxml,
                  method = "bhhh")

summary(ml.pool)
Call:
mlogit(formula = choice ~ post + cm + lnb + vcost + kcost + kcost:age + 
    kdereg | -1 | 0 | env, data = NOxml, subset = available == 
    1, method = "bhhh")
Frequencies of alternatives:choice
        1         2         3         4         5         6         7         8 
0.0000000 0.0617089 0.0110759 0.0158228 0.0316456 0.0031646 0.0300633 0.0237342 
        9        10        11        12        13        14        15 
0.0031646 0.4620253 0.0300633 0.0174051 0.0031646 0.2436709 0.0632911 

bhhh method
16 iterations, 0h:0m:0s 
g'(-H)^-1g = 8E-07 
gradient close to zero 
Coefficients :
                     Estimate Std. Error  z-value  Pr(>|z|)    
post               -2.3099589  0.2082171 -11.0940 < 2.2e-16 ***
cm                 -2.0621443  0.1597324 -12.9100 < 2.2e-16 ***
lnb                -2.0328973  0.1737734 -11.6986 < 2.2e-16 ***
vcost              -0.3119937  0.0387471  -8.0521 8.882e-16 ***
kcost               0.0085048  0.0184770   0.4603 0.6453078    
kdereg             -0.0666010  0.0116438  -5.7198 1.066e-08 ***
kcost:age          -0.0200939  0.0057976  -3.4659 0.0005285 ***
sig.envderegulated  0.3188074  0.1237070   2.5771 0.0099628 ** 
sig.envpublic      -0.3262072  0.0815995  -3.9977 6.397e-05 ***

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -808.11

### LR test for the model selection 
stat <- 2 * (logLik(ml.dereg) + logLik(ml.reg) +
             logLik(ml.pub) - logLik(ml.pool))
stat
### 'log Lik.' 61.6718 (df=6)
pchisq(stat, df = 9, lower.tail = FALSE)
### 'log Lik.' 6.377283e-10 (df=6)  (H0 is rejected) 

### Predictions and marginal effects




