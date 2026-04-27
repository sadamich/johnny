https://haas.berkeley.edu/faculty/meredith-fowlie/

https://cran.r-project.org/web/packages/mlogit/refman/mlogit.html#NOx
data("NOx", package = "mlogit")
NOx$kdereg <- with(NOx, kcost * (env == "deregulated"))
NOxml <- dfidx(NOx, idx = list(c("chid", "id"), "alt"))
ml.pub <- mlogit(choice ~ post + cm + lnb + vcost + kcost + kcost:age |
- 1, subset = available & env == "public", data = NOxml)
summary(ml.pub)
Call:
mlogit(formula = choice ~ post + cm + lnb + vcost + kcost + kcost:age | 
    -1, data = NOxml, subset = available & env == "public", method = "nr")
Frequencies of alternatives:choice
        1         2         3         4         5         6         7         8 
0.0000000 0.0265487 0.0000000 0.0000000 0.0000000 0.0088496 0.0796460 0.0088496 
        9        10        11        12        13        14        15 
0.0000000 0.5044248 0.0000000 0.0088496 0.0000000 0.3628319 0.0000000 
nr method
8 iterations, 0h:0m:0s 
g'(-H)^-1g = 1.67E-07 
gradient close to zero 
Coefficients :
           Estimate Std. Error z-value  Pr(>|z|)    
post      -5.705835   1.018913 -5.5999 2.144e-08 ***
cm        -4.432539   0.558873 -7.9312 2.220e-15 ***
lnb       -3.963699   0.586844 -6.7543 1.436e-11 ***
vcost     -1.564083   0.361457 -4.3272 1.510e-05 ***
kcost      0.038842   0.109664  0.3542   0.72319    
kcost:age -0.080378   0.044560 -1.8038   0.07126 .  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -78.461

ml.reg <- update(ml.pub, subset = available & env == "regulated")
summary(ml.reg)
Call:
mlogit(formula = choice ~ post + cm + lnb + vcost + kcost + kcost:age | 
    -1, data = NOxml, subset = available & env == "regulated", 
    method = "nr")
Frequencies of alternatives:choice
        1         2         3         4         5         6         7         8 
0.0000000 0.0787671 0.0102740 0.0239726 0.0273973 0.0034247 0.0273973 0.0171233 
        9        10        11        12        13        14        15 
0.0000000 0.4109589 0.0376712 0.0136986 0.0000000 0.3116438 0.0376712 
nr method
5 iterations, 0h:0m:0s 
g'(-H)^-1g = 5.67E-07 
gradient close to zero 
Coefficients :
            Estimate Std. Error  z-value  Pr(>|z|)    
post      -2.6654865  0.2795304  -9.5356 < 2.2e-16 ***
cm        -1.9109606  0.1778379 -10.7455 < 2.2e-16 ***
lnb       -2.2076916  0.2206089 -10.0073 < 2.2e-16 ***
vcost     -0.2784424  0.0630969  -4.4129  1.02e-05 ***
kcost      0.0075067  0.0314564   0.2386   0.81139    
kcost:age -0.0232735  0.0118042  -1.9716   0.04865 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -359.74

ml.dereg <- update(ml.pub, subset = available & env == "deregulated")
summary(ml.dereg)
Call:
mlogit(formula = choice ~ post + cm + lnb + vcost + kcost + kcost:age | 
    -1, data = NOxml, subset = available & env == "deregulated", 
    method = "nr")
Frequencies of alternatives:choice
        1         2         3         4         5         6         7         8 
0.0000000 0.0572687 0.0176211 0.0132159 0.0528634 0.0000000 0.0088106 0.0396476 
        9        10        11        12        13        14        15 
0.0088106 0.5066079 0.0352423 0.0264317 0.0088106 0.0969163 0.1277533 
nr method
5 iterations, 0h:0m:0s 
g'(-H)^-1g = 2.59E-06 
successive function values within tolerance limits 
Coefficients :
           Estimate Std. Error z-value  Pr(>|z|)    
post      -1.501995   0.215685 -6.9638 3.311e-12 ***
cm        -1.537863   0.191047 -8.0497 8.882e-16 ***
lnb       -1.551053   0.222682 -6.9653 3.276e-12 ***
vcost     -0.187826   0.055033 -3.4130 0.0006426 ***
kcost     -0.060065   0.023119 -2.5981 0.0093739 ** 
kcost:age -0.037235   0.012229 -3.0447 0.0023289 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -339.07

ml.pool <- ml.dereg
# YC gestion de la quatrième partie
ml.pool <- mlogit(choice ~ post + cm + lnb + vcost + kcost + kcost:age +
kdereg | - 1 | 0 | env, subset = available == 1, data = NOxml,
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

https://www.philipleifeld.com/
install.packages("texreg")
library("texreg")
htmlreg(list(Public = ml.pub, Deregulated = ml.dereg, Regulated = ml.reg,
             Pooled = ml.pool), caption = "Environmental compliance choices.",
        omit.coef = "(post)|(cm)|(lnb)", float.pos = "hbt", label = "tab:nox")

stat <- 2 * (logLik(ml.dereg) + logLik(ml.reg) +
             logLik(ml.pub) - logLik(ml.pool))
stat
## 'log Lik.' 61.6718 (df=6)
pchisq(stat, df = 9, lower.tail = FALSE)
## 'log Lik.' 6.377283e-10 (df=6)
