library("mlogit")
data("ModeCanada", package = "mlogit")
str(ModeCanada)
'data.frame':   15520 obs. of  11 variables:
 $ case  : int  1 1 2 2 3 3 4 4 5 5 ...
 $ alt   : Factor w/ 4 levels "train","air",..: 1 4 1 4 1 4 1 4 1 4 ...
 $ choice: int  0 1 0 1 0 1 0 1 0 1 ...
 $ dist  : int  83 83 83 83 83 83 83 83 83 83 ...
 $ cost  : num  28.2 15.8 28.2 15.8 28.2 ...
 $ ivt   : int  50 61 50 61 50 61 50 61 50 61 ...
 $ ovt   : int  66 0 66 0 66 0 66 0 66 0 ...
 $ freq  : int  4 0 4 0 4 0 4 0 4 0 ...
 $ income: int  45 45 25 25 70 70 70 70 55 55 ...
 $ urban : int  0 0 0 0 0 0 0 0 0 0 ...
 $ noalt : int  2 2 2 2 2 2 2 2 2 2 ...
MC <- dfidx(ModeCanada, subset = noalt == 4)
ml.MC1 <- mlogit(choice ~ cost + freq + ovt | income | ivt, MC)
summary(ml.MC1)
Call:
mlogit(formula = choice ~ cost + freq + ovt | income | ivt, data = MC, 
    method = "nr")
Frequencies of alternatives:choice
    train       air       bus       car 
0.1666067 0.3738755 0.0035984 0.4559194 
nr method
9 iterations, 0h:0m:0s 
g'(-H)^-1g = 0.00014 
successive function values within tolerance limits 
Coefficients :
                  Estimate Std. Error  z-value  Pr(>|z|)    
(Intercept):air -3.2741952  0.6244152  -5.2436 1.575e-07 ***
(Intercept):bus -2.5758571  1.0845227  -2.3751 0.0175439 *  
(Intercept):car -1.4300823  0.3013764  -4.7452 2.083e-06 ***
cost            -0.0333389  0.0070955  -4.6986 2.620e-06 ***
freq             0.0925297  0.0050976  18.1517 < 2.2e-16 ***
ovt             -0.0430036  0.0032247 -13.3356 < 2.2e-16 ***
income:air       0.0381466  0.0040831   9.3426 < 2.2e-16 ***
income:bus      -0.0509401  0.0181702  -2.8035 0.0050553 ** 
income:car       0.0101536  0.0031648   3.2083 0.0013353 ** 
ivt:train       -0.0014504  0.0011875  -1.2214 0.2219430    
ivt:air          0.0595097  0.0100727   5.9080 3.463e-09 ***
ivt:bus         -0.0067835  0.0044334  -1.5301 0.1259938    
ivt:car         -0.0064603  0.0018985  -3.4029 0.0006668 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -1874.3
McFadden R^2:  0.35443 
Likelihood ratio test : chisq = 2058.1 (p.value = < 2.22e-16)


ml.MC1b <- mlogit(choice ~ cost + freq + ovt | income | ivt, ModeCanada,
subset = noalt == 4, idx = c("case", "alt"))
summary(ml.MC1b)

MC$time <- with(MC, ivt + ovt)
ml.MC1 <- mlogit(choice ~ cost + freq | income | time, MC, 
alt.subset = c("car", "train", "air"), reflevel = "car")
summary(ml.MC1)

head(fitted(ml.MC1, type = "outcome"))
head(fitted(ml.MC1, type = "probabilities"), 4)
 car     train       air
109 0.4206404 0.3884120 0.1909475
110 0.3696476 0.2903582 0.3399941
111 0.4296769 0.4232704 0.1470527
112 0.3696476 0.2903582 0.3399941
sum(log(fitted(ml.MC1, type = "outcome")))

logLik(ml.MC1)
[1] -1951.344
apply(fitted(ml.MC1, type = "probabilities"), 2, mean)
 car     train       air 
0.4575659 0.1672084 0.3752257 
predict(ml.MC1)
 car     train       air 
0.5066362 0.2116876 0.2816761 
NMC <- MC
# YC2020/05/03 should replace everywhere index() by idx()
NMC[idx(NMC)$alt == "train", "time"] <- 0.8 *
NMC[idx(NMC)$alt == "train", "time"]
Oprob <- fitted(ml.MC1, type = "probabilities")
Nprob <- predict(ml.MC1, newdata = NMC)
rbind(old = apply(Oprob, 2, mean), new = apply(Nprob, 2, mean))
 car     train       air
old 0.4575659 0.1672084 0.3752257
new 0.4044736 0.2635801 0.3319462 
head(Nprob[, "air"] / Nprob[, "car"])
head(Oprob[, "air"] / Oprob[, "car"])
ivbefore <- logsum(ml.MC1)
ivafter <- logsum(ml.MC1, data = NMC)
surplus <- - (ivafter - ivbefore) / coef(ml.MC1)["cost"]
summary(surplus)
car      train        air 
-0.1822177 -0.1509079  0.3331256 

effects(ml.MC1, covariate = "income", type = "ar")
 car      train        air 
-0.1822177 -0.1509079  0.3331256
effects(ml.MC1, covariate = "cost", type = "rr")
 car      train        air
car   -0.9131273  0.9376923  0.9376923
train  0.3358005 -1.2505014  0.3358005
air    1.2316679  1.2316679 -3.1409703 
coef(ml.MC1)[grep("time", names(coef(ml.MC1)))] /
    coef(ml.MC1)["cost"] * 60 
 time:car time:train   time:air 
  29.52728   23.09447   36.95360  

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