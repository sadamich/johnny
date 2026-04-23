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

sum(log(fitted(ml.MC1, type = "outcome")))

logLik(ml.MC1)

apply(fitted(ml.MC1, type = "probabilities"), 2, mean)

predict(ml.MC1)

NMC <- MC
# YC2020/05/03 should replace everywhere index() by idx()
NMC[idx(NMC)$alt == "train", "time"] <- 0.8 *
NMC[idx(NMC)$alt == "train", "time"]
Oprob <- fitted(ml.MC1, type = "probabilities")
Nprob <- predict(ml.MC1, newdata = NMC)
rbind(old = apply(Oprob, 2, mean), new = apply(Nprob, 2, mean))

head(Nprob[, "air"] / Nprob[, "car"])

head(Oprob[, "air"] / Oprob[, "car"])

ivbefore <- logsum(ml.MC1)

ivafter <- logsum(ml.MC1, data = NMC)

surplus <- - (ivafter - ivbefore) / coef(ml.MC1)["cost"]
summary(surplus)

effects(ml.MC1, covariate = "income", type = "ar")

effects(ml.MC1, covariate = "cost", type = "rr")

coef(ml.MC1)[grep("time", names(coef(ml.MC1)))] /
    coef(ml.MC1)["cost"] * 60 

data("NOx", package = "mlogit")
NOx$kdereg <- with(NOx, kcost * (env == "deregulated"))
NOxml <- dfidx(NOx, idx = list(c("chid", "id"), "alt"))
ml.pub <- mlogit(choice ~ post + cm + lnb + vcost + kcost + kcost:age |
- 1, subset = available & env == "public", data = NOxml)
ml.reg <- update(ml.pub, subset = available & env == "regulated")
ml.dereg <- update(ml.pub, subset = available & env == "deregulated")
ml.pool <- ml.dereg
# YC gestion de la quatrième partie
ml.pool <- mlogit(choice ~ post + cm + lnb + vcost + kcost + kcost:age +
kdereg | - 1 | 0 | env, subset = available == 1, data = NOxml,
method = "bhhh")
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