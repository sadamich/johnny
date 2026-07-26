https://cran.r-project.org/web/packages/mlogit/vignettes/c5.mxl.html

### The random parameters (or mixed) logit model
### Derivation of the model
### The probabilities

### Individual parameters

### Panel data

library(mlogit)
data("Train", package = "mlogit")
Train$choiceid <- 1:nrow(Train)
Tr <- dfidx(Train, choice = "choice", varying = 4:11, sep = "_",
            opposite = c("price", "comfort", "time", "change"),
            idx = list(c("choiceid", "id")), idnames = c("chid", "alt"))
Tr$price <- Tr$price / 100 / 2.20371
Tr$time <- Tr$time / 60
Train.ml <- mlogit(choice ~ price + time + change + comfort | - 1, Tr)
Train.ml |> gaze()

  Estimate Std. Error z-value Pr(>|z|)
price    0.28517    0.01475  19.337  < 2e-16
time     0.73791    0.08224   8.973  < 2e-16
change   0.25986    0.05809   4.473 7.71e-06
comfort  0.82417    0.06117  13.473  < 2e-16

coef(Train.ml)[- 1] / coef(Train.ml)[1]
     time    change   comfort 
2.5875726 0.9112191 2.8900676 

coef(Train.ml)[- 1] / coef(Train.ml)[1]


Train.mxlu <- mlogit(choice ~ price + time + change + comfort | - 1, Tr,
panel = TRUE, rpar = c(time = "n", change = "n", comfort = "n"), R = 100,
correlation = FALSE, halton = NA, method = "bhhh")
names(coef(Train.mxlu))
[1] "price"      "time"       "change"     "comfort"    "sd.time"   
[6] "sd.change"  "sd.comfort"

Train.mxlc <- update(Train.mxlu, correlation = TRUE)
names(coef(Train.mxlc))
 [1] "price"                "time"                 "change"              
 [4] "comfort"              "chol.time:time"       "chol.time:change"    
 [7] "chol.change:change"   "chol.time:comfort"    "chol.change:comfort" 
[10] "chol.comfort:comfort"


marg.ut.time <- rpar(Train.mxlc, "time")
summary(marg.ut.time)
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
     -Inf 0.5079967 1.1082438 1.1082438 1.7084909       Inf 

wtp.time <- rpar(Train.mxlc, "time", norm = "price")
summary(wtp.time)

    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    -Inf 1.294733 2.824586 2.824586 4.354438      Inf 
mean(rpar(Train.mxlc, "time", norm = "price"))
## [1] 2.824586
med(rpar(Train.mxlc, "time", norm = "price"))
## [1] 2.824586
stdev(rpar(Train.mxlc, "time", norm = "price"))
## [1] 2.268163

vcov(Train.mxlc, what = "rpar")

vcov(Train.mxlc, what = "rpar", type = "cor")

summary(vcov(Train.mxlc, what = "rpar", type = "cor"))

summary(vcov(Train.mxlc, what = "rpar", type = "cov"))

cor.mlogit(Train.mxlc)

cov.mlogit(Train.mxlc)

stdev(Train.mxlc)

Train.mxlc2 <- update(Train.mxlc, correlation = c("time", "comfort"))


lrtest(Train.mxlc, Train.ml) |> gaze()
## Chisq = 172.197, df: 6, pval = 0.000
waldtest(Train.mxlc) |> gaze()
## chisq = 176.571, df: 6, pval = 0.000
car::lht(Train.mxlc,
         c("chol.time:time = 0", "chol.time:change =  0",
           "chol.time:comfort = 0", "chol.change:change = 0",
           "chol.change:comfort = 0", "chol.comfort:comfort = 0")) |>
    gaze()
## Chisq = 176.571, df: 6, pval = 0.000
scoretest(Train.ml,
          rpar = c(time = "n", change = "n", comfort = "n"),
          R = 100, correlation = TRUE, halton = NA, panel = TRUE)


### RiskyTransport
data(RiskyTransport)
str(RiskyTransport)
id: individual id,
choice: 1 for the chosen mode,
mode: one of Helicopter,WaterTaxi, ⁠Ferry, and ⁠Hovercraft',
cost: the generalised cost of the transport mode,
risk: the fatality rate, numbers of death per 100,000 trips,
weight: weights,
seats: ,
noise: ,
crowdness: ,
convloc: ,
clientele: ,
chid: choice situation id,
african: yes if born in Africa, no otherwise,
lifeExp: declared life expectancy,
dwage: declared hourly wage,
iwage: imputed hourly wage,
educ: level of education, one of low and high,
fatalism: self-ranking of the degree of fatalism,
gender: gender, one of female and male,
age: age,
haveChildren: yes if the traveler has children, no otherwise,
swim: yes if the traveler knows how to swim, 'no, otherwise.


d 'data.frame':   5405 obs. of  22 variables:
 $ id          : int  8020605 8020605 8020605 8020605 8020605 8020605 8020605 8020605 8020605 8020605 ...
 $ choice      : num  0 1 0 0 0 1 0 0 0 1 ...
 $ mode        : Factor w/ 4 levels "Helicopter","WaterTaxi",..: 2 3 4 1 2 3 4 1 2 3 ...
 $ cost        : num  59.3 34.7 57 99.9 59.3 ...
 $ risk        : num  2.55 4.43 3.88 18.41 2.55 ...
 $ weight      : num  1.47 1.47 1.47 1.47 1.47 ...
 $ seats       : num  0.8 0.4 0.8 0.8 0.8 ...
 $ noise       : num  1 0.2 1 1 1 ...
 $ crowdness   : num  1 0.2 1 1 1 ...
 $ convloc     : num  0.8 0.4 0.6 0.6 0.8 ...
 $ clientele   : num  1 0.8 1 1 1 ...
 $ chid        : num  1 1 1 2 2 2 2 3 3 3 ...
 $ african     : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
 $ lifeExp     : num  52 52 52 52 52 52 52 52 52 52 ...
 $ dwage       : num  19.4 19.4 19.4 19.4 19.4 ...
 $ iwage       : num  19.4 19.4 19.4 19.4 19.4 ...
 $ educ        : Factor w/ 2 levels "low","high": 2 2 2 2 2 2 2 2 2 2 ...
 $ fatalism    : num  6 6 6 6 6 6 6 6 6 6 ...
 $ gender      : Factor w/ 2 levels "female","male": 2 2 2 2 2 2 2 2 2 2 ...
 $ age         : int  33 33 33 33 33 33 33 33 33 33 ...
 $ haveChildren: Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
 $ swim        : Factor w/ 2 levels "no","yes": 2 2 2 2 



RT <- dfidx(RiskyTransport, choice = "choice",
            idx = list(c("chid", "id"), "mode"),
            idnames = c("chid", "alt"))
ml.rt <- mlogit(choice ~ cost + risk  + seats + noise + crowdness +
                convloc + clientele | 0, data = RT, weights = weight)

coef(ml.rt)[c("risk", "cost")]
        risk         cost 
-0.093907630 -0.009540895 
z<- mx.rt <- mlogit(choice ~ cost + risk  + seats + noise + crowdness +
                convloc + clientele | 0, data = RT, weights = weight,
                rpar = c(cost = 'zbt', risk = 'zbt'), R = 100,
                halton = NA, panel = TRUE)
summary(z)

Call:
mlogit(formula = choice ~ cost + risk + seats + noise + crowdness + 
    convloc + clientele | 0, data = RT, weights = weight, rpar = c(cost = "zbt", 
    risk = "zbt"), R = 100, halton = NA, panel = TRUE)

Frequencies of alternatives:choice
Helicopter  WaterTaxi      Ferry Hovercraft 
  0.026771   0.407697   0.431121   0.134412 

bfgs method
9 iterations, 0h:0m:7s 
g'(-H)^-1g = 2.67E-07 
gradient close to zero 

Coefficients :
            Estimate Std. Error  z-value  Pr(>|z|)    
cost      -0.0186805  0.0012935 -14.4415 < 2.2e-16 ***
risk      -0.1030286  0.0159221  -6.4708 9.749e-11 ***
seats      0.1084837  0.2333241   0.4649  0.641968    
noise      0.1422621  0.2288830   0.6215  0.534238    
crowdness -0.7157294  0.2225385  -3.2162  0.001299 ** 
convloc   -0.1497513  0.1971243  -0.7597  0.447446    
clientele -0.3314181  0.2540530  -1.3045  0.192055    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Log-Likelihood: -1581.6

random coefficients
     Min.    1st Qu.     Median        Mean     3rd Qu.        Max.
cost    0 0.01320909 0.01868047 -0.01868047 -0.05057003 -0.03736095
risk    0 0.07285225 0.10302864 -0.10302864 -0.27890953 -0.20605728

indpar <- fitted(mx.rt, type = "parameters")
head(indpar)

indpar$VSL <- with(indpar, risk / cost * 100)
quantile(indpar$VSL, c(0.025, 0.975))
##      2.5%     97.5% 
##  432.4199 1054.3428
mean(indpar$VSL)
## [1] 608.94

max(indpar$cost)
## [1] -0.002924437
max(indpar$VSL)
## [1] 3131.825
