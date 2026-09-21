https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#vcovCL
library(sandwich)
## Petersen's data
data("PetersenCL", package = "sandwich")
m <- lm(y ~ x, data = PetersenCL)
str(PetersenCL)
'data.frame':   5000 obs. of  4 variables:
 $ firm: int  1 1 1 1 1 1 1 1 1 1 ...
 $ year: int  1 2 3 4 5 6 7 8 9 10 ...
 $ x   : num  -1.11397 -0.08085 -0.23761 -0.15249 -0.00143 ...
 $ y   : num  2.252 1.242 -1.426 -1.109 0
plot(x,y)

# comparison with cross-section sandwiches
## HC0
all.equal(sandwich(m), vcovCL(m, type = "HC0", cadjust = FALSE))
[1] TRUE
## HC2
all.equal(vcovHC(m, type = "HC2"), vcovCL(m, type = "HC2"))
[1] TRUE
## HC3
all.equal(vcovHC(m, type = "HC3"), vcovCL(m, type = "HC3"))
[1] TRUE
## Innovation data
data("InstInnovation", package = "sandwich")
str(InstInnovation)
'data.frame':   6208 obs. of  25 variables:
 $ company        : Factor w/ 803 levels "3COM CORP","AAR CORP",..: 1 1 1 1 1 1 1 1 2 2 ...
 $ sales          : num  408 827 1295 2327 3147 ...
 $ acompetition   : num  0.884 0.884 0.884 0.884 0.884 ...
 $ competition    : num  0.888 0.894 0.884 0.
## replication of one-way clustered standard errors for model 3, Table I
## and model 1, Table II in Berger et al. (2017), see ?InstInnovation

## count regression formula
f1 <- cites ~ institutions + log(capital/employment) + log(sales) + industry + year

## model 3, Table I: Poisson model
## one-way clustered standard errors
tab_I_3_pois <- glm(f1, data = InstInnovation, family = poisson)
vcov_pois <- vcovCL(tab_I_3_pois, InstInnovation$company)
sqrt(diag(vcov_pois))[2:4]
           institutions log(capital/employment)              log(sales) 
            0.002406388             0.135953255             0.041523405 


## coefficient tables
if(require("lmtest")) {
coeftest(tab_I_3_pois, vcov = vcov_pois)[2:4, ]
}
                           Estimate  Std. Error   z value     Pr(>|z|)
institutions            0.009687237 0.002406388  4.025634 5.682195e-05
log(capital/employment) 0.482883549 0.135953255  3.551835 3.825545e-04
log(sales)              0.820317600 0.041523405 19.755548 7.187199e-87

## Not run: 
## model 1, Table II: negative binomial hurdle model
## (requires "pscl" or alternatively "countreg" from R-Forge)
library("pscl")
library("lmtest")
tab_II_3_hurdle <- hurdle(f1, data = InstInnovation, dist = "negbin")
#  dist = "negbin", zero.dist = "negbin", separate = FALSE)
vcov_hurdle <- vcovCL(tab_II_3_hurdle, InstInnovation$company)
sqrt(diag(vcov_hurdle))[c(2:4, 149:151)]
count_institutions count_log(capital/employment) 
                  0.002068504                   0.098748803 
             count_log(sales)             zero_institutions 
                  0.047443421                   0.002928844 
 zero_log(capital/employment)               zero_log(sales) 
                  0.111083100                   0.047965212 
coeftest(tab_II_3_hurdle, vcov = vcov_hurdle)[c(2:4, 149:151), ]
                                Estimate  Std. Error   t value     Pr(>|t|)
count_institutions            0.005804773 0.002068504  2.806267 5.028314e-03
count_log(capital/employment) 0.549520037 0.098748803  5.564827 2.739371e-08
count_log(sales)              0.439832896 0.047443421  9.270683 2.538186e-20
zero_institutions             0.010180140 0.002928844  3.475822 5.129482e-04
zero_log(capital/employment)  0.408626763 0.111083100  3.678568 2.366312e-04
zero_log(sales)               0.538426195 0.047965212 11.225348 5.998071e-29

