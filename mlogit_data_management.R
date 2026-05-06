library("mlogit")
install.packages("mlogit")
### Wide format
data("Train", package = "mlogit")
str(Train)
'data.frame':   2929 obs. of  11 variables:
 $ id       : int  1 1 1 1 1 1 1 1 1 1 ...
 $ choiceid : int  1 2 3 4 5 6 7 8 9 10 ...
 $ choice   : Factor w/ 2 levels "A","B": 1 1 1 2 2 2 2 2 1 1 ...
 $ price_A  : num  2400 2400 2400 4000 2400 4000 2400 2400 4000 2400 ...
 $ time_A   : num  150 150 115 130 150 115 150 115 115 150 ...
 $ change_A : num  0 0 0 0 0 0 0 0 0 0 ...
 $ comfort_A: num  1 1 1 1 1 0 1 1 0 1 ...
 $ price_B  : num  4000 3200 4000 3200 3200 2400 3200 3200 3200 4000 ...
 $ time_B   : num  150 130 115 150 150 130 115 150 130 115 ...
 $ change_B : num  0 0 0 0 0 0 0 0 0 0 ...
 $ comfort_B: num  1 1 0 0 0 0 1 0 1 0 ...
attach(Train)
Train$choiceid <- 1:nrow(Train)
head(Train, 3)
 id choiceid choice price_A time_A change_A comfort_A price_B time_B change_B
1  1        1      A    2400    150        0         1    4000    150        0
2  1        2      A    2400    150        0         1    3200    130        0
3  1        3      A    2400    115        0         1    4000    115        0
  comfort_B
1         1
2         1
3         0
head(Train, 4)
head(Train, 10)
head(Train, 20)
Tr <- dfidx(Train, shape = "wide", varying = 4:11, sep = "_",
            idx = list(c("choiceid", "id")), idnames = c(NA, "alt"))
head(Tr, 3)
~~~~~~~
 first 3 observations out of 5858 
~~~~~~~
  choice price time change comfort idx
1      A  2400  150      0       1 1:A
2      A  4000  150      0       1 1:B
3      A  2400  150      0       1 2:A

~~~ indexes ~~~~
  choiceid id alt
1        1  1   A
2        1  1   B
3        2  1   A
indexes:  1, 1, 2 



### Long format
data("ModeCanada", package = "mlogit")
str(ModeCanada)
head(ModeCanada)
MC <- dfidx(ModeCanada, subset = noalt == 4,
            alt.levels = c("train", "air", "bus", "car"))

MC <- dfidx(ModeCanada, subset = noalt == 4, idx = "case",
            alt.levels = c("train", "air", "bus", "car"))

MC <- dfidx(ModeCanada, subset = noalt == 4, idx = c("case", "alt"))

MC <- dfidx(ModeCanada, subset = noalt == 4)

MC <- dfidx(ModeCanada, subset = noalt == 4, idx = c("case", "alt"),
            drop.index = FALSE)
head(MC)
library("Formula")
f <- Formula(choice ~ cost | income + urban | ivt)
f2 <- Formula(choice ~ cost + ivt | income + urban)
f2 <- Formula(choice ~ cost + ivt | income + urban | 0)
f3 <- Formula(choice ~ 0 | income | 0)
f3 <- Formula(choice ~ 0 | income)
f4 <- Formula(choice ~ cost + ivt)
f4 <- Formula(choice ~ cost + ivt | 1)
f4 <- Formula(choice ~ cost + ivt | 1 | 0)
f5 <- Formula(choice ~ cost | income + 0 | ivt)
f5 <- Formula(choice ~ cost | income - 1 | ivt)
f <- Formula(choice ~ cost | income  | ivt)
mf <- model.frame(MC, f)
head(model.matrix(mf), 4)
statpval <- function(x){
    if (inherits(x, "anova")) 
        result <- as.matrix(x)[2, c("Chisq", "Pr(>Chisq)")]
    if (inherits(x, "htest")) result <- c(x$statistic, x$p.value)
    names(result) <- c("stat", "p-value")
    round(result, 3)
}

data(Fishing)
attach(Fishing)
str(Fishing)
'data.frame':   1182 obs. of  10 variables:
 $ mode         : Factor w/ 4 levels "beach","pier",..: 4 4 3 2 3 4 1 4 3 3 ...
 $ price.beach  : num  157.9 15.1 161.9 15.1 106.9 ...
 $ price.pier   : num  157.9 15.1 161.9 15.1 106.9 ...
 $ price.boat   : num  157.9 10.5 24.3 55.9 41.5 ...
 $ price.charter: num  182.9 34.5 59.3 84.9 71 ...
 $ catch.beach  : num  0.0678 0.1049 0.5333 0.0678 0.0678 ...
 $ catch.pier   : num  0.0503 0.0451 0.4522 0.0789 0.0503 ...
 $ catch.boat   : num  0.26 0.157 0.241 0.164 0.108 ...
 $ catch.charter: num  0.539 0.467 1.027 0.539 0.324 ...
 $ income       : num  7083 1250 3750 2083 4583
str(mode)
Factor w/ 4 levels "beach","pier",..: 4 4 3 2 3 4 1 4 3 3 ...
str(price.beach)
num [1:1182] 157.9 15.1 161.9 15.1 106.9 ...
str(income)
num [1:1182] 7083 1250 3750 2083 4583 ...
Fish<- dfidx(Fishing, varying=2:9,shape="wide",choice="mode")
head(Fish, 5)
~~~~~~~
 first 5 observations out of 4728 
~~~~~~~
   mode   income   price  catch    idx
1 FALSE 7083.332 157.930 0.0678 1:each
2 FALSE 7083.332 157.930 0.2601 1:boat
3  TRUE 7083.332 182.930 0.5391 1:rter
4 FALSE 7083.332 157.930 0.0503 1:pier
5 FALSE 1250.000  15.114 0.1049 2:each

~~~ indexes ~~~~
  id1     id2
1   1   beach
2   1    boat
3   1 charter
4   1    pier
5   2   beach
indexes:  1, 2 
summary(mlogit(mode~price+catch, data=Fish))
Call:
mlogit(formula = mode ~ price + catch, data = Fish, method = "nr")
Frequencies of alternatives:choice
  beach    boat charter    pier 
0.11337 0.35364 0.38240 0.15059 
nr method
7 iterations, 0h:0m:0s 
g'(-H)^-1g = 6.22E-06 
successive function values within tolerance limits 
Coefficients :
                      Estimate Std. Error  z-value  Pr(>|z|)    
(Intercept):boat     0.8713749  0.1140428   7.6408 2.154e-14 ***
(Intercept):charter  1.4988884  0.1329328  11.2755 < 2.2e-16 ***
(Intercept):pier     0.3070552  0.1145738   2.6800 0.0073627 ** 
price               -0.0247896  0.0017044 -14.5444 < 2.2e-16 ***
catch                0.3771689  0.1099707   3.4297 0.0006042 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log-Likelihood: -1230.8
McFadden R^2:  0.17823 
Likelihood ratio test : chisq = 533.88 (p.value = < 2.22e-16)
> 
