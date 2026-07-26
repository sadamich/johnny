install.packages("mlogit")
library("mlogit")
Format
id: individual identifient,
choiceid: choice identifient,
choice: one of 'A' or 'B',
price_z: price of proposition z (z = 'A', 'B') in cents of guilders,
time_z: travel time of proposition z (z = 'A', 'B') in minutes,
comfort_z: comfort of proposition z (z = 'A', 'B'), 0, 1 or 2 in decreasing comfort order,
change_z: number of changes for proposition z (z = 'A', 'B').

### Wide format
2(A or B) * 4 (attributes)
2^4
[1] 16 scenarios
data("Train", package = "mlogit")
str(Train)
A sample of 235 Dutch individuals facing 2929 choice situations
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
### the two trains: the model A or B                                                      ###
### Index variables                                                       ###
str(id)
 int [1:2929] 1 1 1 1 1 1 1 1 1     ### the individuals (n)                 
str(id[id==1])
int [1:10] 1 1 1 1 1 1 1 1 1 1
str(id[id==5])
[1:15] 5 5 5 5 5 5 5 5 5 5 ...
str(id[id==50])
int [1:13] 50 50 50 50 50 50 50 50 50 50 ...
str(id[id==150])
[1:10] 150 150 150 150 150 150 150 150 150 150
str(id[id==200])
int [1:12] 200 200 200 200 200 200 200 200 200 200 .

str(choiceid)
int [1:2929] 1 2 3 4 5 6 7 8 9 10   ### the choice (m)  
str(choice)
 Factor w/ 2 levels "A","B": 1 1 1 2 2 2 2 2 1 1 ### the choice (y = m) [2929] .
### alternative specific
str(price_A)
 num [1:2929] 2400 2400 2400 4000 2400 4000 ### the explanatory variables (x1)
str(time_A)
 num [1:2929] 150 150 115 130 150 115 150 115 ### the explanatory variable(x2)
str(change_A)
num [1:2929] 0 0 0 0 0 0 0 0 0 0          ### the explanatory variable (x3)
str(comfort_A)
num [1:2929] 1 1 1 1 1 0 1 1 0 1         ### the explanatory variable (x4)
str(price_B)
 num [1:2929] 4000 3200 4000 3200 3200   ### the explanatory variable (x5)
str(time_B)
num [1:2929] 150 130 115 150 150 130 115 ### the explanatory variable (x6)
str(change_B)
 num [1:2929] 0 0 0 0 0 0 0 0 0 0        ### the explanatory variable (x7)
str(comfort_B)
 num [1:2929] 1 1 0 0 0 0 1 0 1 0        ### the explanatory variable (x8)
Train$choiceid <- 1:nrow(Train)
head(Train, 30)
### the wide format has rows of choiceid
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
attach(ModeCanada)
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
 $ noalt : int  2 2 2 2 2 2 2 2 2 2 ..

str(noalt)
int [1:15520] 2 2 2 2 2 2 2 2 2 2       ### the explanatory variable (x)
hist(noalt)
str(alt)  ### the four choices : train, air, bus, car 
Factor w/ 4 levels "train","air",: 1 4  ### the explanatory variable (m)
hist(alt)
alt_n<- as.numeric(alt)
hist(alt_n)
str(choice)
int [1:15520] 0 1 0 1 0 1 0 1 0 1       ### the depandent variabe (y = m) 
str(dist)
int [1:15520] 83 83 83 83 83 83 83 83   ### the explanatory variable (x1)
hist(dist)
str(cost)
num [1:15520] 28.2 15.8 28.2 15.8 28.2  ### the explanatory variable (x2)
hist(cost)
str(ivt)
 int [1:15520] 50 61 50 61 50 61 50 61  ### the explanatory variable (x3)
hist(ivt)
str(ovt)
int [1:15520] 66 0 66 0 66 0 66 0 66 0  ### the explanatory variable (x4)
hist(ovt)
str(freq)
 int [1:15520] 4 0 4 0 4 0 4 0 4 0      ### the explanatory variable (x5)
hist(freq)
str(income)
int [1:15520] 45 45 25 25 70 70 70 70   ### the explanatory variable (x6)
hist(income)
str(urban)
 int [1:15520] 0 0 0 0 0 0 0 0 0 0      ### the explanatory variable (x7)
hist(urban)
head(ModeCanada)
MC <- dfidx(ModeCanada, subset = noalt == 4,
            alt.levels = c("train", "air", "bus", "car"))
str(MC)
Classes ‘tbl_dfidx’, ‘dfidx’, ‘tbl_df’, ‘tbl’ and 'data.frame': 11116 obs. of  10 variables:
 $ choice: 'xseries' int  0 1 0 0 0 1 0 0 0 1 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ dist  : 'xseries' num  377 377 377 377 377 377 377 377 377 377 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ cost  : 'xseries' num  58.2 142.8 27.5 71.6 58.2 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ ivt   : 'xseries' num  215 56 301 262 215 56 301 262 215 56 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ ovt   : 'xseries' num  74 85 63 0 74 85 63 0 74 85 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ freq  : 'xseries' num  4 9 8 0 4 9 8 0 4 9 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ income: 'xseries' num  45 45 45 45 70 70 70 70 35 35 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ urban : 'xseries' num  0 0 0 0 0 0 0 0 0 0 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ noalt : 'xseries' int  4 4 4 4 4 4 4 4 4 4 ...
  ..- attr(*, "idx")=Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':     11116 obs. of  2 variables:
  .. ..$ case: int [1:11116] 109 109 109 109 110 110 110 110 111 111 ...
  .. ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  .. ..- attr(*, "ids")= num [1:2] 1 2
 $ idx   :Classes ‘idx’, ‘tbl_df’, ‘tbl’ and 'data.frame':        11116 obs. of  2 variables:
  ..$ case: int  109 109 109 109 110 110 110 110 111 111 ...
  ..$ alt : Factor w/ 4 levels "train","air",..: 1 2 3 4 1 2 3 4 1 2 ...
  ..- attr(*, "ids")= num [1:2] 1 2
 - attr(*, "clseries")= chr "xseries"
attach(noalt)
hist(noalt)
str(noalt)
MC <- dfidx(ModeCanada, subset = noalt == 4, idx = "case",
            alt.levels = c("train", "air", "bus", "car
MC <- dfidx(ModeCanada, subset = noalt == 4, idx = c("case", "alt"))
MC <- dfidx(ModeCanada, subset = noalt == 4)
MC <- dfidx(ModeCanada, subset = noalt == 4, idx = c("case", "alt"),
            drop.index = FALSE)
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

### The model Fishing                                                      ###
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
Factor "beach","pier",: 4 4 3 ### the dependent variable = the option (y = m)
str(price.beach)
num [1:1182] 157.9 15.1 161.9 ### the explanatory variable (x1)
str(price.pier)
num [1:1182] 157.9 15.1 161.9 ### the explanatory variable (x2)
str(price.boat)
num [1:1182] 157.9 10.5 24.3  ### the explanatory variable (x3)
str(price.charter)
num [1:1182] 182.9 34.5 59.3  ### the explanatory variable (x4)
str(catch.beach)
num [1:1182] 0.0678 0.1049    ### the explanatory variable (x5)
str(catch.pier) 
num [1:1182] 0.0503 0.0451    ### the explanatory variable (x6)
str(catch.boat)
num [1:1182] 0.26 0.157 0.241 ### the explanatory variable (x7)
str(catch.charter)
num [1:1182] 0.539 0.467      ### the explanatory variable (x8)
str(income)
num [1:1182] 7083 1250 3750   ### the explanatory variable (x9)
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

