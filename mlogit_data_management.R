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
