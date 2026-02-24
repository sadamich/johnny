library("mlogit")
install.packages("mlogit")
### Wide format
data("Train", package = "mlogit")
str(Train)
Train$choiceid <- 1:nrow(Train)
head(Train, 3)


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
