https://cran.r-project.org/web/packages/mlogit/vignettes/c3.rum.html

### Random utility model

### The distribution of the error terms
    ### the independence      mean = conditional mean
    ### a Gumbel distribution

### IIA property

### Interpretation
    ### Marginal rates of substitution
    ### Consumer’s surplus

library(mlogit)
data(ModeCanada)
str(ModeCanada)
MC <- dfidx(ModeCanada, subset = noalt == 4)
ml.MC1 <- mlogit(choice ~ cost + freq + ovt | income | ivt, MC)

ml.MC1b <- mlogit(choice ~ cost + freq + ovt | income | ivt, ModeCanada,
                  subset = noalt == 4, idx = c("case", "alt"))

MC$time <- with(MC, ivt + ovt)
ml.MC1 <- mlogit(choice ~ cost + freq | income | time, MC, 
                 alt.subset = c("car", "train", "air"),
                 reflevel = "car")

summary(ml.MC1)
head(fitted(ml.MC1, type = "outcome"))
head(fitted(ml.MC1, type = "probabilities"), 4)
sum(log(fitted(ml.MC1, type = "outcome")))
apply(fitted(ml.MC1, type = "probabilities"), 2, mean)
head(predict(ml.MC1, shape = "wide"))

### The modificatin of the model
NMC <- MC
NMC$time[NMC$alt == "train"] <- 0.8 * NMC$time[NMC$alt == "train"]
Oprob <- fitted(ml.MC1, type = "probabilities")
Nprob <- predict(ml.MC1, newdata = NMC, shape = "wide")
rbind(old = apply(Oprob, 2, mean), new = apply(Nprob, 2, mean))
head(Nprob[, "air"] / Nprob[, "car"])
head(Oprob[, "air"] / Oprob[, "car"])
### The surplus (der Ueberschuss) 
ivbefore <- logsum(ml.MC1)
ivafter <- logsum(ml.MC1, data = NMC)
surplus <- - (ivafter - ivbefore) / coef(ml.MC1)["cost"]
summary(surplus)
effects(ml.MC1, covariate = "income", type = "ar")
effects(ml.MC1, covariate = "cost", type = "rr")
coef(ml.MC1)[grep("time", names(coef(ml.MC1)))] /coef(ml.MC1)["cost"] * 60 


### Predictions and marginal effects

ids <- unique(ModeCanada$case)
set.seed(1L)
ids <- sample(ids, 50)
smallMC <- subset(ModeCanada, case %in% ids)
ml <- mlogit(choice ~ cost | income, smallMC, alt.subset = c("car", "train", "air"))
summary(ml)

p<- preds(ml)

