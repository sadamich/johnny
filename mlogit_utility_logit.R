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
