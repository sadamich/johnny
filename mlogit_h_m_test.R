https://cran.r-project.org/web/packages/mlogit/refman/mlogit.html#hmftest
library(mlogit)
## from Greene's Econometric Analysis p. 731
data("TravelMode", package = "AER")
TravelMode <- mlogit.data(TravelMode, choice = "choice", shape = "long",
                          alt.var = "mode", chid.var = "individual",
                          drop.index = FALSE)

## Create a variable of income only for the air mode
TravelMode$avinc <- with(TravelMode, (mode == 'air') * income)

## Estimate the model on all alternatives, with car as the base level
## like in Greene's book.
x <- mlogit(choice ~ wait + gcost + avinc, TravelMode, reflevel = "car")

## Estimate the same model for ground modes only (the variable avinc
## must be dropped because it is 0 for every observation

g <- mlogit(choice ~ wait + gcost, TravelMode, reflevel = "car",
            alt.subset = c("car", "bus", "train"))
## Compute the test
hmftest(x,g)
Hausman-McFadden test

data:  TravelMode
chisq = 33.337, df = 4, p-value = 1.019e-06
alternative hypothesis: IIA is rejected

