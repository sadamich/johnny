https://cran.r-project.org/web/packages/mlogit/vignettes/c6.mprobit.html

### Multinomial probit
### The model
### Identification
### Simulations   ( GHK algorithm)


library(mlogit)
data(Fishing)
str(Fishing)
Fish <- dfidx(Fishing, varying = 2:9, choice = "mode",
              idnames = c("chid", "alt"))
Fish.mprobit <- mlogit(mode~price | income | catch, Fish, probit = TRUE,
                       alt.subset=c('beach', 'boat','pier'))
summary(Fish.mprobit)
    

