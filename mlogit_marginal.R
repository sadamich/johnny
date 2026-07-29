https://cran.r-project.org/web/packages/mlogit/refman/mlogit.html#effects.mlogit
library(mlogit)

data("Fishing", package = "mlogit")
Fish <- dfidx(Fishing, varying = 2:9, choice = "mode")
m <- mlogit(mode ~ price | income | catch, data = Fish)
# compute a data.frame containing the mean value of the covariates in
# the sample
z <- with(Fish, data.frame(price = tapply(price, idx(m, 2), mean),
                           catch = tapply(catch, idx(m, 2), mean),
                           income = mean(income)))
# compute the marginal effects (the second one is an elasticity
## IGNORE_RDIFF_BEGIN
effects(m, covariate = "income", data = z)
   beach          boat       charter          pier 
 1.132965e-06  3.113069e-05 -2.408677e-05 -8.176877e-06 

## IGNORE_RDIFF_END
effects(m, covariate = "price", type = "rr", data = z)
    beach       boat    charter       pier
beach   -2.4634447  0.1512128  0.1512128  0.1512128
boat     0.5797049 -0.8172610  0.5797049  0.5797049
charter  0.9741351  0.9741351 -1.1590941  0.9741351
pier     0.1844507  0.1844507  0.1844507 -2.4302068

effects(m, covariate = "catch", type = "ar", data = z)
               beach        boat     charter         pier
beach    0.040943135 -0.01803326 -0.01984425 -0.003065622
boat    -0.010447074  0.10568073 -0.08249023 -0.012743428
charter -0.012623645 -0.09057996  0.11860203 -0.015398428
pier    -0.001887074 -0.01354055 -0.01490036  0.030327981
