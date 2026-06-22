https://cran.r-project.org/web/packages/mlogit/vignettes/c3.rum.html
library(mlogit)
data(NOx)
str(NOx)
attach(NOx)

NOx$kdereg <- with(NOx, kcost * (env == "deregulated"))
NOxml <- dfidx(NOx, idx = c(id = "chid", "alt"))
ml.pub <- mlogit(choice ~ post + cm + lnb + vcost +
                     kcost + kcost:age | - 1,
                 subset = available & env == "public",
                 data = NOxml)
ml.reg <- update(ml.pub, subset = available & env == "regulated")
ml.dereg <- update(ml.pub, subset = available & env == "deregulated")
ml.pool <- ml.dereg
ml.pool <- mlogit(choice ~ post + cm + lnb + vcost + kcost +
                      kcost:age + kdereg | - 1 | 0 | env,
                  subset = available == 1, data = NOxml,
                  method = "bhhh")

summary(ml.pool)