https://cran.r-project.org/web/packages/urca/refman/urca.html#cajolst
https://users.ssc.wisc.edu/~behansen/718/LutkepohlSaikkonenTrenkler2004.pdf
library(urca)
data(denmark)
sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
sjd.lst <- cajolst(sjd, trend=TRUE, K=2, season=4)
summary(sjd.lst)
plot(sjd.lst)

