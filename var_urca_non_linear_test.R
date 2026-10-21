https://cran.r-project.org/web/packages/urca/refman/urca.html#lttest
library(urca)
Likelihood ratio test for no linear trend in VAR
data(denmark)
sjd <- as.matrix(denmark[, c("LRM", "LRY", "IBO", "IDE")])
sjd.vecm <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
lttest(sjd.vecm, r=1)
LR-test for no linear trend

H0: H*2(r<=1)
H1: H2(r<=1)
Test statistic is distributed as chi-square
with 3 degress of freedom
        test statistic p-value
LR test           1.98    0.58
plot(sjd.vecm)


#
data(finland)
sjf <- as.matrix(finland)
sjf.vecm <- ca.jo(sjf, ecdet = "none", type="eigen", K=2,
spec="longrun", season=4)
lttest(sjf.vecm, r=3)
LR-test for no linear trend

H0: H*2(r<=3)
H1: H2(r<=3)

Test statistic is distributed as chi-square
with 1 degress of freedom
        test statistic p-value
LR test           4.78    0.03
plot(sjf.vecm)