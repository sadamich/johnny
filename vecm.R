### VECM: Package urca###
### Source: https://cran.r-project.org/web/packages/urca/refman/urca.html#ca.jo ###
### Example 1 ###
data(denmark)
### period	Time index from 1974:Q1 until 1987:Q3 ###
### LRM: Logarithm of real money, M2              ###
### LRY: Logarithm of real income                 ###
### LPY: Logarithm of price deflator              ###
### IBO: Bond rate                                ###
### IDE: Bank deposit rate                        ###
head(denmark)
sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
str(sjd)
sjd.vecm <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
summary(sjd.vecm)

### Example 2 ###
data(finland)
### A data frame with 106 observations on the following ###
### 4 variables,ranging from 1958:Q2 until 1984:Q3      ###
### lrm1: Logarithm of real money, M1                   ###
### lny: Logarithm of real income                       ###
### lnmr: Marginal rate of interest                     ###
### difp: Inflation rate                                ###
head(finland)
sjf <- finland
str(sjf)
sjf.vecm <- ca.jo(sjf, ecdet = "none", type="eigen", K=2,
spec="longrun", season=4)
summary(sjf.vecm)


