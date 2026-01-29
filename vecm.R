### VECM: Package urca###
### Source: https://cran.r-project.org/web/packages/urca/refman/urca.html#ca.jo ###
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
