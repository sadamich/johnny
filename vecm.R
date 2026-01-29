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
HD1 <- matrix(c(1, -1, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 1), c(5,3))
DA <- matrix(c(1,0,0,0, 0, 1, 0, 0, 0, 0, 0, 1), c(4,3))
summary(ablrtest(sjd.vecm, H=HD1, A=DA, r=1))
summary(alphaols(sjd.vecm))
summary(alphaols(sjd.vecm, reg.number=1))
DA <- matrix(c(1,0,0,0), c(4,1))
summary(alrtest(sjd.vecm, A=DA, r=1))
HD0 <- matrix(c(-1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1), c(5,4))
summary(blrtest(sjd.vecm, H=HD0, r=1))

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

### Example 3 ###
data(UKpppuip)
### A data frame of quarterly data ranging from 1971:Q1      ###
### until 1987:Q2. All variables are expressed in logarithms.###
### p1: UK wholesale price index                             ###
### p2: Trade weighted foreign whole sale price index        ###
### e12: UK effective exchange rate                          ###
### i1: Three-month treasury bill rate in the UK             ###
### i2: Three-month Eurodollar interest rate                 ###
### dpoil0: World oil price at period t                      ###
### dpoil1: World oil price at period t-1                    ###
head(UKpppuip)
attach(UKpppuip)
dat1 <- cbind(p1, p2, e12, i1, i2)
dat2 <- cbind(doilp0, doilp1)
H1 <- ca.jo(dat1, type='trace', K=2, season=4, dumvar=dat2)
H51 <- c(1, -1, -1, 0, 0)
H52 <- c(0, 0, 0, 1, -1)
summary(bh5lrtest(H1, H=H51, r=2))
summary(bh5lrtest(H1, H=H52, r=2))
H1 <- ca.jo(dat1, type='trace', K=2, season=4, dumvar=dat2)
H6 <- matrix(c(1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0), c(5,3))
bh6lrtest(z=H1, H=H6, r=2, r1=1, conv.val=0.0001, max.iter=50)
