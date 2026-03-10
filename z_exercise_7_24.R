### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 24 (p.720)                                                  ###
xr724 <- read.csv("xr724.csv", header =TRUE)
str(xr724)
attach(xr724)
### a Graphical interpretation and ADF Test                                ###
sp<- ts(SP, freq = 1, start = 1871)
plot(sp, main = "Time series", ylab = "SP")
div<- ts(DIV, freq =1, start = 1871)
plot(div, main = "Time series", ylab ="DIV")
### Problem (a) ADF Test                                                   ### 
library(tseries)
adf.test(sp)
   Augmented Dickey-Fuller Test
data:  sp
Dickey-Fuller = 2.1625, Lag order = 4, p-value = 0.99
alternative hypothesis: stationary
adf.test(div)
   Augmented Dickey-Fuller Test
data:  div
Dickey-Fuller = 3.3353, Lag order = 4, p-value = 0.99
alternative hypothesis: stationary
### Problem (b) Johansen Test and cointegration                              ###
vecm_data<- data.frame(sp, div) 
library(tsDyn)
eq_vecm<- VECM(vecm_data,
  lag =2,
  r = 1,
  include = "none",
  beta = NULL,
  estim = "ML",
  LRinclude = "both",
  exogen = NULL
)
         ECT         sp -1      div -1        sp -2      div -2
Equation sp  -0.09901525  0.0509918863 -21.1144600 -0.334375481 -1.60673220
Equation div -0.00110938 -0.0002719986   0.4628918 -0.005447073 -0.08934595
coefA(eq_vecm)
                 ECT
Equation sp  -0.09901525
Equation div -0.00110938
coefB(eq_vecm)
              r1
sp      1.0000000
div   -67.0436583
const  10.5687629
trend   0.1355738
coefPI(eq_vecm)
         sp        div       const         trend
Equation sp  -0.09901525 6.63834444 -1.04646868 -0.0134238722
Equation div -0.00110938 0.07437692 -0.01172478 -0.0001504029
