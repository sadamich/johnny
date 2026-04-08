### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm526<- read.csv("xm526.csv", header=TRUE)
str(xm526)
attach(xm526)
detach(xm526)
### Problem a (p.434)                                                      ###
t<- 1:195
eq_t<- lm(IP ~ t)
IP<- ts(IP, freq=4, start =1950)
t<- ts(t,  freq=4, start =1950)
summary(eq_t)
### Trend model Call: lm(formula = IP ~ t)                                 ###
Residuals:
    Min      1Q  Median      3Q     Max 
-10.629  -3.071  -0.345   2.644  16.334 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 18.186538   0.637854   28.51   <2e-16 ***
t            0.507957   0.005644   90.00   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 4.436 on 193 degrees of freedom
Multiple R-squared:  0.9767,    Adjusted R-squared:  0.9766 
F-statistic:  8100 on 1 and 193 DF,  p-value: < 2.2e-16
library(strucchange)
eq_cusum<- efp(IP~t,type = "Rec-CUSUM")
plot(eq_cusum,freq=4)
sctest(eq_cusum)
Recursive CUSUM test
data:  eq_cusum
S = 1.8942, p-value = 1.148e-06
### The parametr is not constant.                                          ###
d2<- c(0,1,0,0)
D2<- rep(d2,49)
d3<- c(0,0,1,0)
D3<- rep(d3, 49)
d4<- c(0,0,0,1)
D4<- rep(d4,49)
eq_season<- lm(IP ~ t+D2[1:195]+D3[1:195]+D4[1:195])
summary(eq_season)
### Seasonal dummies Call:                                                 ###
### lm(formula = IP ~ t + D2[1:195] + D3[1:195] + D4[1:195])               ###
Residuals:
     Min       1Q   Median       3Q      Max 
-10.4311  -3.1058  -0.3588   2.6126  16.0457 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 17.90561    0.84280  21.245   <2e-16 ***
t            0.50791    0.00568  89.421   <2e-16 ***
D2[1:195]    0.47044    0.90197   0.522    0.603    
D3[1:195]    0.57799    0.90202   0.641    0.522    
D4[1:195]    0.08923    0.90666   0.098    0.922    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 4.464 on 190 degrees of freedom
Multiple R-squared:  0.9768,    Adjusted R-squared:  0.9763 
F-statistic:  2000 on 4 and 190 DF,  p-value: < 2.2e-16
plot(decompose(IP))
### Problem c Outlier                                                      ###
res<- resid(eq_season)
res<- ts(res, freq= 4, start=1950)
plot(res)
hist(res)
### Problem d the yearly growth rate                                       ###
dip<- IP[5:195] - IP[1:191]
eq_d<- lm(dip ~ 1)
summary(eq_d)
### The fourth difference Call: lm(formula = dip ~ 1)                      ###
Residuals:
     Min       1Q   Median       3Q      Max 
-10.6982  -1.9802   0.5468   2.1718   7.4538 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   2.2192     0.2325   9.543   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 3.214 on 190 degrees of freedom
res_d<- resid(eq_d)
res_d<- ts(res_d, freq=4, start=1951)
plot(res_d)