### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)
str(xm609)
### Exhibit 6 14 a and b (p.512)                                           ###
hist(STRIKEDUR)
summary(STRIKEDUR)
LOGSTRIKE<- log(STRIKEDUR)
hist(LOGSTRIKE)
summary(LOGSTRIKE)
panel03<- lm(LOGSTRIKE ~ 1)
summary(panel03)
### Panel 3 (p.517) Call: lm(formula = LOGSTRIKE ~ 1)                      ###
Residuals:
    Min      1Q  Median      3Q     Max 
-3.1045 -0.7780  0.1914  0.8319  2.2708 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   3.1045     0.1644   18.88   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.295 on 61 degrees of freedom
panel04<- lm(LOGSTRIKE~PROD)
summary(panel04)
### Panel 4 (p.517) Call:lm(formula = LOGSTRIKE ~ PROD)                    ###
Residuals:
    Min      1Q  Median      3Q     Max 
-2.6135 -0.7850  0.2115  0.9307  1.9917 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)    3.206      0.161  19.912  < 2e-16 ***
PROD          -9.181      3.404  -2.697  0.00907 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.233 on 60 degrees of freedom
Multiple R-squared:  0.1081,    Adjusted R-squared:  0.09324 
F-statistic: 7.273 on 1 and 60 DF,  p-value: 0.009073
install.packages("survival")
library(survival)
eq_suv<- coxph(Surv(log(STRIKECENS80)) ~ PROD, xm609)
summary(eq_suv)
