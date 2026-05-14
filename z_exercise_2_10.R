### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 2 10 (p.114)                                                  ###
xr210 <- read.csv("xr210.csv", header = TRUE)
attach(xr210)
str(xr210)
plot(PRICE, QUANTITY)
### Problem (a) 7 Assumptions

### Problem (b) 
eq_b <- lm(QUANTITY ~ PRICE)
summary(eq_b)
Call:lm(formula = QUANTITY ~ PRICE)
Residuals:
     Min       1Q   Median       3Q      Max 
-14.2500  -6.8333   0.9167   4.7500  22.7500 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   774.92      47.67   16.26 1.61e-08 ***
PRICE        -693.33      50.07  -13.85 7.52e-08 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 10.62 on 10 degrees of freedom
Multiple R-squared:  0.9504,    Adjusted R-squared:  0.9455 
F-statistic: 191.7 on 1 and 10 DF,  p-value: 7.523e-08

### Problem (c)                                                            ###
sst<- sum (QUANTITY^2) - 1/12*(sum(QUANTITY))^2
sst
[1] 22760.25
sse<- (-693.33)^2*(sum(PRICE^2) -1/12*(sum(PRICE))^2)
sse
[1] 21631.79
sse/sst
[1] 0.9504198
res<- resid(eq_b)
ssr<- sum(res^2)
1 - ssr/sst
[1] 0.9504289
### Problem (d)                                                            ###
ssr/(12-2)
sqrt(ssr/(12-2))
[1] 10.62191 (sd) 
X<- PRICE
z<- solve(t(X)%*%X)
### Problem (e)
(-693.33 -0)/10.62191
[1] -65.27357
z*(ssr/(12-2))
sqrt(z*(ssr/(12-2)))
### Problem (f)
confint(eq_b)
                2.5 %    97.5 %
(Intercept)  668.7073  881.1260
PRICE       -804.9011 -581.7656
