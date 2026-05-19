### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 4 11 (p.270)

e<- rt(50,3)
plot(e)
hist(e)
### Problem (a) 
y<- 1/2+e
plot(y)
hist(y)
summary(y)
 Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-2.8406697 -0.0001665  0.6448760  0.5275373  0.9538041  3.4188208 

### Problem (b)
eq<- lm(y ~ 1)
summary(eq)
Call:lm(formula = y ~ 1)
Residuals:
    Min      1Q  Median      3Q     Max 
-3.3682 -0.5277  0.1173  0.4263  2.8913 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)   0.5275     0.1561    3.38  0.00143 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.103 on 49 degrees of freedom
### H0 (b1 =0) is rejected.
sd of the sample mean
res<- resid(eq)
ssr<- sum(res^2)
sd_sample<- sqrt(ssr/49)
sd_sample

### Problem (c)                                                            ###
library(gmm)
x<- rep(1,50)
eq_gmm<- gmm(y ~ 1, x=x)
summary(eq_gmm)



### Problem (d)
cauchy

### Problem (e)
t3 distribution
