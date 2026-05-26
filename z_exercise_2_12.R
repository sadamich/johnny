### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 2 12 (p.115)
xr201<- read.csv("xr201.csv",header =TRUE)
str(xr201)
attach(xr201)
### Problem (a)                                                            ###
x<- RENDMARK
y<- RENDCYCO

mean(x)
[1] 0.808884
mean(y)
[1] 0.499826
var(x)
[1] 22.61871
var(y)
[1] 61.61613
cov(x,y)
[1] 26.48941

### Sample Moments                                                         ###
1/240* sum((x - mean(x))^2)
[1] 22.52446
1/240* sum ((y - mean(y))^2)
[1] 61.35939
1/240* sum((x - mean(x))*(y- mean(y)))
[1] 26.37904

### Problem (b) 

a<- mean(y) - 1.17113*mean(x)
a
[1] -0.4474823
b<- sum((x - mean(x))*(y - mean(y)))/sum((x - mean(x))*(x-mean(x)))
sum(b)
[1] 1.171128
res<- resid(capm)
ssr<- sum(res^2)
1/(240-2)*ssr
[1] 30.72217
sqrt(1/(240-2)*ssr)
[1] 5.542759                  (the standard error of the regression)
sst<- sum ((y - mean(y))^2)
1 - ssr/sst
[1] 0.5034802

capm <- lm(RENDCYCO ~ RENDMARK)
summary(capm)
Call:lm(formula = RENDCYCO ~ RENDMARK)
Residuals:
     Min       1Q   Median       3Q      Max 
-20.4122  -3.5274   0.2316   3.4774  15.1150 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.44748    0.36294  -1.233    0.219    
RENDMARK     1.17113    0.07539  15.535   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’
Residual standard error: 5.543 on 238 degrees of freedom
Multiple R-squared:  0.5035,    Adjusted R-squared:  0.5014 
F-statistic: 241.3 on 1 and 238 DF,  p-value: < 2.2e-16



