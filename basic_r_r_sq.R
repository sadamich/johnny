### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###

### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Seite 354 Die KQ Schaetzer

x<- c(3,5,6,8,9)
y<- c(9,10,13,12,15)
cov(x,y)
sd(x)
sd(y)
f_cor<- function(cov, sd1,sd2){
result<- cov/(sd1*sd2)
return(result)
}
f_cor(5.05,2.387467,2.387467)
[1] 0.8859651
cor(x,y)
[1] 0.8859649
sst<- (y - mean(y))^2
[1]  7.84  3.24  1.44  0.04 10.24
sum(sst)
[1] 22.8

b<- cor(x,y)*(sd(x)/sd(y))
b
[1] 0.8859649

eq<- lm(y~x)
summary(eq)
res<- resid(eq)
ssr<- sum(res^2)
ssr
[1] 4.903509

Call:
lm(formula = y ~ x)
Residuals:
       1        2        3        4        5 
 0.03509 -0.73684  1.37719 -1.39474  0.71930 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   6.3070     1.7557   3.592   0.0370 *
x             0.8860     0.2677   3.309   0.0454 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.278 on 3 degrees of freedom
Multiple R-squared:  0.7849,    Adjusted R-squared:  0.7132 
F-statistic: 10.95 on 1 and 3 DF,  p-value: 0.04543

sse<- 0.8859649^2*(x-mean(x))^2
sum(sse)
[1] 17.89649

### SST = SSE + SSR
sum(sst)
[1] 22.8
sum(sse)+sum(res^2)
[1] 22.8

### R^2
sum(sse)/sum(sst)
[1] 0.7849338
### r^2 = R^2
cor(x,y)^2
[1] 0.7849338
