### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Exercise 7 19 (p.718)                                                  ###
xm722 <- read.csv("xm722.csv", header =TRUE)
str(xm722)
attach(xm722)
r<- lm(DUS3MT[2:624] ~ US3MTBIL[1:623])
summary(r)
Call: lm(formula = DUS3MT[2:624] ~ US3MTBIL[1:623])
Residuals:
    Min      1Q  Median      3Q     Max 
-4.7302 -0.1346 -0.0178  0.1460  2.8131 
Coefficients:
                 Estimate Std. Error t value Pr(>|t|)  
(Intercept)      0.078463   0.037163   2.111   0.0351 *
US3MTBIL[1:623] -0.014162   0.006349  -2.231   0.0261 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.4669 on 621 degrees of freedom
Multiple R-squared:  0.007948,  Adjusted R-squared:  0.00635 
F-statistic: 4.975 on 1 and 621 DF,  p-value: 0.02607
### a Residuals                                                            ###
res<- resid(r)
res<- ts(res, freq=12, start =c(1948,2))
plot(res, main= "Time series", ylab = "residuals")
hist(res)
acf(res)
acf(res^2)
### ARCH by ols                                                            ###
str(res)
res_sq<- res^2
res_sq<- res_sq[5:623]
res_lag<- res[4:622]
res_lag2<- res[3:621]
res_lag3<- res[2:620]
res_lag4<- res[1:619]
const<- rep(1,619)
res_lag_sq<- res_lag^2
e_ols<- lm(res_sq ~ res_lag^2)
summary(e_ols)
Call: lm(formula = res_sq ~ res_lag^2)
Residuals:
    Min      1Q  Median      3Q     Max 
-1.1645 -0.2280 -0.1872 -0.0949 21.1245 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.21912    0.04588   4.776 2.24e-06 ***
res_lag      0.36656    0.09806   3.738 0.000203 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.141 on 616 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.02218,   Adjusted R-squared:  0.02059 
F-statistic: 13.97 on 1 and 616 DF,  p-value: 0.0002027

e_ols_2<- lm(res_sq ~ res_lag^2+res_lag2^2)
summary(e_ols_2)
Call: lm(formula = res_sq ~ res_lag^2 + res_lag2^2)
Residuals:
    Min      1Q  Median      3Q     Max 
-1.1899 -0.2322 -0.1869 -0.0779 21.1057 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.21923    0.04578   4.789  2.1e-06 ***
res_lag      0.31116    0.10181   3.056  0.00234 ** 
res_lag2     0.20008    0.10182   1.965  0.04986 *  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.138 on 615 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.02828,   Adjusted R-squared:  0.02512 
F-statistic:  8.95 on 2 and 615 DF,  p-value: 0.0001475
e_ols_3<- lm(res_sq ~ res_lag^2+res_lag2^2+res_lag3^2)
summary(e_ols_3)
Call: lm(formula = res_sq ~ res_lag^2 + res_lag2^2 + res_lag3^2)
Residuals:
    Min      1Q  Median      3Q     Max 
-1.2533 -0.2331 -0.1874 -0.0746 21.0837 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.21926    0.04580   4.788 2.12e-06 ***
res_lag      0.32463    0.10376   3.129  0.00184 ** 
res_lag2     0.17680    0.10746   1.645  0.10042    
res_lag3     0.07059    0.10377   0.680  0.49663    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 1.139 on 614 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.02901,   Adjusted R-squared:  0.02427 
F-statistic: 6.115 on 3 and 614 DF,  p-value: 0.000421
e_ols_4<- lm(res_sq ~ res_lag^2+res_lag2^2+res_lag3^2+res_lag4^2)
summary(e_ols_4)
Call: lm(formula = res_sq ~ res_lag^2 + res_lag2^2 + res_lag3^2 + res_lag4^2)
Residuals:
    Min      1Q  Median      3Q     Max 
-1.3228 -0.2342 -0.1849 -0.0762 21.0573 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.21931    0.04582   4.786 2.13e-06 ***
res_lag      0.32445    0.10382   3.125  0.00186 ** 
res_lag2     0.18862    0.10935   1.725  0.08504 .  
res_lag3     0.05024    0.10935   0.459  0.64610    
res_lag4     0.06158    0.10384   0.593  0.55337    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 1.139 on 613 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.02957,   Adjusted R-squared:  0.02324 
F-statistic:  4.67 on 4 and 613 DF,  p-value: 0.001017
### AR(1) Model is good                                                   ###

### ML estimate                          ###
library(maxLik)
X<- cbind(const, res_lag^2)
y<- res_sq
negSSE <- function(beta) {
e <- y- X %*% beta
-crossprod(e)
}
m <- maxLik(negSSE, start=c(0,0))
summary(m, eigentol=1e-15)
Maximum Likelihood estimation
Newton-Raphson maximisation, 3 iterations
Return code 2: successive function values within tolerance limit (tol)
Log-Likelihood: -756.8839 
2  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  0.15815    0.02892   5.469 4.52e-08 ***
[2,]  0.27683    0.02470  11.208  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

X<- cbind(const, res_lag^2, res_lag2^2)
y<- res_sq
negSSE <- function(beta) {
e <- y- X %*% beta
-crossprod(e)
}
m2 <- maxLik(negSSE, start=c(0,0,0))
summary(m2, eigentol=1e-15)
Maximum Likelihood estimation
Newton-Raphson maximisation, 3 iterations
Return code 2: successive function values within tolerance limit (tol)
Log-Likelihood: -704.1042 
3  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  0.11641    0.02920   3.987 6.70e-05 ***
[2,]  0.20373    0.02572   7.922 2.33e-15 ***
[3,]  0.26407    0.02570  10.276  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’

X<- cbind(const, res_lag^2, res_lag2^2,res_lag3^2)
y<- res_sq
negSSE <- function(beta) {
e <- y- X %*% beta
-crossprod(e)
}
m3 <- maxLik(negSSE, start=c(0,0,0,0))
summary(m3, eigentol=1e-15)
Maximum Likelihood estimation
Newton-Raphson maximisation, 3 iterations
Return code 2: successive function values within tolerance limit (tol)
Log-Likelihood: -703.6189 
4  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  0.11335    0.02937   3.859 0.000114 ***
[2,]  0.19679    0.02665   7.385 1.53e-13 ***
[3,]  0.25872    0.02627   9.849  < 2e-16 ***
[4,]  0.02625    0.02664   0.985 0.324448    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
X<- cbind(const, res_lag^2, res_lag2^2,res_lag3^2,res_lag4^2)
y<- res_sq
negSSE <- function(beta) {
e <- y- X %*% beta
-crossprod(e)
}
m4 <- maxLik(negSSE, start=c(0,0,0,0,0))
summary(m4, eigentol=1e-15)

Maximum Likelihood estimation
Newton-Raphson maximisation, 3 iterations
Return code 2: successive function values within tolerance limit (tol)
Log-Likelihood: -703.1092 
5  free parameters
Estimates:
     Estimate Std. error t value  Pr(> t)    
[1,]  0.11640    0.02954   3.941 8.12e-05 ***
[2,]  0.19750    0.02665   7.411 1.25e-13 ***
[3,]  0.26568    0.02716   9.781  < 2e-16 ***
[4,]  0.03155    0.02716   1.161    0.245    
[5,] -0.02691    0.02666  -1.010    0.313    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’
### AR(2) Model is good                                                  ###