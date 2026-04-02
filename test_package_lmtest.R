### https://cran.r-project.org/web/packages/lmtest/refman/lmtest.html ###
library(lmtest)

### fit two competing, non-nested models and their encompassing            ###
### model for aggregate consumption, as in Greene (1993),                  ###
### Examples 7.11 and 7.12                                                 ###  
### load data and compute lags                                             ###
data(USDistLag)
usdl <- na.contiguous(cbind(USDistLag, lag(USDistLag, k = -1)))
colnames(usdl) <- c("con", "gnp", "con1", "gnp1")
### Model 1 C(t) = a0 + a1*Y(t) + a2*C(t-1) + u                            ###                                 
fm1 <- lm(con ~ gnp + con1, data = usdl)
### Model 2 C(t) = b0 + b1*Y(t) + b2*Y(t-1) + v                            ###
fm2 <- lm(con ~ gnp + gnp1, data = usdl)
### Model 3  Encompassing model                                            ###
fm3 <- lm(con ~ gnp + con1 + gnp1, data = usdl)

### a simple ANOVA for fm3 vs. fm2                                         ###
waldtest(fm3, fm2)
Wald test
Model 1: con ~ gnp + con1 + gnp1
Model 2: con ~ gnp + gnp1
  Res.Df Df      F    Pr(>F)    
1     15                        
2     16 -1 27.093 0.0001067 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.
anova(fm3, fm2)
Analysis of Variance Table
Model 1: con ~ gnp + con1 + gnp1
Model 2: con ~ gnp + gnp1
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1     15  406.9                                  
2     16 1141.8 -1   -734.93 27.093 0.0001067 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
### as df = 1, the test is equivalent to the corresponding t test in       ###
coeftest(fm3)
t test of coefficients:
             Estimate Std. Error t value  Pr(>|t|)    
(Intercept)  6.334762   9.574496  0.6616 0.5182445    
gnp          0.367170   0.048676  7.5432 1.763e-06 ***
con1         1.044563   0.200682  5.2051 0.0001067 ***
gnp1        -0.391718   0.110488 -3.5454 0.0029371 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### various equivalent specifications of the two models                    ###
waldtest(fm3, fm2)
waldtest(fm3, 2)
waldtest(fm3, "con1")
waldtest(fm3, . ~ . - con1)

### comparing more than one model                                          ###  
### (equivalent to the encompassing test)                                  ###
waldtest(fm1, fm3, fm2)
Wald test
Model 1: con ~ gnp + con1
Model 2: con ~ gnp + con1 + gnp1
Model 3: con ~ gnp + gnp1
  Res.Df Df      F    Pr(>F)    
1     16                        
2     15  1 12.569 0.0029371 ** 
3     16 -1 27.093 0.0001067 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 

encomptest(fm1, fm2)
Encompassing test
Model 1: con ~ gnp + con1
Model 2: con ~ gnp + gnp1
Model E: con ~ gnp + con1 + gnp1
          Res.Df Df      F    Pr(>F)    
M1 vs. ME     15 -1 12.569 0.0029371 ** 
M2 vs. ME     15 -1 27.093 0.0001067 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### using the asymptotic Chisq statistic                                   ###
waldtest(fm3, fm2, test = "Chisq")
Wald test
Model 1: con ~ gnp + con1 + gnp1
Model 2: con ~ gnp + gnp1
  Res.Df Df  Chisq Pr(>Chisq)    
1     15                         
2     16 -1 27.093  1.939e-07 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
### plugging in a HC estimator                                             ###
if(require(sandwich)) waldtest(fm3, fm2, vcov = vcovHC)  
Loading required package: sandwich
Wald test
Model 1: con ~ gnp + con1 + gnp1
Model 2: con ~ gnp + gnp1
  Res.Df Df      F   Pr(>F)   
1     15                      
2     16 -1 9.7456 0.006998 **
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 