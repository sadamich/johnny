### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 

### Example 7 32 Interes and bonds rates (p.707)                           ###
xm511<- read.csv("xm511.csv", header = TRUE)
str(xm511)
attach(xm511)
detach(xm511)
DAAA_90<- DAAA[481:600]
DUS3MT_90<- DUS3MT[481:600]
DAAA1<- DAAA[480:599]
DUS3MT1<- DUS3MT[480:599]
DAAA2<- DAAA[479:598]
DUS3MT2<- DUS3MT[479:598]

panel01<- lm(DAAA_90~DUS3MT_90+DUS3MT1+DAAA1)
summary(panel01)
### Panel 1 (p.708) 
lm(formula = DAAA_90 ~ DUS3MT_90 + DUS3MT1 + DAAA1)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.36672 -0.09405 -0.01975  0.09760  0.27544 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.003077   0.013533  -0.227 0.820544    
DUS3MT_90    0.330442   0.080906   4.084 8.17e-05 ***
DUS3MT1     -0.126609   0.084199  -1.504 0.135379    
DAAA1        0.307879   0.090652   3.396 0.000936 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.1468 on 116 degrees of freedom
Multiple R-squared:  0.243,     Adjusted R-squared:  0.2234 
F-statistic: 12.41 on 3 and 116 DF,  p-value: 4.253e-07

### 2 stage estimate                                                       ###
const<- rep(1, 120)
z<- cbind(const,DUS3MT1,DUS3MT2,DAAA1,DAAA2)
library("gmm")
eq_tsls<- tsls(DAAA_90~DUS3MT_90+DUS3MT1+DAAA1, x=z)                                               ###
summary(eq_tsls)
### panel 2 (p.708) tsls(g = DAAA_90 ~ DUS3MT_90 + DUS3MT1 + DAAA1, x = z) ###
Method:  Two Stage Least Squares(Meat type = Classical) 
Coefficients:
             Estimate    Std. Error  t value     Pr(>|t|)  
(Intercept)  -0.0069825   0.0156905  -0.4450158   0.6563083
DUS3MT_90    -0.0136143   0.5246153  -0.0259511   0.9792964
DUS3MT1      -0.0331636   0.1671476  -0.1984091   0.8427250
DAAA1         0.3859292   0.1525559   2.5297559   0.0114142
J-Test: degrees of freedom is 1 
                J-test   P-value
Test E(g)=0:    0.34909  0.55463
 First stage F-statistics: 
DUS3MT_90 : F( 4 ,  115 ) =  6.482529  (P-Vavue =  9.684995e-05 )

panel03<- lm(DUS3MT_90~DAAA_90+DAAA1+DUS3MT1)
summary(panel03)
### Panel 3 (p.708) lm(formula = DUS3MT_90 ~ DAAA_90 + DAAA1 + DUS3MT1)    ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.57296 -0.07975  0.00488  0.08180  0.54448 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.008754   0.014502  -0.604  0.54728    
DAAA_90      0.380471   0.093156   4.084 8.17e-05 ***
DAAA1        0.081194   0.101716   0.798  0.42636    
DUS3MT1      0.285623   0.087285   3.272  0.00141 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1575 on 116 degrees of freedom
Multiple R-squared:  0.2664,    Adjusted R-squared:  0.2474 
F-statistic: 14.04 on 3 and 116 DF,  p-value: 7.172e-08

eq_tsls2<- tsls(DUS3MT_90~DAAA_90+DAAA1+DUS3MT1, x=z)                                               ###
summary(eq_tsls2)
### Panel 4 (p.708)tsls(g = DUS3MT_90 ~ DAAA_90 + DAAA1 + DUS3MT1, x = z)  ###
Method:  Two Stage Least Squares(Meat type = Classical) 
Coefficients:
             Estimate   Std. Error  t value    Pr(>|t|) 
(Intercept)  -0.012350   0.021047   -0.586807   0.557333
DAAA_90      -0.146298   1.938574   -0.075467   0.939843
DAAA1         0.282863   0.749923    0.377190   0.706033
DUS3MT1       0.266205   0.121696    2.187460   0.028709
J-Test: degrees of freedom is 1 
                J-test    P-value 
Test E(g)=0:    2.952122  0.085765
 First stage F-statistics: 
DAAA_90 : F( 4 ,  115 ) =  4.551734  (P-Vavue =  0.001893333 )

### 1 stage   X-> Z                                                       ###
iv_dus<- lm(DUS3MT_90~DUS3MT1+DUS3MT2+DAAA1+DAAA2)
fit_dus<- fitted(iv_dus)
### 1 stage   X-> Z                                                       ###
iv_daaa<- lm(DAAA_90~DAAA1+DAAA2+DUS3MT1+DUS3MT2)
fit_daaa<- fitted(iv_daaa)
### 2 stage   Y-> fitX                                                    ###
eq2<- lm(DAAA_90~fit_dus+DUS3MT1+DAAA1)
summary(eq2)
### Panel 2 (p.708)                                                       ###
lm(formula = DAAA_90 ~ fit_dus + DUS3MT1 + DAAA1)
Residuals:
    Min      1Q  Median      3Q     Max 
-0.3411 -0.1029 -0.0240  0.1098  0.3523 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.006983   0.015608  -0.447   0.6554  
fit_dus     -0.013614   0.521862  -0.026   0.9792  
DUS3MT1     -0.033164   0.166270  -0.199   0.8423  
DAAA1        0.385929   0.151755   2.543   0.0123 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.157 on 116 degrees of freedom
Multiple R-squared:  0.1341,    Adjusted R-squared:  0.1117 
F-statistic:  5.99 on 3 and 116 DF,  p-value: 0.000785
### 2 stage     Y-> fitX                                                   ###
eq4<- lm(DUS3MT_90~fit_daaa+DAAA1+DUS3MT1)
summary(eq4)
### Panel 4 (p.708)                                                        ###
lm(formula = DUS3MT_90 ~ fit_daaa + DAAA1 + DUS3MT1)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.56716 -0.09539 -0.00928  0.11267  0.55756 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.01235    0.01993  -0.620   0.5367  
fit_daaa    -0.14630    1.83561  -0.080   0.9366  
DAAA1        0.28286    0.71009   0.398   0.6911  
DUS3MT1      0.26621    0.11523   2.310   0.0226 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1685 on 116 degrees of freedom
Multiple R-squared:  0.161,     Adjusted R-squared:  0.1393 
F-statistic: 7.418 on 3 and 116 DF,  p-value: 0.0001372