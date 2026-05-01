### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 29  Primary metal industries ###
xm729<- read.csv("xm729.csv", header = TRUE)
attach(xm729)
str(xm729)
plot(LOGPROD_1)

### Single equations estimation: Compare with the panel 1 (p.690)         ###
eq1<- lm(LOGPROD_01~LOGLAB_01+LOGCAP_01)
summary(eq1)
Call:
lm(formula = LOGPROD_01 ~ LOGLAB_01 + LOGCAP_01)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.20302 -0.05930  0.02835  0.04497  0.18559 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.08791    0.35471  -3.067  0.00422 ** 
LOGLAB_01    0.96180    0.02408  39.948  < 2e-16 ***
LOGCAP_01    0.27270    0.05222   5.222 8.84e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
Residual standard error: 0.1007 on 34 degrees of freedom
Multiple R-squared:  0.9794,    Adjusted R-squared:  0.9781 
F-statistic: 806.3 on 2 and 34 DF,  p-value: < 2.2e-16

eq2<- lm(LOGPROD_02~LOGLAB_02+LOGCAP_02)
summary(eq2)
Call:
lm(formula = LOGPROD_02 ~ LOGLAB_02 + LOGCAP_02)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.70883 -0.10271  0.00485  0.09079  0.51166 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.363460   0.695877   1.959 0.058314 .  
LOGLAB_02   0.922977   0.236463   3.903 0.000427 ***
LOGCAP_02   0.005695   0.111640   0.051 0.959615    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1993 on 34 degrees of freedom
Multiple R-squared:  0.802,     Adjusted R-squared:  0.7904 
F-statistic: 68.87 on 2 and 34 DF,  p-value: 1.102e-12

eq3<- lm(LOGPROD_03~LOGLAB_03+LOGCAP_03)
summary(eq3)
Call:
lm(formula = LOGPROD_03 ~ LOGLAB_03 + LOGCAP_03)
Residuals:
     Min       1Q   Median       3Q      Max 
-0.18944 -0.05916 -0.01942  0.04798  0.26924 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.33998    0.28908  -1.176  0.24773    
LOGLAB_03    1.33022    0.22695   5.861  1.3e-06 ***
LOGCAP_03    0.18063    0.05652   3.196  0.00301 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1081 on 34 degrees of freedom
Multiple R-squared:  0.8092,    Adjusted R-squared:  0.7979 
F-statistic: 72.08 on 2 and 34 DF,  p-value: 5.91e-13

install.packages("plm")
library(plm)

LOGPROD<- LOGPROD_1+ LOGPROD_2+LOGPROD_3+LOGPROD_4+LOGPROD_5+
          LOGPROD_6+ LOGPROD_7+LOGPROD_8+LOGPROD_9+LOGPROD_10+
          LOGPROD_11+ LOGPROD_22+LOGPROD_13+LOGPROD_14+LOGPROD_15+
          LOGPROD_15+ LOGPROD_17+LOGPROD_18+LOGPROD_19+LOGPROD_20+
          LOGPROD_21+ LOGPROD_22+LOGPROD_23+LOGPROD_24+LOGPROD_25+
          LOGPROD_26

LOGLAB<-  LOGLAB_1+ LOGLAB_2+LOGLAB_3+LOGLAB_4+LOGLAB_5+
          LOGLAB_6+ LOGLAB_7+LOGLAB_8+LOGLAB_9+LOGLAB_10+
          LOGLAB_11+ LOGLAB_22+LOGLAB_13+LOGLAB_14+LOGLAB_15+
          LOGLAB_15+ LOGLAB_17+LOGLAB_18+LOGLAB_19+LOGLAB_20+
          LOGLAB_21+ LOGLAB_22+LOGLAB_23+LOGLAB_24+LOGLAB_25+
          LOGLAB_26
LOGCAP<-  LOGCAP_1+ LOGCAP_2+LOGCAP_3+LOGCAP_4+LOGCAP_5+
          LOGCAP_6+ LOGCAP_7+LOGCAP_8+LOGCAP_9+LOGCAP_10+
          LOGCAP_11+ LOGCAP_22+LOGCAP_13+LOGCAP_14+LOGCAP_15+
          LOGCAP_15+ LOGCAP_17+LOGCAP_18+LOGCAP_19+LOGCAP_20+
          LOGCAP_21+ LOGCAP_22+LOGCAP_23+LOGCAP_24+LOGCAP_25+
          LOGCAP_26
panel01<- plm(LOGPROD ~LOGLAB+LOGCAP  ,data = xm729, model = "pooling")
summary(panel01)
Pooling Model

Call:
plm(formula = LOGPROD ~ LOGLAB + LOGCAP, data = xm729, model = "pooling")

Balanced Panel: n = 37, T = 1, N = 37

Residuals:
    Min.  1st Qu.   Median  3rd Qu.     Max. 
-7.77110 -1.40605  0.34965  1.33931  5.08106 

Coefficients:
              Estimate Std. Error t-value  Pr(>|t|)    
(Intercept) -21.166472   9.981381 -2.1206   0.04133 *  
LOGLAB        0.837977   0.093611  8.9517 1.836e-10 ***
LOGCAP        0.290315   0.054183  5.3580 5.888e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    1254.2
Residual Sum of Squares: 242.56
R-Squared:      0.8066
Adj. R-Squared: 0.79523
F-statistic: 70.9027 on 2 and 34 DF, p-value: 7.4062e-13
summary(LOGLAB)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  22.20   26.52   30.96   31.02   35.11   39.96 
summary(LOGCAP)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  173.5   183.9   194.4   190.0   196.8   197.8 


LOGPROD<- as.matrix(LOGPROD_01+LOGPROD_02+LOGPROD_03)
LOGLAB<- as.matrix(LOGLAB_01+LOGLAB_02+LOGLAB_03)
LOGCAP<- as.matrix(LOGCAP_01+LOGCAP_02+LOGCAP_03)
eq_panel01<- pggls(LOGPROD~LOGLAB+LOGCAP, data = xm729, model = "pooling")
summary(eq_panel01)
Oneway (individual) effect General FGLS model

Call:
pggls(formula = LOGPROD ~ LOGLAB + LOGCAP, data = xm729, model = "pooling")

Balanced Panel: n = 37, T = 1, N = 37

Residuals:
       Min.     1st Qu.      Median     3rd Qu.        Max. 
-0.59720045 -0.19182276 -0.02137554  0.19041025  0.60007493 

Coefficients:
            Estimate Std. Error z-value  Pr(>|z|)    
(Intercept) 1.085518   0.580498  1.8700 0.0614870 .  
LOGLAB      1.033792   0.068943 14.9948 < 2.2e-16 ***
LOGCAP      0.108959   0.028085  3.8796 0.0001046 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Total Sum of Squares: 21.898
Residual Sum of Squares: 2.7555
Multiple R-squared: 0.87417
