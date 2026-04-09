### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm531<- read.csv("xm531.csv", header=TRUE)
str(xm531)
attach(xm531)
detach(xm531)
### Problem a (p.435)                                                      ###
eq<- lm(GC[1:26]~ PG[1:26]+RI[1:26])
summary(eq)
lm(formula = GC[1:26] ~ PG[1:26] + RI[1:26])
Residuals:
      Min        1Q    Median        3Q       Max 
-0.048858 -0.012934 -0.000906  0.018407  0.032896 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.08931    0.07992   63.68   <2e-16 ***
PG[1:26]    -0.51313    0.02444  -21.00   <2e-16 ***
RI[1:26]     0.54067    0.02433   22.22   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.02142 on 23 degrees of freedom
Multiple R-squared:  0.9852,    Adjusted R-squared:  0.9839 
F-statistic: 764.6 on 2 and 23 DF,  p-value: < 2.2e-16
### Problem b                                                               ###
res<- resid(eq)
plot(res)
eq_cusum<- efp(GC[1:26] ~ PG[1:26] + RI[1:26],type = "Rec-CUSUM")
plot(eq_cusum)
sctest(eq_cusum)
    Recursive CUSUM test
data:  eq_cusum
S = 0.43884, p-value = 0.7645
### Problem c                                                               ###
pg_sq<- PG[1:26]^2
ri_sq<- RI[1:26]^2
eq_h<- lm(res^2 ~ PG[1:26]+pg_sq+RI[1:26]+ri_sq)
summary(eq_h)
26*0.1291 =  3.3566
### Problem d                                                               ###
eq_sc<- lm(res[2:26] ~ PG[2:26]+RI[2:26]+res[1:25])
summary(eq_sc)
25*0.05252 = 1.313
### Problem e                                                              ###
eq_f<- lm(GC[27:30] ~ PG[27:30] + RI[27:30])
summary(eq_f)
anova(eq, eq_f)
sis of Variance Table

Response: GC[1:26]
          Df  Sum Sq Mean Sq F value    Pr(>F)    
PG[1:26]   1 0.47507 0.47507  1035.5 < 2.2e-16 ***
RI[1:26]   1 0.22650 0.22650   493.7 < 2.2e-16 ***
Residuals 23 0.01055 0.00046                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Warning message:
In anova.lmlist(object, ...) :
  models with response ‘"GC[27:30]"’ removed because 
response differs from model 
