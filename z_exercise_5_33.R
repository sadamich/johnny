### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xr533<- read.csv("xr533.csv", header=TRUE)
str(xr533)
attach(xr533)
detach(xr533)
### Problem a (p.435)                                                      ###
eq<- lm(BUCHANAN~GORE)
summary(eq)
res<- resid(eq)
plot(res)
### Problem b                                                              ###
eq_d<- lm(BUCHANAN~GORE+DUMPALM)
summary(eq_d)
### The Dummy Call:lm(formula = BUCHANAN ~ GORE + DUMPALM)                 ###
Residuals:
    Min      1Q  Median      3Q     Max 
-383.27  -81.66  -37.01   38.62  393.50 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.094e+02  1.952e+01   5.602 4.79e-07 ***
GORE        2.540e-03  2.438e-04  10.419 2.04e-15 ***
DUMPALM     2.615e+03  1.493e+02  17.509  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 137.5 on 64 degrees of freedom
Multiple R-squared:  0.9092,    Adjusted R-squared:  0.9064 
F-statistic: 320.6 on 2 and 64 DF,  p-value: < 2.2e-16
summary(BUCHANAN)
BUCHANAN[OBS==50]

1.094e+02 +2.540e-03*50+2.615e+03*50