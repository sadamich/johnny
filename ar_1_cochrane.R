### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 

### Example 7 8 Industrial Production  p.562 ###
xm701<- read.csv("xm701.csv", header = TRUE)
str(xm701)
attach(xm701)
detach(xm701)

D4Y_61<- D4Y[45:180]
d4_1<- D4Y[44:179]

eq<- lm(D4Y_61~d4_1)
summary(eq)
res<- resid(eq)
res1<- c(NA,res)[1:136]
eq2<- lm(res~res1)
summary(eq2)
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.0006243  0.0018342   0.340    0.734    
res1        0.4443882  0.0737503   6.026 1.55e-08 ***

y<- D4Y_61 - 0.4443882*D4Y[44:179]
1-0.4443882
const<- rep(0.5556118,136)
x<- d4_1 - 0.4443882*D4Y[43:178]
eq3<- lm(y ~ const+ x -1)
summary(eq3)
Call:
lm(formula = y ~ const + x - 1)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.071346 -0.009355 -0.000235  0.010882  0.084951 

Coefficients:
      Estimate Std. Error t value Pr(>|t|)    
const 0.008948   0.003815   2.345   0.0205 *  
x     0.741532   0.057577  12.879   <2e-16 **
