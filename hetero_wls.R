### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm511<- read.csv("xm511.csv", header =TRUE)
str(xm511)
attach(xm511)
panel01<- lm(DAAA ~ DUS3MT)
summary(panel01)
### Panel 1 (p.341) Call:lm(formula = DAAA ~ DUS3MT)                       ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.70283 -0.07086 -0.00395  0.06571  1.06943 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.006393   0.006982   0.916     0.36    
DUS3MT      0.274585   0.014641  18.754   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.171 on 598 degrees of freedom
Multiple R-squared:  0.3703,    Adjusted R-squared:  0.3693 
F-statistic: 351.7 on 1 and 598 DF,  p-value: < 2.2e-16
res<- resid(panel01)
panel02<- lm(res^2 ~ DUM7599)
summary(panel02)
### Panel 2 (p.341) Call:lm(formula = res^2 ~ DUM7599)                     ###
Residuals:
     Min       1Q   Median       3Q      Max 
-0.04857 -0.03664 -0.00933 -0.00225  1.09511 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.009719   0.004374   2.222   0.0267 *  
DUM7599     0.038850   0.006186   6.281 6.49e-10 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.07576 on 598 degrees of freedom
Multiple R-squared:  0.06188,   Adjusted R-squared:  0.06031 
F-statistic: 39.45 on 1 and 598 DF,  p-value: 6.491e-10
v<- fitted(panel02)
DAAA_v <- DAAA/sqrt(v)
DUS3MT_v<- DUS3MT/sqrt(v)
panel03<- lm(DAAA ~ DUS3MT, weight = 1/v)
summary(panel03)
### Panel 3 (p.341) Call:lm(formula = DAAA ~ DUS3MT, weights = 1/v)        ###
Weighted Residuals:
    Min      1Q  Median      3Q     Max 
-4.7535 -0.5155 -0.0561  0.4383  5.0318 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.013384   0.005127    2.61  0.00927 ** 
DUS3MT      0.214989   0.014079   15.27  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.9859 on 598 degrees of freedom
Multiple R-squared:  0.2805,    Adjusted R-squared:  0.2793 
F-statistic: 233.2 on 1 and 598 DF,  p-value: < 2.2e-16


library(maxLik)
loglik <- function(theta) {
 beta0 <- theta[1]
 beta1 <- theta[2]
 sigma <- theta[5]
 N <- nrow(xm511)
 mu <- beta0 + beta1*DUS3MT
 -1/2*N*log(2*pi) - 1/2*N*log(sigma^2) - 1/2*sum((DAAA -mu)^2/sigma^2)
 }
 m <- maxLik(loglik,start=c(beta0 = 0, beta1=0.2,sigma=1))
 summary(m)
