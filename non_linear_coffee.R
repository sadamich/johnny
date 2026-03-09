### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 4 6 Coffee sales                                               ###
xm402 <- read.csv("xm402.csv", header = TRUE)
str(xm402)
attach(xm402)
library(gslnls)
For D1
non_3 <- gsl_nls(fn = LOGQ1 ~ beta_1 + (beta_2/beta_3) * (D1^beta_3 -1), data = xm402, start = c(beta_1 = NA, beta_2 = NA, beta_3= NA))
coef(non_3)
 beta_1        beta_2        beta_3 
  5.807117686  10.298318439 -13.430734856 
summary(non_3)
### Panel 1(p.248) Formula: LOGQ1 ~ beta_1 + (beta_2/beta_3) * (D1^beta_3 - 1)###
Parameters:
        Estimate Std. Error t value Pr(>|t|)    
beta_1   5.80712    0.04015 144.636   <2e-16 ***
beta_2  10.29832    3.29560   3.125   0.0122 *  
beta_3 -13.43073    6.67610  -2.012   0.0751 .  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.09835 on 9 degrees of freedom
Number of iterations to convergence: 2 
Achieved convergence tolerance: 2.369e-13
For D2
non_3_2 <- gsl_nls(fn = LOGQ2 ~ beta_1 + (beta_2/beta_3) * (D2^beta_3 -1), data = xm402, start = c(beta_1 = NA, beta_2 = NA, beta_3= NA))
coef(non_3_2) 
summary(non_3_2) 
### Panel 4 (p.248)Formula: LOGQ2 ~ beta_1 + (beta_2/beta_3) * (D2^beta_3 - 1)###
Parameters:
       Estimate Std. Error t value Pr(>|t|)    
beta_1  4.37780    0.04324 101.254 4.54e-15 ***
beta_2 10.28864    3.00174   3.428  0.00754 ** 
beta_3 -8.59529    5.20747  -1.651  0.13322    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.1059 on 9 degrees of freedom
Number of iterations to convergence: 2 
Achieved convergence tolerance: 2.363e-13


