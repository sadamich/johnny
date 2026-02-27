### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Panel 2 Tobit Model (p.498)                                            ###
library("censReg")
cf) Arne Henningsen
eq_cen<- censReg(LOGINV~GENDER+ACTIVITY+AGE+AGE_2)
summary(eq_cen)
Call: censReg(formula = LOGINV ~ GENDER + ACTIVITY + AGE + AGE_2)
Observations:
         Total  Left-censored     Uncensored Right-censored 
           925            455            470              0 
Coefficients:
            Estimate Std. error t value  Pr(> t)    
(Intercept) -5.93645    1.97528  -3.005  0.00265 ** 
GENDER       2.12629    0.36038   5.900 3.63e-09 ***
ACTIVITY     1.69149    0.37313   4.533 5.81e-06 ***
AGE          0.19593    0.07906   2.478  0.01320 *  
AGE_2       -0.18465    0.07581  -2.436  0.01486 *  
logSigma     1.42543    0.03749  38.020  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Newton-Raphson maximisation, 5 iterations
Return code 1: gradient close to zero (gradtol)
Log-likelihood: -1669.562 on 6 Df
