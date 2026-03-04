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
res<- resid(eq_cen)
print(eq_cen, logSigma = TRUE, digits = 4)
margEff(eq_cen, xValues = NULL, vcov = NULL,
   calcVCov = TRUE, returnJacobian = TRUE, vcovLogSigma)
  GENDER   ACTIVITY        AGE      AGE_2 
 1.2213435  0.9715951  0.1125443 -0.1060622
1.2213435 / 2.12629 
[1] 0.5744012
0.9715951 / 1.69149 
[1] 0.5744019
0.1125443 /  0.19593 
[1] 0.5744108
-0.1060622 / -0.18465 
[1] 0.5743959
### Panel 4 Probit model: the first equation of Heckman two step estimates(p.499) ### 
panel04<- glm(formula = RESPONSE ~ GENDER + ACTIVITY + AGE + AGE_2, family = binomial(link = "probit"))
summary(panel04)
install.packages("sampleSelection")
library(sampleSelection)
lambda<- invMillsRatio( panel04)
str(lambda)
attach(lambda)
hist(IMR1)
summary(IMR1)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.4067  0.7072  0.7357  0.8004  0.9122  1.6045  

### Tobit 1 Model: the second equation of Heckman two step estimation(p.499) ###
library("sampleSelection")
panel06<- heckit2fit( 
   selection <- RESPONSE~ GENDER+ACTIVITY+AGE+AGE_2,

   outcome <- LOGINV ~ GENDER + ACTIVITY + AGE + AGE_2, 
   data= xm601,
   weights = NULL, inst = NULL,
   print.level = 0,
   maxMethod = "Newton-Raphson" )
summary(panel06)
Tobit 2 model (sample selection model)
2-step Heckman / heckit estimation
925 observations (455 censored and 470 observed)
13 free parameters (df = 913)
Probit selection equation:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.49758    0.53682  -2.790  0.00539 ** 
GENDER       0.58811    0.09668   6.083 1.73e-09 ***
ACTIVITY     0.56117    0.11157   5.030 5.92e-07 ***
AGE          0.04168    0.02154   1.935  0.05334 .  
AGE_2       -0.04098    0.02061  -1.989  0.04703 *  
Outcome equation:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.62831    5.45468  -0.115    0.908
GENDER       0.53564    1.16447   0.460    0.646
ACTIVITY     0.48995    0.97001   0.505    0.614
AGE          0.12324    0.09117   1.352    0.177
AGE_2       -0.10861    0.08948  -1.214    0.225
Multiple R-Squared:0.0487,      Adjusted R-Squared:0.0384
   Error terms:
              Estimate Std. Error t value Pr(>|t|)
invMillsRatio    1.951      2.973   0.656    0.512
sigma            1.780         NA      NA       NA
rho              1.096         NA      NA       NA



