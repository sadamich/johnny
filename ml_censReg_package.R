https://cran.r-project.org/web/packages/censReg/refman/censReg.html
library(censReg)

### Censored Regression (Tobit) Model
## Kleiber & Zeileis ( 2008 ), page 142
data( "Affairs", package = "AER" )
attach(Affairs)
str(Affairs)
'data.frame':   601 obs. of  9 variables:
 $ affairs      : num  0 0 0 0 0 0 0 0 0 0 ...
 $ gender       : Factor w/ 2 levels "female","male": 2 1 1 2 2 1 1 2 1 2 ...
 $ age          : num  37 27 32 57 22 32 22 57 32 22 ...
 $ yearsmarried : num  10 4 15 15 0.75 1.5 0.75 15 15 1.5 ...
 $ children     : Factor w/ 2 levels "no","yes": 1 1 2 2 1 1 1 2 2 1 ...
 $ religiousness: int  3 4 1 5 2 2 2 2 4 4 ...
 $ education    : num  18 14 12 18 17 17 12 14 16 14 ...
 $ occupation   : int  7 6 1 6 6 5 1 4 1 4 ...
 $ rating       : int  4 4 4 5 3 5 3 4 2 5 .

estResult <- censReg( affairs ~ age + yearsmarried + religiousness +
   occupation + rating, data = Affairs )
print( estResult )
### Call: censReg(formula = affairs ~ age + yearsmarried + religiousness + 
    occupation + rating, data = Affairs)
Coefficients:
  (Intercept)           age  yearsmarried religiousness    occupation 
       8.1742       -0.1793        0.5541       -1.6862        0.3261 
       rating      logSigma 
      -2.2850        2.1099 

## Kleiber & Zeileis ( 2008 ), page 143
estResultBoth <- censReg( affairs ~ age + yearsmarried + religiousness +
   occupation + rating, data = Affairs, right = 4 )
print( estResultBoth )
### Call: censReg(formula = affairs ~ age + yearsmarried + religiousness + 
    occupation + rating, right = 4, data = Affairs)
Coefficients:
  (Intercept)           age  yearsmarried religiousness    occupation 
       7.9010       -0.1776        0.5323       -1.6163        0.3242 
       rating      logSigma 
      -2.2070        2.0723 


## Kleiber & Zeileis ( 2008 ), page 142
data( "Affairs", package = "AER" )
estResult <- censReg( affairs ~ age + yearsmarried + religiousness +
   occupation + rating, data = Affairs )
margEff( estResult )
          age  yearsmarried religiousness    occupation        rating 
  -0.04192089    0.12953651   -0.39417189    0.07621840   -0.53413656 

summary( margEff( estResult ) )
               Marg. Eff. Std. Error t value  Pr(>|t|)    
age            -0.041921   0.018444 -2.2728   0.02339 *  
yearsmarried    0.129537   0.031168  4.1561 3.713e-05 ***
religiousness  -0.394172   0.093379 -4.2212 2.811e-05 ***
occupation      0.076218   0.059472  1.2816   0.20049    
rating         -0.534137   0.094896 -5.6286 2.802e-08 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1

margEff( estResult, xValues = c( 1, 40, 4, 2, 4, 4 ) )
          age  yearsmarried religiousness    occupation        rating 
  -0.02982223    0.09215139   -0.28041119    0.05422125   -0.37998110 
 
summary( estResult )

### Call: censReg(formula = affairs ~ age + yearsmarried + religiousness + 
    occupation + rating, data = Affairs)
Observations:
         Total  Left-censored     Uncensored Right-censored 
           601            451            150              0 
Coefficients:
              Estimate Std. error t value  Pr(> t)    
(Intercept)    8.17420    2.74145   2.982  0.00287 ** 
age           -0.17933    0.07909  -2.267  0.02337 *  
yearsmarried   0.55414    0.13452   4.119 3.80e-05 ***
religiousness -1.68622    0.40375  -4.176 2.96e-05 ***
occupation     0.32605    0.25442   1.282  0.20001    
rating        -2.28497    0.40783  -5.603 2.11e-08 ***
logSigma       2.10986    0.06710  31.444  < 2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Newton-Raphson maximisation, 7 iterations
Return code 1: gradient close to zero (gradtol)
Log-likelihood: -705.5762 on 7 Df

