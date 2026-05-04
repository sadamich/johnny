### https://cran.r-project.org/web/packages/systemfit                        ###
library("systemfit")
library("sandwich")

xm729<- read.csv("xm729.csv", header = TRUE)
attach(xm729)
str(xm729)

eq1<- LOGPROD_01~ LOGLAB_01+LOGCAP_01
eq2<- LOGPROD_02~ LOGLAB_02+LOGCAP_02
eq3<- LOGPROD_03~ LOGLAB_03+LOGCAP_03
system<- list(eq1,eq2,eq3)
eq<- systemfit(system, data=xm729)
summary(eq)

systemfit results 
method: OLS 

         N  DF     SSR detRCov   OLS-R2 McElroy-R2
system 111 102 2.09207   4e-06 0.918262   0.951196

     N DF      SSR      MSE     RMSE       R2   Adj R2
eq1 37 34 0.344691 0.010138 0.100688 0.979352 0.978137
eq2 37 34 1.350434 0.039719 0.199295 0.802030 0.790385
eq3 37 34 0.396947 0.011675 0.108050 0.809154 0.797928

The covariance matrix of the residuals
           eq1        eq2        eq3
eq1 0.01013798 0.00107126 0.00240572
eq2 0.00107126 0.03971865 0.00810346
eq3 0.00240572 0.00810346 0.01167491

The correlations of the residuals
          eq1       eq2      eq3
eq1 1.0000000 0.0533855 0.221128
eq2 0.0533855 1.0000000 0.376310
eq3 0.2211276 0.3763105 1.000000


OLS estimates for 'eq1' (equation 1)
Model Formula: LOGPROD_01 ~ LOGLAB_01 + LOGCAP_01
              Estimate Std. Error  t value   Pr(>|t|)    
(Intercept) -1.0879107  0.3547126 -3.06702  0.0042219 ** 
LOGLAB_01    0.9617962  0.0240761 39.94817 < 2.22e-16 ***
LOGCAP_01    0.2727025  0.0522175  5.22243 8.8362e-06 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.100688 on 34 degrees of freedom
Number of observations: 37 Degrees of Freedom: 34 
SSR: 0.344691 MSE: 0.010138 Root MSE: 0.100688 
Multiple R-Squared: 0.979352 Adjusted R-Squared: 0.978137 

OLS estimates for 'eq2' (equation 2)
Model Formula: LOGPROD_02 ~ LOGLAB_02 + LOGCAP_02
              Estimate Std. Error t value   Pr(>|t|)    
(Intercept) 1.36345992 0.69587697 1.95934 0.05831353 .  
LOGLAB_02   0.92297671 0.23646270 3.90327 0.00042691 ***
LOGCAP_02   0.00569495 0.11164043 0.05101 0.95961486    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.199295 on 34 degrees of freedom
Number of observations: 37 Degrees of Freedom: 34 
SSR: 1.350434 MSE: 0.039719 Root MSE: 0.199295 
Multiple R-Squared: 0.80203 Adjusted R-Squared: 0.790385 

OLS estimates for 'eq3' (equation 3)
Model Formula: LOGPROD_03 ~ LOGLAB_03 + LOGCAP_03

              Estimate Std. Error  t value   Pr(>|t|)    
(Intercept) -0.3399832  0.2890830 -1.17607  0.2477342    
LOGLAB_03    1.3302229  0.2269483  5.86135 1.3036e-06 ***
LOGCAP_03    0.1806280  0.0565164  3.19603  0.0030055 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.10805 on 34 degrees of freedom
Number of observations: 37 Degrees of Freedom: 34 
SSR: 0.396947 MSE: 0.011675 Root MSE: 0.10805 
Multiple R-Squared: 0.809154 Adjusted R-Squared: 0.797928 


eq_sur<- systemfit(system,"SUR", data=xm729)
summary(eq_sur)
systemfit results 
method: SUR 
         N  DF    SSR detRCov   OLS-R2 McElroy-R2
system 111 102 2.0941   4e-06 0.918182   0.951303

     N DF      SSR      MSE     RMSE       R2   Adj R2
eq1 37 34 0.344823 0.010142 0.100707 0.979344 0.978129
eq2 37 34 1.351688 0.039756 0.199388 0.801847 0.790191
eq3 37 34 0.397594 0.011694 0.108139 0.808843 0.797598

The covariance matrix of the residuals used for estimation
           eq1        eq2        eq3
eq1 0.01013798 0.00107126 0.00240572
eq2 0.00107126 0.03971865 0.00810346
eq3 0.00240572 0.00810346 0.01167491

The covariance matrix of the residuals
           eq1        eq2        eq3
eq1 0.01014184 0.00104171 0.00246629
eq2 0.00104171 0.03975552 0.00821772
eq3 0.00246629 0.00821772 0.01169395

The correlations of the residuals
          eq1       eq2      eq3
eq1 1.0000000 0.0518789 0.226467
eq2 0.0518789 1.0000000 0.381129
eq3 0.2264674 0.3811290 1.000000


SUR estimates for 'eq1' (equation 1)
Model Formula: LOGPROD_01 ~ LOGLAB_01 + LOGCAP_01
              Estimate Std. Error  t value   Pr(>|t|)    
(Intercept) -1.0480122  0.3509528 -2.98619  0.0052079 ** 
LOGLAB_01    0.9606624  0.0235852 40.73161 < 2.22e-16 ***
LOGCAP_01    0.2668743  0.0517225  5.15974 1.0659e-05 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.100707 on 34 degrees of freedom
Number of observations: 37 Degrees of Freedom: 34 
SSR: 0.344823 MSE: 0.010142 Root MSE: 0.100707 
Multiple R-Squared: 0.979344 Adjusted R-Squared: 0.978129 

SUR estimates for 'eq2' (equation 2)
Model Formula: LOGPROD_02 ~ LOGLAB_02 + LOGCAP_02
              Estimate Std. Error  t value   Pr(>|t|)    
(Intercept)  1.4789084  0.6616776  2.23509 0.03208833 *  
LOGLAB_02    0.9541743  0.2236733  4.26593 0.00015016 ***
LOGCAP_02   -0.0126456  0.1060505 -0.11924 0.90578604    
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.199388 on 34 degrees of freedom
Number of observations: 37 Degrees of Freedom: 34 
SSR: 1.351688 MSE: 0.039756 Root MSE: 0.199388 
Multiple R-Squared: 0.801847 Adjusted R-Squared: 0.790191 

SUR estimates for 'eq3' (equation 3)
Model Formula: LOGPROD_03 ~ LOGLAB_03 + LOGCAP_03
              Estimate Std. Error  t value   Pr(>|t|)    
(Intercept) -0.3941361  0.2872357 -1.37217  0.1789965    
LOGLAB_03    1.3552606  0.2098567  6.45803 2.2031e-07 ***
LOGCAP_03    0.1844597  0.0542665  3.39915  0.0017406 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.108139 on 34 degrees of freedom
Number of observations: 37 Degrees of Freedom: 34 
SSR: 0.397594 MSE: 0.011694 Root MSE: 0.108139 
Multiple R-Squared: 0.808843 Adjusted R-Squared: 0.797598 

eq01<- LOGPROD_1~ LOGLAB_1+LOGCAP_1
eq02<- LOGPROD_2~ LOGLAB_2+LOGCAP_2
eq03<- LOGPROD_3~ LOGLAB_3+LOGCAP_3
eq4<- LOGPROD_4~ LOGLAB_4+LOGCAP_4
eq5<- LOGPROD_5~ LOGLAB_5+LOGCAP_5
eq6<- LOGPROD_6~ LOGLAB_6+LOGCAP_6
eq7<- LOGPROD_7~ LOGLAB_7+LOGCAP_7
eq8<- LOGPROD_8~ LOGLAB_8+LOGCAP_8
eq9<- LOGPROD_9~ LOGLAB_9+LOGCAP_9
eq10<- LOGPROD_10~ LOGLAB_10+LOGCAP_10
eq11<- LOGPROD_11~ LOGLAB_11+LOGCAP_11
eq12<- LOGPROD_12~ LOGLAB_12+LOGCAP_12
eq13<- LOGPROD_13~ LOGLAB_13+LOGCAP_13
eq14<- LOGPROD_14~ LOGLAB_14+LOGCAP_14
eq15<- LOGPROD_15~ LOGLAB_15+LOGCAP_15
eq16<- LOGPROD_16~ LOGLAB_16+LOGCAP_16
eq17<- LOGPROD_17~ LOGLAB_17+LOGCAP_17
eq18<- LOGPROD_18~ LOGLAB_18+LOGCAP_18
eq19<- LOGPROD_19~ LOGLAB_19+LOGCAP_19
eq20<- LOGPROD_20~ LOGLAB_20+LOGCAP_20
eq21<- LOGPROD_21~ LOGLAB_21+LOGCAP_21
eq22<- LOGPROD_22~ LOGLAB_22+LOGCAP_22
eq23<- LOGPROD_23~ LOGLAB_23+LOGCAP_23
eq24<- LOGPROD_24~ LOGLAB_24+LOGCAP_24
eq25<- LOGPROD_25~ LOGLAB_25+LOGCAP_25
eq26<- LOGPROD_26~ LOGLAB_26+LOGCAP_26
system2<- list(eq01,eq02,eq03,eq4,eq5,eq6,eq7,eq8,eq9,eq10,eq11,eq12,eq13,eq14,eq15,
          eq16,eq17,eq18,eq19,eq20,eq21,eq22,eq23,eq24,eq25,eq26)
eq_s<- systemfit(system2,"SUR", data=xm729)
summary(eq_s)



