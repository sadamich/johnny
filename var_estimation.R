### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press, p.665 and p.677       ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 26 Interest and bond rates p.663 ###
xm722<- read.csv("xm722.csv", head=TRUE)
str(xm722)
attach(xm722)
library("vars")
VAR_data <- data.frame(DAAA[25:624], DUS3MT[25:624])
eq_var<- VAR(VAR_data, p = 1)
eq_var
### VAR(1) Estimation Results: ###
Estimated coefficients for equation DAAA.25.624.: 
Call:DAAA.25.624. = DAAA.25.624..l1 + DUS3MT.25.624..l1 + const 
  DAAA.25.624..l1 DUS3MT.25.624..l1             const 
      0.442456563      -0.052701888       0.005132361 
Estimated coefficients for equation DUS3MT.25.624.: 
Call:DUS3MT.25.624. = DAAA.25.624..l1 + DUS3MT.25.624..l1 + const 
  DAAA.25.624..l1 DUS3MT.25.624..l1             const 
      0.605138840       0.106807596       0.001374641
eq_var2<- VAR(VAR_data, p = 2)
eq_var2
### VAR(2) Estimation Results:###
Estimated coefficients for equation DAAA.25.624.: 
Call:DAAA.25.624. = DAAA.25.624..l1 + DUS3MT.25.624..l1 + DAAA.25.624..l2 + DUS3MT.25.624..l2 + const 
  DAAA.25.624..l1 DUS3MT.25.624..l1   DAAA.25.624..l2 DUS3MT.25.624..l2 
      0.529129712      -0.040859540      -0.318417863       0.047550901 
            const 
      0.006690442 
Estimated coefficients for equation DUS3MT.25.624.:  
Call:DUS3MT.25.624. = DAAA.25.624..l1 + DUS3MT.25.624..l1 + DAAA.25.624..l2 + DUS3MT.25.624..l2 + const 
  DAAA.25.624..l1 DUS3MT.25.624..l1   DAAA.25.624..l2 DUS3MT.25.624..l2 
      0.747442625       0.165422978      -0.547064205      -0.051992268 
            const 
      0.004643489
### Granger causality test for VAR(2) model ###
causality(eq_var2)
$Granger
        Granger causality H0: DAAA.25.624. do not Granger-cause DUS3MT.25.624.
data:  VAR object eq_var2
F-Test = 28.753, df1 = 2, df2 = 1186, p-value = 6.4e-13
$Instant
        H0: No instantaneous causality between: DAAA.25.624. and DUS3MT.25.624.
data:  VAR object eq_var2
Chi-squared = 135.86, df = 1, p-value < 2.2e-16

VAR(VAR_data, p = 2, type = "none")
VAR(VAR_data, p = 2, type = "const")
VAR(VAR_data, p = 2, type = "trend")
VAR(VAR_data, p = 2, type = "both")
VARselect(VAR_data, lag.max = 5, type="const")
arch.test(eq_var2)
roots(eq_var2)
serial.test(eq_var2, lags.pt = 16, type = "PT.adjusted")
### Stability test ###
eq_var2.stabil <- stability(eq_var2, type = "OLS-CUSUM")
eq_var2.stabil
plot(eq_var2.stabil)

### Example 7 27 p.674####
### Compare with the result of package tsDyn ###
library(tsDyn)
VECM_data <- data.frame(AAA[25:624], US3MTBIL[25:624])
#Fit a VECM with Engle-Granger 2OLS estimator:
vecm.eg<-VECM(VECM_data, lag=2)
#Fit a VECM with Johansen MLE estimator:
vecm.jo<-VECM(VECM_data, lag=2, estim="ML")



