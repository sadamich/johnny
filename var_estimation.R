### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press, p.665                 ###

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