library(vars)
https://cran.r-project.org/web/packages/vars/refman/vars.html#SVAR
data(Canada)
str(Canada)
 Time-Series [1:84, 1:4] from 1980 to 2001: 930 930 930 931 933 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "e" "prod" "rw" "U"
Canadian unemployment rate in %	444113DSA
Canadian manufacturing real wage	444321KSA
Canadian consumer price index	445241K
Canadian nominal GDP	CAN1008S1
Canadian civil employment in 1000 persons	445005DSA
prod :=	100*(ln(CAN1008S1/445241K)-ln(445005DSA))
e :=	100*ln(445005DSA)
U :=	444113DSA
rw :=	100*ln(100*444321KSA)
prod is used as a measure of labour productivity; 
e is used for employment; U is the unemployment rate 
and rw assigns the real wage.

var.2c <- VAR(Canada, p = 2, type = "const")
VAR Estimation Results:
======================= 

Estimated coefficients for equation e: 
====================================== 
Call:
e = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + const 

         e.l1       prod.l1         rw.l1          U.l1          e.l2 
 1.637821e+00  1.672717e-01 -6.311863e-02  2.655848e-01 -4.971338e-01 
      prod.l2         rw.l2          U.l2         const 
-1.016501e-01  3.844492e-03  1.326893e-01 -1.369984e+02 


Estimated coefficients for equation prod: 
========================================= 
Call:
prod = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + const 

        e.l1      prod.l1        rw.l1         U.l1         e.l2      prod.l2 
  -0.1727658    1.1504282    0.0513039   -0.4785013    0.3852589   -0.1724119 
       rw.l2         U.l2        const 
  -0.1188510    1.0159180 -166.7755177 


Estimated coefficients for equation rw: 
======================================= 
Call:
rw = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + const 

         e.l1       prod.l1         rw.l1          U.l1          e.l2 
 -0.268832871  -0.081065001   0.895478330   0.012130033   0.367848941 
      prod.l2         rw.l2          U.l2         const 
 -0.005180947   0.052676565  -0.127708256 -33.188338774 


Estimated coefficients for equation U: 
====================================== 
Call:
U = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + const 

        e.l1      prod.l1        rw.l1         U.l1         e.l2      prod.l2 
 -0.58076382  -0.07811707   0.01866214   0.61893150   0.40981822   0.05211668 
       rw.l2         U.l2        const 
  0.04180115  -0.07116885 149.78056487 


amat <- diag(4)
diag(amat) <- NA
amat[2, 1] <- NA
amat[4, 1] <- NA
    [,1] [,2] [,3] [,4]
[1,]   NA    0    0    0
[2,]   NA   NA    0    0
[3,]    0    0   NA    0
[4,]   NA    0    0   NA

## Estimation method scoring
SVAR(x = var.2c, estmethod = "scoring", Amat = amat, Bmat = NULL,
max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 
SVAR Estimation Results:
======================== 
Estimated A matrix:
         e  prod    rw     U
e    2.756 0.000 0.000 0.000
prod 0.087 1.533 0.000 0.000
rw   0.000 0.000 1.282 0.000
U    2.562 0.000 0.000 4.882

## Estimation method direct
SVAR(x = var.2c, estmethod = "direct", Amat = amat, Bmat = NULL,
hessian = TRUE, method="BFGS") 
SVAR Estimation Results:
======================== 
Estimated A matrix:
         e  prod    rw     U
e    2.756 0.000 0.000 0.000
prod 0.087 1.533 0.000 0.000
rw   0.000 0.000 1.282 0.000
U    2.562 0.000 0.000 4.882


https://cran.r-project.org/web/packages/vars/refman/vars.html#SVEC
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace",
              ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
SR
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
LR
SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = FALSE)
SVEC Estimation Results:
======================== 
Estimated contemporaneous impact matrix:
         prod        e         U      rw
prod  0.58402  0.07434 -0.152578 0.06900
e    -0.12029  0.26144 -0.155096 0.08978
U     0.02526 -0.26720  0.005488 0.04982
rw    0.11170  0.00000  0.483771 0.48791

Estimated long run impact matrix:
        prod       e       U rw
prod  0.7910  0.0000  0.0000  0
e     0.2024  0.5769 -0.4923  0
U    -0.1592 -0.3409  0.1408  0
rw   -0.1535  0.5961 -0.2495  0
> 
