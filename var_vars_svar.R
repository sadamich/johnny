library(vars)
https://cran.r-project.org/web/packages/vars/refman/vars.html#SVAR

data(Canada)
str(Canada)
 Time-Series [1:84, 1:4] from 1980 to 2001: 930 930 930 931 933 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "e" "prod" "rw" "U"

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

