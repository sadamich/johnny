https://cran.r-project.org/web/packages/vars/refman/vars.html#BQ
https://www.uh.edu/~bsorense/BlanchardQuah1989.pdf
library(vars)
data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
BQ(var.2c)

SVAR Estimation Results:
======================== 


Estimated contemporaneous impact matrix:
             e     prod       rw        U
e    -0.007644 -0.28470  0.07374 -0.21234
prod  0.543663  0.21658 -0.03379 -0.28652
rw    0.082112  0.28588  0.71874  0.06162
U     0.129451  0.05668 -0.01039  0.24111

Estimated identified long run impact matrix:
          e    prod    rw      U
e    104.37  0.0000  0.00 0.0000
prod  45.35  5.1971  0.00 0.0000
rw   168.41 -2.1145 10.72 0.0000
U    -19.26 -0.4562  1.41 0.5331
