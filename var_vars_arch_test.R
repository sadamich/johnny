https://cran.r-project.org/web/packages/vars/refman/vars.html#arch.test
library(vars)
data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
arch.test(var.2c)


        ARCH (multivariate)

data:  Residuals of VAR object var.2c
Chi-squared = 538.89, df = 500, p-value = 0.1112

summary(arch.test(var.2c))
         Length Class  Mode   
resid    328    -none- numeric
arch.mul   5    htest  list


