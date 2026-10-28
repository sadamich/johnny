https://cran.r-project.org/web/packages/vars/refman/vars.html#serial.test
library(vars)

data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
serial.test(var.2c, lags.pt = 16, type = "PT.adjusted")

        Portmanteau Test (adjusted)
data:  Residuals of VAR object var.2c
Chi-squared = 231.59, df = 224, p-value = 0.3497

serial.test(var.2c, lags.pt = 16, type = "BG")
        Breusch-Godfrey LM test
data:  Residuals of VAR object var.2c
Chi-squared = 92.628, df = 80, p-value = 0.1581

serial.test(var.2c, lags.pt = 16, type = "ES")
        Edgerton-Shukur F test

data:  Residuals of VAR object var.2c
F statistic = 1.1186, df1 = 80, df2 = 199, p-value = 0.2648