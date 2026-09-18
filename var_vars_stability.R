
https://cran.r-project.org/web/packages/vars/refman/vars.html#stability


data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
var.2c.stabil <- stability(var.2c, type = "OLS-CUSUM")
var.2c.stabil
## Not run: 
plot(var.2c.stabil)