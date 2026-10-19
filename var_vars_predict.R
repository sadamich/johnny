https://cran.r-project.org/web/packages/vars/refman/vars.html#fanchart
library(vars)

data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
var.2c.prd <- predict(var.2c, n.ahead = 8, ci = 0.95)
fanchart(var.2c.prd)