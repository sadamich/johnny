https://cran.r-project.org/web/packages/vars/refman/vars.html#roots

data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
roots(var.2c)
[1] 0.9950338 0.9081062 0.9081062 0.7380565 0.7380565 0.1856381 0.1428889
[8] 0.1428889
