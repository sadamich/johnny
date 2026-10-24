https://cran.r-project.org/web/packages/vars/refman/vars.html#causality

data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
causality(var.2c, cause = "e")
$Granger

        Granger causality H0: e do not Granger-cause prod rw U

data:  VAR object var.2c
F-Test = 6.2768, df1 = 6, df2 = 292, p-value = 3.206e-06
$Instant
        H0: No instantaneous causality between: e and prod rw U
data:  VAR object var.2c
Chi-squared = 26.068, df = 3, p-value = 9.228e-06


#use a robust HC variance-covariance matrix for the Granger test:
causality(var.2c, cause = "e", vcov.=vcovHC(var.2c))
$Granger
        Granger causality H0: e do not Granger-cause prod rw U

data:  VAR object var.2c
F-Test = 3.3923, df1 = 6, df2 = 292, p-value = 0.002994
$Instant
        H0: No instantaneous causality between: e and prod rw U

data:  VAR object var.2c
Chi-squared = 26.068, df = 3, p-value = 9.228e-06


#use a wild-bootstrap procedure to for the Granger test
## Not run: causality(var.2c, cause = "e", boot=TRUE, boot.runs=1000)