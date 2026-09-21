https://cran.r-project.org/web/packages/sandwich/refman/sandwich.html#vcovBS

library(sandwich)
(Clustered) Bootstrap Covariance Matrix Estimation
## Petersen's data
data("PetersenCL", package = "sandwich")
str(PetersenCL)
attach(PetersenCL)
'data.frame':   5000 obs. of  4 variables: ### 10 * 500 firms(firmgroups) 
 $ firm: int  1 1 1 1 1 1 1 1 1 1 ...
 $ year: int  1 2 3 4 5 6 7 8 9 10 ...
 $ x   : num  -1.11397 -0.08085 -0.23761 -0.15249 -0.00143 ...
 $ y   : num  2.252 1.242 -1.426 -1.109 0.915
hist(firm, freq=FALSE)
m <- lm(y ~ x, data = PetersenCL)
summary(m)
Call:
lm(formula = y ~ x, data = PetersenCL)
Residuals:
    Min      1Q  Median      3Q     Max 
-6.7611 -1.3680 -0.0166  1.3387  8.6779 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.02968    0.02836   1.047    0.295    
x            1.03483    0.02858  36.204   <2e-16 ***
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 2.005 on 4998 degrees of freedom
Multiple R-squared:  0.2078,    Adjusted R-squared:  0.2076 
F-statistic:  1311 on 1 and 4998 DF,  p-value: < 2.2e-16


## comparison of different standard errors  
suppressWarnings(RNGversion("3.5.0"))   ### warning message ###
set.seed(1)
cbind(
  "classical" = sqrt(diag(vcov(m))),
  "HC-cluster" = sqrt(diag(vcovCL(m, cluster = ~ firm))),
  "BS-cluster" = sqrt(diag(vcovBS(m, cluster = ~ firm))),
  "FW-cluster" = sqrt(diag(vcovBS(m, cluster = ~ firm, type = "fractional")))
)

             classical HC-cluster BS-cluster FW-cluster
(Intercept) 0.02835932 0.06701270 0.07067533 0.06596187
x           0.02858329 0.05059573 0.04878784 0.04965168


## two-way wild cluster bootstrap with Mammen distribution
vcovBS(m, cluster = ~ firm + year, type = "wild-mammen")
            (Intercept)           x
(Intercept) 0.004135069 0.000364327
x           0.000364327 0.002659964
> 

## jackknife estimator coincides with HC3 (aka CV3)
all.equal(
  vcovBS(m, cluster = ~ firm, type = "jackknife"),
  vcovCL(m, cluster = ~ firm, type = "HC3", cadjust = FALSE),
  tolerance = 1e-7)
[1] TRUE