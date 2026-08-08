https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf
library(sandwich)
### 4.3. Testing and dating structural changes in the presence of
### heteroskedasticity and autocorrelation
library("strucchange")
data("RealInt", package = "strucchange")
ocus <- gefp(RealInt ~ 1, fit = lm, vcov = kernHAC)

bp <- breakpoints(RealInt ~ 1)
confint(bp, vcov = kernHAC)

 Confidence intervals for breakpoints
         of optimal 3-segment partition: 

Call:
confint.breakpointsfull(object = bp, vcov. = kernHAC)

Breakpoints at observation number:
  2.5 % breakpoints 97.5 %
1    37          47     48
2    77          79     81

Corresponding to breakdates:
  2.5 %   breakpoints 97.5 % 
1 1970(1) 1972(3)     1972(4)
2 1980(1) 1980(3)     1981(1)
