https://cran.r-project.org/web/packages/eulerr/
library(eulerr)
https://cran.r-project.org/web/packages/eulerr/vignettes/introduction.html


# Input in the form of a named numeric vector
fit1 <- euler(c(
  "A" = 25,
  "B" = 5,
  "C" = 5,
  "A&B" = 5,
  "A&C" = 5,
  "B&C" = 3,
  "A&B&C" = 3
))

fit1
      original fitted residuals regionError
A           25 25.057    -0.057       0.015
B            5  5.333    -0.333       0.009
C            5  5.333    -0.333       0.009
A&B          5  4.389     0.611       0.010
A&C          5  4.389     0.611       0.010
B&C          3  1.418     1.582       0.030
A&B&C        3  3.707    -0.707       0.016
diagError: 0.03 
stress:    0.005 


# Input as a matrix of logicals
set.seed(1)
mat <- cbind(
  A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
  B = sample(c(TRUE, FALSE), 50, TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
)
fit2 <- euler(mat)
fit2
      original fitted residuals regionError
A           13     13         0       0.008
B            4      4         0       0.002
C            0      0         0       0.000
A&B         17     17         0       0.010
A&C          5      5         0       0.003
B&C          1      0         1       0.024
A&B&C        2      2         0       0.001

diagError: 0.024 
stress:    0.002 

# Cleveland dot plot of the residuals
dotchart(resid(fit2))

{r error-plot, fig.cap = "A plot from `error_plot()`."} error_plot(fit2))

coef(fit2)