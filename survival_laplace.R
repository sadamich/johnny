https://cran.r-project.org/web/packages/BivLaplaceRL/refman/BivLaplaceRL.html#biv_brlmr_order

library(BivLaplaceRL)
sX <- function(x1, x2) sgumbel_biv(x1, x2, k1 = 2, k2 = 1)
sY <- function(x1, x2) sgumbel_biv(x1, x2, k1 = 1, k2 = 1)


biv_brlmr_order(sX, sY, t2_fixed = 0.5)
$order_holds
[1] TRUE

$ratio_values
 [1] 2 2 2 2 2 2 2 2 2 2

$t1_grid
 [1] 0.2 0.5 0.8 1.1 1.4 1.7 2.0 2.3 2.6 2.9
biv_hazard_gradient(t1 = 1, t2 = 1)
biv_hazard_gradient(t1 = 0.5, t2 = 0.5, k1 = 2, k2 = 1.5, theta = 0.3)
