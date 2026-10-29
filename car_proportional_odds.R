https://cran.r-project.org/web/packages/car/refman/car.html#poTest
library(car)

if (require("MASS")){
    .W <- Womenlf
    .W$partic <- factor(.W$partic, levels=c("not.work", "parttime", "fulltime"))
    poTest(polr(partic ~ hincome + children + region, data=.W))
}

Loading required package: MASS

Tests for Proportional Odds
polr(formula = partic ~ hincome + children + region, data = .W)

                b[polr] b[>not.work] b[>parttime] Chisquare df Pr(>Chisq)    
Overall                                               26.47  6    0.00018 ***
hincome         -0.0569      -0.0453      -0.1011      5.73  1    0.01666 *  
childrenpresent -2.0099      -1.6043      -2.7266     17.90  1    2.3e-05 ***
regionBC         0.1530       0.3420      -0.7228      2.69  1    0.10114    
regionOntario    0.2687       0.1878       0.0620      0.07  1    0.78563    
regionPrairie    0.4797       0.4719       0.3501      0.05  1    0.82760    
regionQuebec    -0.0711      -0.1731      -0.2974      0.07  1    0.79252    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
