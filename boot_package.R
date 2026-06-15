https://cran.r-project.org/web/packages/boot/refman/boot.html#boot.ci

library(boot)

# confidence intervals for the city data
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
city.boot <- boot(city, ratio, R = 999, stype = "w", sim = "ordinary")
boot.ci(city.boot, conf = c(0.90, 0.95),
        type = c("norm", "basic", "perc", "bca"))


BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 999 bootstrap replicates

CALL : 
boot.ci(boot.out = city.boot, conf = c(0.9, 0.95), type = c("norm", 
    "basic", "perc", "bca"))

Intervals : 
Level      Normal              Basic         
90%   ( 1.123,  1.849 )   ( 1.104,  1.765 )   
95%   ( 1.053,  1.919 )   ( 0.983,  1.796 )  

Level     Percentile            BCa          
90%   ( 1.276,  1.936 )   ( 1.278,  1.939 )   
95%   ( 1.245,  2.057 )   ( 1.245,  2.064 )  
Calculations and Intervals on Original Scale
> 
