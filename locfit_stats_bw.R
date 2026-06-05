https://search.r-project.org/R/refmans/stats/html/bandwidth.html

Examples
require(graphics)
str(precip)
 Named num [1:70] 67 54.7 7 48.5 14 17.2 20.7 13 43.4 40.2 ...
 - attr(*, "names")= chr [1:70] "Mobile" "Juneau" "Phoenix" "Little Rock" ...

head(precip,10)
 Mobile        Juneau       Phoenix   Little Rock   Los Angeles 
         67.0          54.7           7.0          48.5          14.0 
   Sacramento San Francisco        Denver      Hartford    Wilmington 
         17.2          20.7          13.0          43.4          40.2 
x<- precip
hist(x)
plot(density(precip, n = 1000))
rug(precip)
lines(density(precip, bw = "nrd"), col = 2)
lines(density(precip, bw = "ucv"), col = 3)
lines(density(precip, bw = "bcv"), col = 4)
lines(density(precip, bw = "SJ-ste"), col = 5)
lines(density(precip, bw = "SJ-dpi"), col = 6)
legend(55, 0.035,
       legend = c("nrd0", "nrd", "ucv", "bcv", "SJ-ste", "SJ-dpi"),
       col = 1:6, lty = 1)

