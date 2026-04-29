https://cran.r-project.org/web/packages/cluster/refman/cluster.html#fanny

Description
Computes a fuzzy clustering of the data into k clusters.

## generate 10+15 objects in two clusters, plus 3 objects lying
## between those clusters.
x <- rbind(cbind(rnorm(10, 0, 0.5), rnorm(10, 0, 0.5)),
           cbind(rnorm(15, 5, 0.5), rnorm(15, 5, 0.5)),
           cbind(rnorm( 3,3.2,0.5), rnorm( 3,3.2,0.5)))
fannyx <- fanny(x, 2)
## Note that observations 26:28 are "fuzzy" (closer to # 2):
fannyx
Fuzzy Clustering object of class 'fanny' :                      
m.ship.expon.        2
objective     14.75046
tolerance        1e-15
iterations          10
converged            1
maxit              500
n                   28
Membership coefficients (in %, rounded):
      [,1] [,2]
 [1,]   93    7
 [2,]   97    3
 [3,]   97    3
 [4,]   91    9
 [5,]   96    4
 [6,]   92    8
 [7,]   97    3
 [8,]   89   11
 [9,]   96    4
[10,]   97    3
[11,]    6   94
[12,]    4   96
[13,]   10   90
[14,]    5   95
[15,]    4   96
[16,]    8   92
[17,]    4   96
[18,]    9   91
[19,]    8   92
[20,]    9   91
[21,]   12   88
[22,]    6   94
[23,]    7   93
[24,]    7   93
[25,]    7   93
[26,]   28   72
[27,]   36   64
[28,]   47   53
Fuzzyness coefficients:
dunn_coeff normalized 
 0.8448683  0.6897365 
Closest hard clustering:
 [1] 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

Available components:
 [1] "membership"  "coeff"       "memb.exp"    "clustering"  "k.crisp"    
 [6] "objective"   "convergence" "diss"        "call"        "silinfo"    
[11] "data"       
> 


summary(fannyx)
plot(fannyx)

(fan.x.15 <- fanny(x, 2, memb.exp = 1.5)) # 'crispier' for obs. 26:28
(fanny(x, 2, memb.exp = 3))               # more fuzzy in general

data(ruspini)
f4 <- fanny(ruspini, 4)
stopifnot(rle(f4$clustering)$lengths == c(20,23,17,15))
plot(f4, which = 1)
## Plot similar to Figure 6 in Stryuf et al (1996)
plot(fanny(ruspini, 5))