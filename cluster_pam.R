https://cran.r-project.org/web/packages/cluster/refman/cluster.html#pam
library(cluster)
Description
Partitioning (clustering) of the data into k clusters “around medoids”,
 a more robust version of K-means.
## generate 25 objects, divided into 2 clusters.
set.seed(17) # to get reproducible data:
x <- rbind(cbind(rnorm(10,0,0.5), rnorm(10,0,0.5)),
           cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
pamx <- pam(x, 2)
pamx # Medoids: '9' and '15' ...
Medoids:
     ID                       
[1,] 10 0.3037281 -0.008874562
[2,] 16 4.9445682  4.965255299
Clustering vector:
 [1] 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
Objective function:
    build      swap 
0.6895253 0.5636592 

Available components:
 [1] "medoids"    "id.med"     "clustering" "objective"  "isolation" 
 [6] "clusinfo"   "silinfo"    "diss"       "call"       "data" 
 
summary(pamx)
Medoids:
     ID                       
[1,] 10 0.3037281 -0.008874562
[2,] 16 4.9445682  4.965255299
Clustering vector:
 [1] 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
Objective function:
    build      swap 
0.6895253 0.5636592 

Numerical information per cluster:
     size max_diss   av_diss diameter separation
[1,]   10 1.434945 0.6119106 1.936426   5.513579
[2,]   15 1.037537 0.5314915 1.819463   5.513579

Isolated clusters:
 L-clusters: character(0)
 L*-clusters: [1] 1 2

Silhouette plot information:
   cluster neighbor sil_width
10       1        2 0.9015677
8        1        2 0.8990387
3        1        2 0.8864581
6        1        2 0.8856178
2        1        2 0.8789426
7        1        2 0.8771480
4        1        2 0.8727587
9        1        2 0.8604396
5        1        2 0.8518077
1        1        2 0.7993765
16       2        1 0.9180002
21       2        1 0.9174870
23       2        1 0.9135378
17       2        1 0.9105330
22       2        1 0.9084663
13       2        1 0.9038646
19       2        1 0.9038378
12       2        1 0.9027713
15       2        1 0.8952701
18       2        1 0.8921488
25       2        1 0.8917861
11       2        1 0.8834024
14       2        1 0.8789767
20       2        1 0.8642016
24       2        1 0.7836149
Average silhouette width per cluster:
[1] 0.8713155 0.8911932
Average silhouette width of total data set:
[1] 0.8832421

300 dissimilarities, summarized :
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.04783 0.77672 3.72500 3.92810 6.99140 8.48570 
Metric :  euclidean 
Number of objects : 25

Available components:
 [1] "medoids"    "id.med"     "clustering" "objective"  "isolation" 
 [6] "clusinfo"   "silinfo"    "diss"       "call"       "data"      

plot(pamx)

stopifnot(pamx$id.med == c(9, 15))
stopifnot(identical(pamx$clustering, rep(1:2, c(10, 15))))

## use obs. 1 & 16 as starting medoids -- same result (for seed above, *and* typically) :
(p2m <- pam(x, 2, medoids = c(1,16)))



## no _build_ *and* no _swap_ phase: just cluster all obs. around (1, 16):
p2.s <- pam(x, 2, medoids = c(1,16), do.swap = FALSE)
p2.s

keep_nms <- setdiff(names(pamx), c("call", "objective"))# .$objective["build"] differ
stopifnot(p2.s$id.med == c(1,16), # of course
          identical(pamx[keep_nms],
                    p2m[keep_nms]))

p3m <- pam(x, 3, trace.lev = 2)
## rather stupid initial medoids:
(p3m. <- pam(x, 3, medoids = 3:1, trace.lev = 1))


pam(daisy(x, metric = "manhattan"), 2, diss = TRUE)

data(ruspini)
## Plot similar to Figure 4 in Stryuf et al (1996)
## Not run: plot(pam(ruspini, 4), ask = TRUE)
