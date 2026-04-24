https://cran.r-project.org/web/packages/cluster/refman/cluster.html
library(cluster)
https://cran.r-project.org/web/packages/cluster/refman/cluster.html#agnes
Agglomerative Nesting (Hierarchical Clustering)

data(votes.repub)
str(votes.repub)
agn1 <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
agn1
Call:    agnes(x = votes.repub, metric = "manhattan", stand = TRUE) 
Agglomerative coefficient:  0.7977555 
Order of objects:
 [1] Alabama        Georgia        Arkansas       Louisiana      Mississippi   
 [6] South Carolina Alaska         Vermont        Arizona        Montana       
[11] Nevada         Colorado       Idaho          Wyoming        Utah          
[16] California     Oregon         Washington     Minnesota      Connecticut   
[21] New York       New Jersey     Illinois       Ohio           Indiana       
[26] Michigan       Pennsylvania   New Hampshire  Wisconsin      Delaware      
[31] Kentucky       Maryland       Missouri       New Mexico     West Virginia 
[36] Iowa           South Dakota   North Dakota   Kansas         Nebraska      
[41] Maine          Massachusetts  Rhode Island   Florida        North Carolina
[46] Tennessee      Virginia       Oklahoma       Hawaii         Texas         
Height (summary):
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  8.382  12.804  18.528  23.118  28.411  87.455 

Available components:
[1] "order"     "height"    "ac"        "merge"     "diss"      "call"     
[7] "method"    "order.lab" "data"     

plot(agn1)

op <- par(mfrow=c(2,2))
agn2 <- agnes(daisy(votes.repub), diss = TRUE, method = "complete")
plot(agn2)

## alpha = 0.625 ==> beta = -1/4  is "recommended" by some
agnS <- agnes(votes.repub, method = "flexible", par.method = 0.625)
plot(agnS)
par(op)

## "show" equivalence of three "flexible" special cases
d.vr <- daisy(votes.repub)
a.wgt  <- agnes(d.vr, method = "weighted")
a.sing <- agnes(d.vr, method = "single")
a.comp <- agnes(d.vr, method = "complete")
iC <- -(6:7) # not using 'call' and 'method' for comparisons
stopifnot(
  all.equal(a.wgt [iC], agnes(d.vr, method="flexible", par.method = 0.5)[iC])   ,
  all.equal(a.sing[iC], agnes(d.vr, method="flex", par.method= c(.5,.5,0, -.5))[iC]),
  all.equal(a.comp[iC], agnes(d.vr, method="flex", par.method= c(.5,.5,0, +.5))[iC]))

## Exploring the dendrogram structure
(d2 <- as.dendrogram(agn2)) # two main branches
d2[[1]] # the first branch
d2[[2]] # the 2nd one  { 8 + 42  = 50 }
d2[[1]][[1]]# first sub-branch of branch 1 .. and shorter form
identical(d2[[c(1,1)]],
          d2[[1]][[1]])
## a "textual picture" of the dendrogram :
str(d2)

data(agriculture)

## Plot similar to Figure 7 in ref
## Not run: plot(agnes(agriculture), ask = TRUE)


data(animals)
aa.a  <- agnes(animals) # default method = "average"
aa.ga <- agnes(animals, method = "gaverage")
op <- par(mfcol=1:2, mgp=c(1.5, 0.6, 0), mar=c(.1+ c(4,3,2,1)),
          cex.main=0.8)
plot(aa.a,  which.plots = 2)
plot(aa.ga, which.plots = 2)
par(op)


## Show how "gaverage" is a "generalized average":
aa.ga.0 <- agnes(animals, method = "gaverage", par.method = 0)
stopifnot(all.equal(aa.ga.0[iC], aa.a[iC]))