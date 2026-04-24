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


https://cran.r-project.org/web/packages/cluster/refman/cluster.html#daisy
Dissimilarity Matrix Calculation

data(agriculture)
## Example 1 in ref:
##  Dissimilarities using Euclidean metric and without standardization
d.agr <- daisy(agriculture, metric = "euclidean", stand = FALSE)
d.agr
Dissimilarities :
            B        DK         D        GR         E         F       IRL
DK   5.408327                                                            
D    2.061553  3.405877                                                  
GR  22.339651 22.570113 22.661200                                        
E    9.818350 11.182576 10.394710 12.567418                              
F    3.448188  3.512834  2.657066 20.100995  8.060397                    
IRL 12.747549 13.306014 13.080138  9.604166  3.140064 10.564563          
I    5.803447  5.470832  5.423099 17.383325  5.727128  2.773085  7.920859
L    4.275512  2.220360  2.300000 24.035391 12.121056  4.060788 14.569145
NL   1.649242  5.096077  2.435159 20.752349  8.280097  2.202272 11.150785
P   17.236299 17.864490 17.664088  5.162364  7.430343 15.164432  4.601087
UK   2.828427  8.052950  4.850773 21.485344  8.984431  5.303772 12.103718
            I         L        NL         P
DK                                         
D                                          
GR                                         
E                                          
F                                          
IRL                                        
I                                          
L    6.660330                              
NL   4.204759  4.669047                    
P   12.515990 19.168985 15.670673          
UK   6.723095  7.102112  3.124100 16.323296
Metric :  euclidean 
Number of objects : 12

as.matrix(d.agr)[,"DK"] # via as.matrix.dist(.)
  B        DK         D        GR         E         F       IRL         I 
 5.408327  0.000000  3.405877 22.570113 11.182576  3.512834 13.306014  5.470832 
        L        NL         P        UK 
 2.220360  5.096077 17.864490  8.052950 
## compare with
as.matrix(daisy(agriculture, metric = "gower"))
             B         DK          D        GR          E          F        IRL
B   0.00000000 0.22148078 0.08178881 0.8438459 0.38135483 0.11538211 0.47547804
DK  0.22148078 0.00000000 0.13969197 0.9145729 0.45208184 0.12117405 0.54620505
D   0.08178881 0.13969197 0.00000000 0.8854337 0.42294264 0.09203485 0.51706585
GR  0.84384585 0.91457286 0.88543366 0.0000000 0.46249103 0.79339881 0.36836781
E   0.38135483 0.45208184 0.42294264 0.4624910 0.00000000 0.33090779 0.09412321
F   0.11538211 0.12117405 0.09203485 0.7933988 0.33090779 0.00000000 0.42503100
IRL 0.47547804 0.54620505 0.51706585 0.3683678 0.09412321 0.42503100 0.00000000
I   0.15222215 0.22294916 0.19380996 0.6916237 0.22913268 0.10177511 0.32325589
L   0.15646414 0.06501664 0.07467532 0.9601090 0.49761796 0.16671017 0.59174117
NL  0.05318802 0.19426679 0.09477583 0.7906578 0.32816681 0.08816811 0.42229002
P   0.66155453 0.73228154 0.70314234 0.1822913 0.28019970 0.61110749 0.18607649
UK  0.10095934 0.32244012 0.18274816 0.7629870 0.30049599 0.21634145 0.39461920
            I          L         NL         P        UK
B   0.1522221 0.15646414 0.05318802 0.6615545 0.1009593
DK  0.2229492 0.06501664 0.19426679 0.7322815 0.3224401
D   0.1938100 0.07467532 0.09477583 0.7031423 0.1827482
GR  0.6916237 0.96010899 0.79065783 0.1822913 0.7629870
E   0.2291327 0.49761796 0.32816681 0.2801997 0.3004960
F   0.1017751 0.16671017 0.08816811 0.6111075 0.2163414
IRL 0.3232559 0.59174117 0.42229002 0.1860765 0.3946192
I   0.0000000 0.26848528 0.11202114 0.5093324 0.2401945
L   0.2684853 0.00000000 0.16945115 0.7778177 0.2574235
NL  0.1120211 0.16945115 0.00000000 0.6083665 0.1281733
P   0.5093324 0.77781766 0.60836651 0.0000000 0.5806957
UK  0.2401945 0.25742348 0.12817333 0.5806957 0.0000000 

## Example 2 in reference, extended  ---  different ways of "mixed" / "gower":

example(flower) # -> data(flower) *and* provide 'flowerN'

summary(d0    <- daisy(flower))  # -> the first 3 {0,1} treated as *N*ominal
summary(dS123 <- daisy(flower,  type = list(symm = 1:3))) # first 3 treated as *S*ymmetric
stopifnot(dS123 == d0) # i.e.,  *S*ymmetric <==> *N*ominal {for 2-level factor}
summary(dNS123<- daisy(flowerN, type = list(symm = 1:3)))
stopifnot(dS123 == d0)
## by default, however ...
summary(dA123 <- daisy(flowerN)) # .. all 3 logicals treated *A*symmetric binary (w/ warning)
summary(dA3  <- daisy(flower, type = list(asymm = 3)))
summary(dA13 <- daisy(flower, type = list(asymm = c(1, 3), ordratio = 7)))
## Mixing variable *names* and column numbers (failed in the past):
summary(dfl3 <- daisy(flower, type = list(asymm = c("V1", "V3"), symm= 2,
                                          ordratio= 7, logratio= 8)))

## If we'd treat the first 3 as simple {0,1}
Nflow <- flower
Nflow[,1:3] <- lapply(flower[,1:3], function(f) as.integer(as.character(f)))
summary(dN <- daisy(Nflow)) # w/ warning: treated binary .. 1:3 as interval
## Still, using Euclidean/Manhattan distance for {0-1} *is* identical to treating them as "N" :
stopifnot(dN == d0)
stopifnot(dN == daisy(Nflow, type = list(symm = 1:3))) # or as "S"



