### 3 5 CAPM 
data(Finance)
str(Finance)
r <-Finance[1:500,1:5]
str(r)
'data.frame':   500 obs. of  5 variables:
 $ WMK : num  0 0.482 0.479 -0.961 -1.459 ...
 $ UIS : num  4.84 3.6 0 -1.12 0 ...
 $ ORB : num  0 7.7 -1.87 -5.83 3.92 ...
 $ MAT : num  1.88 1.42 -1.85 -2 -1.93 ...
 $ ABAX: num  -4.26 12.26 7.41 -7.41 14.31 ...
WMK :Returns of WEIS MARKETS INC
UIS :Returns of UNISYS CP NEW
ORB :Returns of ORBITAL SCIENCES CP
MAT :Returns of Mattel, Inc.
ABAX :Returns of ABAXIS, Inc.

rm <-Finance[1:500,"rm"]
rf <-Finance[1:500,"rf"]
rf :Risk-free rate of Fama-French
rm :Return of the market portfolio of Fama-French
str(rm)
num [1:500] -0.149 0.141 -0.739 -0.389 0.411 0.011 0.491 
str(rf)
 num [1:500] 0.011 0.011 0.011 0.011 0.011 0.011 0.011 0.011 0.011 0

z <-as.matrix(r-rf)
str(z)
 num [1:500, 1:5] -0.011 0.471 0.468 -0.972 -1.47 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:500] "1993-01-05" "1993-01-06" "1993-01-07" "1993-01-08" ...
  ..$ : chr [1:5] "WMK" "UIS" "ORB" "MAT" ...
head(z, 5)
                  WMK       UIS       ORB       MAT      ABAX
1993-01-05 -0.0110000  4.829451 -0.011000  1.870627 -4.266961
1993-01-06  0.4705419  3.587873  7.685104  1.404375 12.249232
1993-01-07  0.4682341 -0.011000 -1.880213 -1.865936  7.399797
1993-01-08 -0.9717760 -1.133556 -5.837891 -2.013292 -7.421797
1993-01-11 -1.4696969 -0.011000  3.911071 -1.939591 14.299084
zm <-as.matrix(rm-rf)
str(zm)
num [1:500, 1] -0.16 0.13 -0.75 -0.4 0.4 0 0.48 0.72 0.36 0.01 ...


res<-gmm(z~zm,x=zm)
coef(res)

### the hypothesis test 
R<-cbind(diag(5),matrix(0,5,5))

c <-rep(0,5)
library(car)
linearHypothesis(res,R,c,test = "Chisq") 

res2<-gmm(z~zm-1,cbind(1,zm))
specTest(res2)

### 3 6 CAPM and Discounting factor 
g5 <- function(tet, x) {
gmat <- (tet[1] + tet[2] * (1 + c(x[, 1]))) * (1 + x[, 2:6])- 1
return(gmat)
}
res_sdf <- gmm(g5, x = as.matrix(cbind(rm, r)), c(0, 0))
specTest(res_sdf)