https://cran.r-project.org/web/packages/gmm/refman/gmm.html#estfun
library(gmm)
n = 500
phi<-c(.2,.7)
thet <- 0
sd <- .2
x <- matrix(arima.sim(n=n,list(order=c(2,0,1),ar=phi,ma=thet,sd=sd)),ncol=1)
y <- x[7:n]
ym1 <- x[6:(n-1)]
ym2 <- x[5:(n-2)]
H <- cbind(x[4:(n-3)], x[3:(n-4)], x[2:(n-5)], x[1:(n-6)])
g <- y ~ ym1 + ym2
x <- H
res <- gmm(g, x,weightsMatrix = diag(5))
summary(res)
### the structure of variables                                             ###
str(y)
 num [1:494] 0.87 1.96 1.05 2.7 1.98
str(x)
num [1:494, 1:4] 1.52 3.18 3.84 0.87 1.96 ...

str(ym1)
 num [1:494] 3.84 0.87 1.96 1.05 2.7 ..
str(ym2)
num [1:494] 3.18 3.84 0.87 1.96 1.05 .
str(H)
num [1:494, 1:4] 1.52 3.18 3.84 0.87 1.96 ...

str(phi)
 num [1:2] 0.2 0.7
str(thet)


gt <- res$gt
 num [1:494, 1:5] -1.898 -0.969 0.223 1.156 0.934 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:494] "1" "2" "3" "4" ...
  ..$ : chr [1:5] "(Intercept)" "h1" "h2" "h3" ...
G <- res$G
str(G)
num [1:5, 1:3] -1 0.199 0.194 0.192 0.184 ...

foc <- gt
foc2 <- estfun(res)

foc[1:5,]
foc2[1:5,]

### the histogram of 
plot(foc2)
hist(foc2)