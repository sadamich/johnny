AB+BD+CA=CD

a<- c(3,-1)
b<- c(-4,2)

3*a+2*b
4*a-3*b
-2*(a-b)

sqrt(24^2+ (-10)^2)

sqrt((-14)^2+6^2)
[1] 15.23155
2*sqrt(58)
[1] 15.23155

### Linear algebra
d<- c(8,-3)
v<- c(2,1,-1,3)
X<- matrix(v, nrow=2,ncol=2)
beta<- solve(t(X)%*%X)%*%t(X)%*%d
beta<- solve(X)%*%d
beta
 [,1]
[1,]    3
[2,]   -2
d= 3a-2b  

### Linear dependence
b= ka
a<- c(-2,1)
b<- c(6,-3)
k<- t(a)*b
k
