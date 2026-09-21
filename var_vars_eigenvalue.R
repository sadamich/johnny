### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press, p.665 and p.677       ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 26 Interest and bond rates p.663 ###
xm722<- read.csv("xm722.csv", head=TRUE)
str(xm722)
attach(xm722)
library("vars")
VAR_data <- data.frame(DAAA[25:624], DUS3MT[25:624])
eq_var<- VAR(VAR_data, p = 1)
eq_var
roots(eq_var)
[1] 0.2813355 0.2813355
eq_var2<- VAR(VAR_data, p = 2)
eq_var2
[1] 0.4686267 0.4686267 0.4402689 0.4402689
https://cran.r-project.org/web/packages/vars/refman/vars.html#roots
data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
roots(var.2c)
[1] 0.9950338 0.9081062 0.9081062 0.7380565 0.7380565 0.1856381 0.1428889
[8] 0.1428889

a<- c(2,-1)
b<- c(1/2, -1)
A<- cbind(a,b)
A_eigen<- eigen(A)
A_eigen
eigen() decomposition
$values
[1]  1.8228757 -0.8228757
$vectors
           [,1]       [,2]
[1,]  0.9426029 -0.1744096
[2,] -0.3339158  0.9846732

y<- c(-1,1/2)
x<- y%*%solve(A)
x
  [,1] [,2]
[1,]   -1   -1
v1<- c(0.9426029, -0.3339158)
v2<- c(-0.1744096, 0.9846732)
V<- cbind(v1,v2)
d1<- c(1.8228757, 0)
d2<- c(0,-0.8228757 )
D<- cbind(d1,d2)
B<- V%*%D%*%t(V)
B
t(B)%*%B
### The solution can be shown in the followring functions
### The function and the inverse function 
f1<- function(x){
result<- 2*x+1
return(result)
}
curve(f1, -10, 10)

f2<- function(x){
result<- 1/2*x -1/2
return(result)
}
curve(f2, -10, 10 , add=TRUE, col="red")

### The decomposition : A = VDV'
x1<- c(1/2, 3/2,7/2)
y<- c(1/2, 5/2, 2)
plot(x1,y)
eq<- lm(y~x1)
summary(eq)
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   0.9464     1.2345   0.767    0.584
x1            0.3929     0.5567   0.706    0.609

fit<- fitted(eq)
lines(fit, add=TRUE, col="red")
x0<- c(1,1,1)
X<- cbind(x0,x1)

XX<- t(X)%*%X
eigen(XX)
eigen() decomposition
$values
[1] 16.9227093  0.8272907
$vectors
          [,1]       [,2]
[1,] 0.3674089 -0.9300595
[2,] 0.9300595  0.3674089
v1<- c(0.3674089, 0.9300595 )
v2<- c(-0.9300595, 0.3674089)
V<- cbind(v1,v2)
d1<- c( 16.9227093, 0)
d2<- c(0, 0.8272907)
D<- cbind(d1,d2)
A<- V%*%D%*%t(V)
A
  [,1]  [,2]
[1,]  3.0  5.50
[2,]  5.5 14.75
XX
    x0    x1
x0 3.0  5.50
x1 5.5 14.75
### XX==A==VDV' : the decomposition is OK
b<- solve(XX)%*%t(X)%*%y
b
        [,1]
x0 0.9464286
x1 0.3928571

