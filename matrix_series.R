seq(70, 100, 10)
seq(70, 100, 5)
seq(70, 100, 2)
seq(70, 100, 1)
seq(1,100, 1)
rep(0, 8)
rep(1, 8)
rep(2, 8)
rep(5, 8)
rep(10, 8)

sum(seq(70, 100, 10))
sum(seq(70, 100, 5))
sum(seq(70, 100, 2))
sum(seq(70, 100, 1
sum(rep(0,8))
sum(rep(1,8))
sum(rep(2,8))
sum(rep(5,8))
sum(rep(10,8))

w_testmt<- matrix(c(2,3,6,3,9,10,2,0,3,8,2,9,10,8,7),nrow=5)
w_colsum<- numeric(nrow(w_testmt))
for (i in 1:5){
w_colsum[i]<- sum(w_testmt[i,]*c(1,0.5,0.3))
}
w_colsum

W<- w_testmt
v<- c(1,0.5,0.3)
W%*%v

aq<- function(a, q){
result<- sum(a*1/10^q)
return(result)
}
aq(0.3, 1)
[1] 0.03
aq(0.3, 2)
[1] 0.003
aq(0.3,5)
[1] 3e-06
aq(0.3, 1:6)
[1] 0.0333333

### pi^2/6 
n<- 1:5
s<- sum(1/n^2)
s
[1] 1.463611
1+1/2^2+1/3^2+1/4^2+1/5^2
[1] 1.463611
text(1,3,sum(1/n^2, n==1,100))
n<- 1:100000
s<- sum(1/n^2)
s
[1] 1.644924     (near to pi^2/6) 
pi^2/6
[1] 1.644934
plot.new()
plot.window(c(0,15),c(15,1))
text(6,2, expression(sum(1/n^2,n==1,Inf)==pi^2/6))
text(6,4, expression(sum(1/n^2,n==1,10)==1.549768))
text(6,6, expression(sum(1/n^2,n==1,100)==1.634984))
text(6,8, expression(sum(1/n^2,n==1,10000)==1.644834))
text(6,10, expression(pi^2/6==1.644934))
### 1/3       

n<- 0:10
n2<- 0:100
s<- sum(0.3*(0.1^n))
s
[1] 0.3333333
n1<- 0:50
s1<- sum(0.3*(0.1^n1))
s1
n2<- 0:100
s2<- sum(0.3*(0.1^n2))
s2