https://en.wikipedia.org/wiki/Basel_problem

n<- 1:100000
basel<- sum(1/n^2)
basel
[1] 1.644924
pi^2/6
[1] 1.644934

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