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