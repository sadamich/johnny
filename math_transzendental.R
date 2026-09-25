f<- function(x){
result<- 10*x-x
return(result)
}
curve(f, -1,5)
f(1/9)
[1] 1

9/10
99/100
999/1000

f_1<- function(k){
result<- 0.9*(1/10)^k
return(result)
}
f_1(0)
[1] 0.9
f_1(1)
[1] 0.09
f_1(2)
[1] 0.009

0.99999... = 0.9+0.09+0.009....

x= 0.9999999
10x - x = 9
9x = 9
x= 1