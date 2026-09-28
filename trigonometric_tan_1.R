
### tan inverse ###
f_tan<- function(x){
result<- x - 1/3*x^3+ 1/5*x^5 - 1/7*x^7 + 1/9*x^9
return(result)
}
curve(f_tan, -3,3)

curve(atan(x),-3,3)

sum(1 - 1/3 + 1/5 -1/7 + 1/9 - 1/11 + 1/13 - 1/15 + 1/17)
[1] 0.8130915

pi/4
[1] 0.7853982