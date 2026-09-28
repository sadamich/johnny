

f<- function(x){
result<- exp(1/2*x^2)
return(result)
}
curve(f, -2,2)
f(-5)
[1] 268337.3
f(-4)
[1] 2980.958
f(-3)
[1] 90.01713
f(-2)
[1] 7.389056
f(-1)
[1] 1.648721

f_2<- function(x){
result<- exp(-1/2*x^2)
return(result)
}
curve(f_2, -5,5)