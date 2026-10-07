par(mfrow=c(2,2))


f_x<- function(x){
result<- 2*x^2+x+1
return(result)
}
curve(f_x, -5, 5)

g_y<- function(y){
result<- sin(y)
return(result)
}
curve(g_y, -5,5)

g_f_x<- function(x){
result<- sin(2*x^2+x+1)
return(result)
}
curve(g_f_x, -5,5)

f_g_y<- function(y){
result<- 2*sin(y)+sin(y)+1
return(result)
}
curve(f_g_y, -5,5)