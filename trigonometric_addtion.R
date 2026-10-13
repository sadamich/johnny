sin(pi/4)
[1] 0.7071068
sin(pi/3)
[1] 0.8660254
cos(pi/4)
[1] 0.7071068
cos(pi/3)
[1] 0.5

sin(pi/4+pi/3)
[1] 0.9659258
sin(pi/4)*cos(pi/3)+cos(pi/4)*sin(pi/3)
[1] 0.9659258

cos(pi/4+pi/3)
[1] -0.258819

cos(pi/4)*cos(pi/3)-sin(pi/4)*sin(pi/3)
[1] -0.258819

f<- function(a,b){
result<- sin(a)*cos(b)+cos(a)*sin(b)
return(result)
}
f(pi/3,pi/4)
[1] 0.9659258

