### Sin Satz
a/sin(A) = b/sin(B) = c/sin(C)=2R

sin_satz<- function(a, R){
result<- a/R
return(result)
}
sin_satz(1,2)
[1] 0.5
sin(pi/6)
[1] 0.5
sin_satz(sqrt(3), 2)
[1] 0.8660254
sin(pi/3)
[1] 0.8660254

### Cos Satz
a^2 = b^2 + c^2 - 2*b*c*cos(A)
b^2 = a^2 + c^2 - 2*a*c*cos(B)
c^2 = a^2 + b^2 - 2*a*b*cos(C)

cos_satz<- function(a,b,c){
result<- (b^2 + c^2 - a^2)/(2*b*c)
return(result)
}

cos_satz(1,2,sqrt(3))
[1] 0.8660254
cos(pi/6)
[1] 0.8660254

cos(pi/3)
cos(5/12*pi)
cos(2/3*pi)

