sin(pi/4)^2
[1] 0.5
(1- cos(2*pi/4))/2
[1] 0.5


cos(pi/4)^2
[1] 0.5
(1+cos(2*pi/4))/2
[1] 0.5

sin(pi/4)*cos(pi/4)
[1] 0.5
(sin(2*pi/4))/2
[1] 0.5

f<- function(a){
result<- (1-cos(2*a))/2
return(result)
}
f(pi/4)

f(pi/3)
[1] 0.75
sin(pi/3)^2
[1] 0.75