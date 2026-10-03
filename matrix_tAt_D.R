A<- matrix(c(1,-1,-1,3), nrow=2,ncol=2)
x<- c(3,4)
y<- c(1,1)
z<- cbind(x,y)
u<- t(z)%*%A%*%z
u

u1<- function(x,y){
result<- x^2-2*x*y+3*y^2
return(result)
}
u1(3,1)


B<- matrix(c(0,1,1,1,0,1,1,1,0), nrow=3,ncol=3)
x<- c(1,1,1)
y<- c(1,1,1)
z<- c(1,1,1)
w<- cbind(x,y,z)
v<- t(w)%*%B%*%w
v

B1<- function(x,y,z){
result<- 2*x*y+2*y*z+2*z*x
return(result)
}
B1(1,1,1)

C<- matrix(c(1,-1,-1,3), nrow=2,ncol=2)
v1<- c(1,0)
s<- t(v1)%*%C
p<- t(s)
p
  [,1]
[1,]    1
[2,]   -1
v2<- c(1,1)     ### 1 -1 = 0 for v1*C*v2 ###
T<- cbind(v1,v2)
t(T)%*%C%*%T
   v1 v2
v1  1  0
v2  0  2        ### Diagonalisierung T'CT = D ###

E<- matrix(c(0,1,1,0), nrow=2,ncol=2)
v1<- c(1,0)
s<- t(v1)%*%E
p<- t(s)
p
     [,1]
[1,]    0
[2,]    1       ### v1 = v2 : not optimal ###
v1<- c(1,1)
s<- t(v1)%*%E
p<- t(s)
p
     [,1]
[1,]    1
[2,]    1     
v2<- c(1,-1)    ### 1 - 1 =0 for v1*E*v2 ###
T<- cbind(v1,v2)
t(T)%*%E%*%T
   v1 v2
v1  2  0
v2  0 -2        ### Diagonalisierung T'ET = D ###

### Cholesky Zerlegung 
https://de.wikipedia.org/wiki/Cholesky-Zerlegung
A<- matrix(c(4,12,-16,12,37,-43,-16,-43,98),nrow= 3, ncol=3)
L<- matrix(c(1,3,-4,0,1,5,0,0,1), nrow=3,ncol=3)
D<- matrix(c(4,0,0,0,1,0,0,0,9), nrow=3,ncol=3)
Z<- L%*%D%*%t(L)
Z
    [,1] [,2] [,3]
[1,]    4   12  -16
[2,]   12   37  -43
[3,]  -16  -43   98     ### A = L*D*L'
G<- matrix(c(2,6,-8,0,1,5,0,0,3), nrow=3,ncol=3)
V<- G%*%t(G)
V
  [,1] [,2] [,3]
[1,]    4   12  -16
[2,]   12   37  -43
[3,]  -16  -43   98

https://de.wikipedia.org/wiki/Satz_von_Courant-Fischer

