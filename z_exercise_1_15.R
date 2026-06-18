### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 1 15 (p.74)

### Problem (a)

x<- rnorm(10, 0,1)
x_s<- sample(x,5)
x_s
z<- median(x_s)
z
[1] -0.07684073

x<- rnorm(10, 0,1)
x_s2<- sample(x,5)
z2<- median(x_s2)
z2
[1] 0.3082674

x<- rnorm(10, 0,1)
x_s3<- sample(x,5)
z3<- median(x_s3)
z3
[1] 0.3744439

x<- rnorm(10, 0,1)
x_s4<- sample(x,5)
z4<- median(x_s4)
z4
[1] -0.02560985

x<- rnorm(10, 0,1)
x_s5<- sample(x,5)
z5<- median(x_s5)
z5
[1] 0.8977068
s_z<- c(-0.07684073,0.3082674,0.3744439,-0.02560985,0.8977068)
sd(s_z)
[1] 0.3907676

s_m<- function(sigma, n){
result<- sigma*sqrt(pi)/sqrt(2*n)
return(result)
}
s_m(sd(x_s),10)
[1] 0.3922418

### Problem (b)  Cauchy distribution
https://de.wikipedia.org/wiki/Cauchy-Verteilung
c<- rcauchy(30, location = 0, scale = 1)
z_c<- mean(c)
z_c
[1] 0.8332047

c2<- rcauchy(30, location = 0, scale = 1)
z_c2<- mean(c2)
z_c2
[1] -0.3978357

c3<- rcauchy(30, location = 0, scale = 1)
z_c3<- mean(c3)
z_c3
[1] 1.901652

c4<- rcauchy(30, location = 0, scale = 1)
z_c4<- mean(c4)
z_c4
[1] 0.5509262
c5<- rcauchy(30, location = 0, scale = 1)
z_c5<- mean(c5)
z_c5
[1] 0.03881944
m_c<- c( 0.8332047,-0.3978357,1.901652,0.5509262,0.03881944)
sd(m_c)?????
sd(t_c_2)
### Problem (c)
set.seed(40)
z<- rt(1000,1)
z1<- sample(z, 1000)
plot(z1, type="l")
hist(z1)
mean(z1)


### Problem (d)

### Problem (e)

### problem (f)


### Problem (g)


