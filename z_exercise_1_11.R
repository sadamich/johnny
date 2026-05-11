### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Exercise 1 11 (p.73)                                                   ###
xr111<- read.csv("xr111.csv", header=TRUE)
str(xr111)
attach(xr111)
### Problem (a)                                                            ###
mean(FGPA)
[1] 2.6888
median(FGPA)
[1] 2.642
sd(FGPA)
[1] 0.5395471

mean(SATM)
[1] 5.83
median(SATM)
[1] 5.95
sd(SATM)
[1] 0.3560587

mean(FEM)
[1] 0.6
median(FEM)
[1] 1
sd(FEM)
[1] 0.5163978


skew<- function(y,mean,n){
result<- 1/n*(sum(y - mean)^3)
return(result)
}
skew(FGPA,mean(FGPA),10)
[1] -2.955864e-47
skew(SATM,mean(SATM),10)
[1] 7.006492e-47
skew(FEM, mean(FEM),10)
[1] 1.094764e-48

kurt<-  function(y,mean,n){
result<- 1/n*(sum(y - mean)^4)
return(result)
}
kurt(FGPA,mean(FGPA),10)
[1] 1.969001e-62
kurt(SATM, mean(SATM),10)
[1] 6.223015e-62
kurt(FEM, mean(FEM),10)
[1] 2.430865e-64

### Problem (b)                                                            ###
var(FGPA)
[1] 0.2911111
var(SATM)
[1] 0.1267778
var(FEM)
[1] 0.2666667

cov(FGPA,SATM)
[1] 0.06561778
cov(FGPA,FEM)
[1] -0.05942222
cov(SATM,FEM)
[1] 0.04666667

cor(FGPA,SATM)
[1] 0.3415628
cor(FGPA,FEM)
[1] -0.2132726
cor(SATM,FEM)
[1] 0.2538054

cv1<- c(0.2911111,0.06561778,-0.05942222)
cv2<- c(0.06561778,0.1267778,0.04666667)
cv3<- c(-0.05942222,0.04666667,0.2666667)
CV<- cbind(cv1,cv2,cv3)
CV
  cv1        cv2         cv3
[1,]  0.29111110 0.06561778 -0.05942222
[2,]  0.06561778 0.12677780  0.04666667
[3,] -0.05942222 0.04666667  0.26666670

### Problem (c)                                                            ###
hist(FGPA)
hist(SATM)
hist(FEM)
plot(FGPA,SATM)
plot(FGPA,FEM)
plot(SATM,FEM)

### Problem (d)

### Problem (e)

Mean[FGPA| 4 male]
FGPA_m<- FGPA[FEM==0]
mean(FGPA_m)
[1] 2.8225
Mean[SATM | 4 male]
SATM_m<- SATM[FEM==0]
mean(SATM_m)
[1] 5.725

Female[FGPA | 6 female]
FGPA_f<- FGPA[FEM==1]
mean(FGPA_f)
[1] 2.599667
Female[SATM | 6 female]
SATM_f<- SATM[FEM==1]
mean(SATM_f)
[1] 5.9
