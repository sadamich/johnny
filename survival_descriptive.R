### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)
str(xm609)
table(STRIKEDUR)
STRIKEDUR
  1   2   3   4   5   7   8   9  10  11  12  13  14  15  17  19  21  22  23  25 
  1   3   5   1   1   1   1   2   1   1   2   1   1   1   1   1   2   1   1   1 
 26  27  28  29  32  33  35  37  38  41  42  43  44  49  52  61  72  85  98  99 
  1   2   1   1   1   1   1   1   1   1   1   2   1   2   2   1   1   1   1   1 
100 104 114 117 119 130 152 153 216 
  1   1   1   1   1   1   1   1   1 

x<- STRIKEDUR
hist(x)
library(REdaS)
densbox(x~1, main="Strike")
curve(dweibull(x, shape=1, scale = 1, log = FALSE))
curve(dweibull(x, shape=1.5, scale = 1, log = FALSE))
curve(dweibull(x, shape=3, scale = 1, log = FALSE))
curve(dweibull(x, shape=1, scale = 3, log = FALSE))
curve(dweibull(x, shape=0.5, scale = 1, log = FALSE))


### Exhibit 6 14  (a)(p.512)
summary(x)
   Min.  1st Qu.  Median   Mean  3rd Qu.    Max. 
   1.00   10.25   27.00   42.68   51.25  216.00

skew<- function(y,mean,n,sd){
result<- 1/n*(sum ((y - mean)^3))/sd^3
return(result)
}
skew(x,mean(x),62,sd)
[1] 1.58493
kurt<-  function(y,mean,n,sd){
result<- (1/n*(sum ((y - mean)^4)))/s^4
return(result)
}

kurt(x,mean(x),62,sd(x))
[1] 5.229548

### Exhibit 6 14 (b) (p.512) 
y<- log(x)
summary(y)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   2.326   3.296   3.104   3.936   5.375 

hist(y)
skew(y,mean(y),62,sd(y))
[1] -0.4376157

kurt(y,mean(y),62,s)
[1] 2.354172


x1<- x[x<=10.25]
str(x1)
int [1:16] 1 2 2 2 3 3 3 3 3 4 ...
x2<- x[x<=27]
str(x2)
int [1:32] 1 2 2 2 3 3 3 3 3 4 ...
x3<- x[x<=51.25]
str(x3)
 int [1:46] 1 2 2 2 3 3 3 3 3 4 .
x4<- x[x<=216]
str(x4)
int [1:62] 1 2 2 2 3 3 3 3 3 4 ...
quan<- c(x1,x2,x3,x4)
q<- c(16,16,14,16)
plot(q)


### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Seite 306
grenzen<- c(0,10,20,30,40,50,60,70,80,90,100,150,200) ???
hist(STRIKEDUR, breaks=grenzen, main = "Strike duration", xlab  = "days")
### seite 308 
tdauer<- table(STRIKEDUR)
n<- sum(tdauer)
prozent<- tdauer*100/n
kumproz<- cumsum(prozent)
round(cbind(absolut = tdauer, Prozent = prozent, kumuliert = kumproz), digits=2)
### Quantile Seite 313
minimum <- min(x)
quartil_1<- quantile(x, 0.25)
quartil_3<- quantile(x, 0.75)
maximum<- max(x)
rbind(minimum, quartil_1, quartil_3, maximum)
          25%
minimum     1.00
quartil_1  10.25
quartil_3  51.25
maximum   216.00

### Boxplot Seite 317 
boxplot(x, ylab= "Strike duration in days")

x1<- x[x<=25]
str(x1)
x2<- x[x<=50]
str(x2)
x3<- x[x<=100]
str(x3)
x4<- x[x<=125]
str(x4)
x5<- x[x<=150]
str(x5)
x6<- x[x<=175]
str(x6)
x7<- x[x<=200]
str(x7)
x8<- x[x<=225]
str(x8)

xx<- 1/62*c(29, 46, 54,58,59,61,62)

### The empirical survival function                                                
hist(STRIKEDUR)
hist(STRIKEDUR, freq=FALSE)

sur<- 1 - xx
str(sur)
plot(sur,type ="l", main = "Suvival function",xlab= "STRIKEDURATION")
plot(xx, type="l", sub= "STRIKEDUR")

### Seite 333  ???
hist(STRIKEDUR, freq=FALSE, main="Strike duration")
curve(dexp(x, rate=1),add=TRUE, col="red")
