### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm609<- read.csv("xm609.csv", header = TRUE)
attach(xm609)

x<- STRIKEDUR

hist(x)
summary(x)

   Min.  1st Qu.  Median   Mean  3rd Qu.    Max. 
   1.00   10.25   27.00   42.68   51.25  216.00

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

