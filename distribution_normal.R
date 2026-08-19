### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### P. 29 - 31

### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Seite 315 - 316 

dnorm(1.96)
[1] 0.05844094
pnorm(1.96)
[1] 0.9750021
qnorm(0.9750021)
[1] 1.96

f<- function(x){
result<- 1/sqrt(2*pi)*exp(-1/2*x^2)
return(result)
}
f(1.96)
[1] 0.05844094


https://search.r-project.org/R/refmans/stats/html/Normal.html

The normal distribution : Usage
dnorm(x, mean = 0, sd = 1, log = FALSE)
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean = 0, sd = 1)
### Descritive statistics / deskritive Statistik                           ###
curve(dnorm(x, 0,1), -4, 4, main = "Normal distribution (PDF)")
curve(pnorm(x, 0,1), -4, 4, main = "Normal distribution (CDF)")
curve(qnorm(x), -4, 4, main = "Normal distribution (Quantil)")
A<- pnorm(1.96)
B<- pnorm(-1.96)
A-B
[1] 0.9500042
AA<- pnorm(2.58)
BB<- pnorm(-2.58)
AA - BB
[1] 0.99012

dnorm(1.96)
[1] 0.05844094
x<- pnorm(1.96)
x
[1] 0.9750021
qnorm(x)
[1] 1.96
dnorm(-1.96)
[1] 0.05844094
y<- pnorm(-1.96)
y
[1] 0.0249979
qnorm(y)
[1] -1.96

### A univariate random variable 
set.seed(37)
x<- rnorm(30, 0,1)
x
 [1]  0.12475399  0.38207459  0.57924277 -0.29374812 -0.82834916 -0.33271359
 [7] -0.19215950  1.36298273  0.85595441  0.21599549 -0.37770210  0.03869354
[13]  1.42481507  0.98230990  0.31046447 -1.66752833 -2.70484969 -1.73692423
[19]  0.40307517 -0.73207272 -0.27836096  1.28423069  0.82402930 -2.38443557
[25]  0.73900226 -0.45904737  0.08286498  0.02402364 -2.10396909  0.46306868
plot(x)
hist(x)

### The density f(x)                                                       ###
dnorm(0.12475399)
[1] 0.3958498
dnorm(x)
 [1] 0.39584985 0.37086060 0.33732797 0.38209634 0.28308171 0.37746112
 [7] 0.39164433 0.15758355 0.27657659 0.38974385 0.37147713 0.39864375
[13] 0.14457057 0.24625076 0.38017157 0.09933434 0.01028525 0.08826678
[19] 0.36781568 0.30516466 0.38378187 0.17489618 0.28409384 0.02324408
[25] 0.30361322 0.35904743 0.39757494 0.39882718 0.04361817 0.35838235

pnorm(0.12475399 )
[1] 0.5496408
pnorm(x)
 [1] 0.549640842 0.648796979 0.718787311 0.384475189 0.203736396 0.369675247
 [7] 0.423808634 0.913556023 0.803988502 0.585504367 0.352825953 0.515432640
[13] 0.922894667 0.837026401 0.621896111 0.047704698 0.003416765 0.041200273
[19] 0.656553537 0.232062092 0.390367641 0.900469375 0.795038545 0.008552672
[25] 0.770047186 0.323100073 0.533020551 0.509583123 0.017690572 0.678342432

p<- pnorm(x)
qnorm(0.549640842)
[1] 0.124754 ### The invers - relation between x and commutative probability
qnorm(0.12475399) 
qnorm(p)
qnorm(p)
 [1]  0.12475399  0.38207459  0.57924277 -0.29374812 -0.82834916 -0.33271359
 [7] -0.19215950  1.36298273  0.85595441  0.21599549 -0.37770210  0.03869354
[13]  1.42481507  0.98230990  0.31046447 -1.66752833 -2.70484969 -1.73692423
[19]  0.40307517 -0.73207272 -0.27836096  1.28423069  0.82402930 -2.38443557
[25]  0.73900226 -0.45904737  0.08286498  0.02402364 -2.10396909  0.46306868
x
 [1]  0.12475399  0.38207459  0.57924277 -0.29374812 -0.82834916 -0.33271359
 [7] -0.19215950  1.36298273  0.85595441  0.21599549 -0.37770210  0.03869354
[13]  1.42481507  0.98230990  0.31046447 -1.66752833 -2.70484969 -1.73692423
[19]  0.40307517 -0.73207272 -0.27836096  1.28423069  0.82402930 -2.38443557
[25]  0.73900226 -0.45904737  0.08286498  0.02402364 -2.10396909  0.46306868

require(graphics)
dnorm(0) == 1/sqrt(2*pi)
dnorm(1) == exp(-1/2)/sqrt(2*pi)
dnorm(1) == 1/sqrt(2*pi*exp(1))

## Using "log = TRUE" for an extended range :
par(mfrow = c(2,1))
plot(function(x) dnorm(x, log = TRUE), -60, 50,
     main = "log { Normal density }")
curve(log(dnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("dnorm(x, log=TRUE)", adj = 0)
mtext("log(dnorm(x))", col = "red", adj = 1)

plot(function(x) pnorm(x, log.p = TRUE), -50, 10,
     main = "log { Normal Cumulative }")
curve(log(pnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("pnorm(x, log=TRUE)", adj = 0)
mtext("log(pnorm(x))", col = "red", adj = 1)

## if you want the so-called 'error function'
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
## (see Abramowitz and Stegun 29.2.29)
## and the so-called 'complementary error function'
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
## and the inverses
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)
erfcinv <- function (x) qnorm(x/2, lower = FALSE)/sqrt(2)



