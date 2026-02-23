### Compare with the Example 7 27 Interest and bond rates p.674            ###
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
xm722<- read.csv("xm722.csv", header = TRUE)
str(xm722)
attach(xm722)
library(tsDyn)
var_data<- data.frame(AAA,US3MTBIL)
panel05<- VECM(
  data = var_data,
  lag = 2,
  r = 1,
  include = "const",
  beta = NULL,
  estim = "ML",
  LRinclude = "both",
  exogen = NULL
)
panel05

panel06<- VECM(
  data = var_data,
  lag = 2,
  r = 1,
  include = "none",
  beta = NULL,
  estim = "ML",
  LRinclude = "none",
  exogen = NULL
)
panel06

panel05_t<- ca.jo(var_data, type = "eigen", ecdet = "trend", K = 2,
spec= "longrun", season = NULL, dumvar = NULL)
summary(panel05_t)

panel05_t2<- ca.jo(var_data, type = "trace", ecdet = "trend", K = 2,
spec= "longrun", season = NULL, dumvar = NULL)
summary(panel05_t2)

library(urca)
