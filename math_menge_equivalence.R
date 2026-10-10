library(eulerr)
https://de.wikipedia.org/wiki/%C3%84quivalenzrelation#%C3%84quivalenzklassen
menge1<- list(A=c(1,7,10,13,17), B=c(2,5,8,16),C=c(3,4,6,11,18),
              D=c(9,12,14,15),E=c(19))
plot(venn(menge1))
plot(euler(menge1, shape = "ellipse"), quantities = TRUE)