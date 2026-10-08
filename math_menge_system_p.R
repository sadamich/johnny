library(eulerr)
https://de.wikipedia.org/wiki/Potenzmenge
https://de.wikipedia.org/wiki/%CE%A3-Algebra
https://de.wikipedia.org/wiki/Dynkin-System
menge1<- list(A=c(1,2,3,4,5,6), B=c(1,3,5),C=c(2,4,6))
plot(venn(menge1))
plot(euler(menge1, shape = "ellipse"), quantities = TRUE)

menge2<- list(A=c(1,2,3,4,5,6), B=c(1,3,5),C=c(2,4,6),
              D=c(1,2), E=c(1,3))
plot(venn(menge2))


po<- list(A= c(1), B= c(1,2),
             C =c(2,3), D=c(1,2,3))
plot(venn(po))
plot(euler(po, shape = "ellipse"), quantities = TRUE)