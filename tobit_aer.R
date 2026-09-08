https://cran.r-project.org/web/packages/AER/refman/AER.html#tobit
library(AER)
data("Affairs")

## from Table 22.4 in Greene (2003)
fm.tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
  data = Affairs)
fm.tobit2 <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
  right = 4, data = Affairs)
summary(fm.tobit)
summary(fm.tobit2)

xm601<- read.csv("xm601.csv", header = TRUE)
attach(xm601)
str(xm601)
detach(xm601)
AGE_2<- AGE^2/100


fm.tobit <- tobit(LOGINV~GENDER+ACTIVITY+AGE+AGE_2,
  data = xm601)
fm.tobit2 <- tobit(LOGINV~GENDER+ACTIVITY+AGE+AGE_2,
  left = 0, data = xm601)
summary(fm.tobit)
summary(fm.tobit2)????