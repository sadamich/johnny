library(systemfit)
data( "Kmenta" )
str(Kmenta)
'data.frame':   20 obs. of  5 variables:
 $ consump  : num  98.5 99.2 102.2 101.5 104.2 ...
 $ price    : num  100 104 103 105 98 ...
 $ income   : num  87.4 97.6 96.7 98.2 99.8 ...
 $ farmPrice: num  98 99.1 99.1 98.1 110.8 ...
 $ trend    : int  1 2 3 4 5 6 7 8 9 10 ...

eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list( demand = eqDemand, supply = eqSupply )

## perform OLS of the system
fitols <- systemfit( system, data = Kmenta )

## design matrix of the entire system
model.matrix( fitols )

## design matrix of the first equation
model.matrix( fitols$eq[[ 1 ]] )