https://cran.r-project.org/web/packages/tseries/refman/tseries.html#portfolio.optim
Portfolio Optimization
library(tseries)
require("zoo")			# For diff() method.
data(EuStockMarkets)
str(EuStockMarkets)
 Time-Series [1:1860, 1:4] from 1991 to 1999: 1629 1614 1607 1621 1618 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "DAX" "SMI" "CAC" "FTSE"
head(EuStockMarkets)
Time Series:
Start = c(1991, 130) 
End = c(1991, 135) 
Frequency = 260 
             DAX    SMI    CAC   FTSE
1991.496 1628.75 1678.1 1772.8 2443.6
1991.500 1613.63 1688.5 1750.5 2460.2
1991.504 1606.51 1678.6 1718.0 2448.2
1991.508 1621.04 1684.1 1708.1 2470.4
1991.512 1618.16 1686.6 1723.1 2484.7
1991.515 1610.61 1671.6 1714.3 2466.8

X <- diff(log(as.zoo(EuStockMarkets)))
str(X)
zooreg’ series from 1991.5 to 1998.64615384615
  Data: num [1:1859, 1:4] -0.00933 -0.00442 0.009 -0.00178 -0.00468 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "DAX" "SMI" "CAC" "FTSE"
  Index:  num [1:1859] 1992 1992 1992 1992 1992 ...
  Frequency: 260 
### pw : the portfolio weights.
res <- portfolio.optim(X)                 ## Long only
res$pw
res <- portfolio.optim(X, shorts=TRUE)    ## Long/Short
res$pw

Description: This function computes the Sharpe ratio of the univariate time series
dax <- log(EuStockMarkets[,"DAX"])
ftse <- log(EuStockMarkets[,"FTSE"])
sharpe(dax)
sharpe(ftse)

Description
This function computes the Sterling ratio of the univariate time serie
dax <- log(EuStockMarkets[,"DAX"])
ftse <- log(EuStockMarkets[,"FTSE"])
sterling(dax)
sterling(ftse)