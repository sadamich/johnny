library(tseries)

x <- rnorm(1000)
x
hist(x)
dim(x) <- c(500,2)
dim(x)
[1] 500   2       ### Matrix (500 * 2)
res <- portfolio.optim(x)
res$pw

require("zoo")
data(EuStockMarkets)			# For diff() method.
str(EuStockMarkets)
Time-Series [1:1860, 1:4] from 1991 to 1999: 1629 1614 1607 1621 1618 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "DAX" "SMI" "CAC" "FTSE"

X <- diff(log(as.zoo(EuStockMarkets)))
res <- portfolio.optim(X)                 ## Long only
res$pw
res <- portfolio.optim(X, shorts=TRUE)    ## Long/Short
res$pw
res$px
res$pm
res$ps

pw	
the portfolio weights.
px	
the returns of the overall portfolio.
pm	
the expected portfolio return.
ps	
the standard deviation of the portfolio returns.


dax <- log(EuStockMarkets[,"DAX"])
ftse <- log(EuStockMarkets[,"FTSE"])
sharpe(dax)
sharpe(ftse)
Details
The Sharpe ratio is defined as a portfolio's mean return in excess of the
 riskless return divided by the portfolio's standard deviation. In finance
 the Sharpe Ratio represents a measure of the portfolio's risk-adjusted 
(excess) return.


sterling(dax)
sterling(ftse)
Details
The Sterling ratio is defined as a portfolio's overall return divided by 
the portfolio's maxdrawdown statistic. In finance the Sterling Ratio 
represents a measure of the portfolio's risk-adjusted return.

