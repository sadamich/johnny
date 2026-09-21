library(tseries)

### S&P 500 : The time series
con <- url("https://finance.yahoo.com")
if(!inherits(try(open(con), silent = TRUE), "try-error")) {
  close(con)
nDays <- 50
  instrument <- "^gspc"
  start <- strftime(as.POSIXlt(Sys.time() - nDays * 24 * 3600),
                    format="%Y-%m-%d") 
  end <- strftime(as.POSIXlt(Sys.time()), format = "%Y-%m-%d") 
  x <- get.hist.quote(instrument = instrument, start = start, end = end,
                      retclass = "ts")

  plotOHLC(x, ylab = "price", main = instrument)
}

### SBUX
con <- url("https://finance.yahoo.com")
if(!inherits(try(open(con), silent = TRUE), "try-error")) {
  close(con)
nDays <- 50
  instrument <- "SBUX"
  start <- strftime(as.POSIXlt(Sys.time() - nDays * 24 * 3600),
                    format="%Y-%m-%d") 
  end <- strftime(as.POSIXlt(Sys.time()), format = "%Y-%m-%d") 
  x <- get.hist.quote(instrument = instrument, start = start, end = end,
                      retclass = "ts")

  plotOHLC(x, ylab = "price", main = instrument)
}
str(x)
 Time-Series [1:47, 1:4] from 46237 to 46283: 108 103 106 106 104 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "Open" "High" "Low" "Close"
head(x)
Time Series:
Start = 46237 
End = 46242 
Frequency = 1 
        Open   High    Low  Close
46237 107.70 108.36 103.30 103.37
46238 103.06 105.66 101.80 104.97
46239 105.99 106.42 104.29 106.00
46240 106.14 106.29 103.69 105.16
46241 104.03 106.85 103.67 105.58
46242     NA     NA     NA     NA
attach(x)
head(x)
data_x<- data.frame(x)
attach(data_x)
head(Close)
[1] 103.37 104.97 106.00 105.16 105.58     NA
con <- url("https://finance.yahoo.com")
if(!inherits(try(open(con), silent = TRUE), "try-error")) {
  close(con)
nDays <- 50
  instrument <- "PG"
  start <- strftime(as.POSIXlt(Sys.time() - nDays * 24 * 3600),
                    format="%Y-%m-%d") 
  end <- strftime(as.POSIXlt(Sys.time()), format = "%Y-%m-%d") 
  x <- get.hist.quote(instrument = instrument, start = start, end = end,
                      retclass = "ts")

  plotOHLC(x, ylab = "price", main = instrument)
}

con <- url("https://finance.yahoo.com")
if(!inherits(try(open(con), silent = TRUE), "try-error")) {
  close(con)
nDays <- 365
  instrument <- "^N225"
  start <- strftime(as.POSIXlt(Sys.time() - nDays * 24 * 3600),
                    format="%Y-%m-%d") 
  end <- strftime(as.POSIXlt(Sys.time()), format = "%Y-%m-%d") 
  x <- get.hist.quote(instrument = instrument, start = start, end = end,
                      retclass = "ts")

  plotOHLC(x, ylab = "price", main = instrument)
}

