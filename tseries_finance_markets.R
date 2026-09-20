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



