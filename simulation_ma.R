### Source:https://www.geo.fu-berlin.de/en/v/soga-r/Introduction-to-R/index.html ###
set.seed(250)
### white noise series ###
w <- rnorm(n = 200, mean = 0, sd = 1)
## moving average
y_ma <- stats::filter(w, rep(1 / 3, 3))
plot.ts(y_ma, main = "Moving Average Series", ylab = "")
### MA(1) ###
ma1_1 <- arima.sim(list(order = c(0, 0, 1), ma = 0.6), n = 100)
ma1_2 <- arima.sim(list(order = c(0, 0, 1), ma = -0.6), n = 100)
library(ggplot2)
library(ggfortify)
library(gridExtra)

p1 <- autoplot(ma1_1,
  ylab = "y",
  main = (expression(MA(1) ~ ~ ~ theta == 0.6))
)

p2 <- autoplot(ma1_2,
  ylab = "y",
  main = (expression(MA(1) ~ ~ ~ theta == -0.6))
)

grid.arrange(p1, p2, ncol = 1)
### The correlogram for MA(1) ###
p1 <- autoplot(acf(ma1_1, plot = FALSE)) +
  ggtitle(expression("Serial Correlation " * MA(1) ~ ~ ~ theta == 0.6)) +
  xlim(1, 20) +
  ylim(-0.5, 0.5)

p2 <- autoplot(acf(ma1_2, plot = FALSE)) +
  ggtitle(expression("Serial Correlation " * MA(1) ~ ~ ~ theta == -0.6)) +
  xlim(1, 20) +
  ylim(-0.5, 0.5)

grid.arrange(p1, p2, ncol = 1)

### Compare with Exhibit 7.4 (p.545)                                       ###
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###