### Source:

set.seed(250)
## white noise series
w <- rnorm(n = 200, mean = 0, sd = 1)
## moving average
y_ma <- stats::filter(w, rep(1 / 3, 3))
plot.ts(y_ma, main = "Moving Average Series", ylab = "")

ma1_1 <- arima.sim(list(order = c(0, 0, 1), ma = 0.6), n = 100)
ma1_2 <- arima.sim(list(order = c(0, 0, 1), ma = -0.6), n = 100)

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

p1 <- autoplot(acf(ma1_1, plot = FALSE)) +
  ggtitle(expression("Serial Correlation " * MA(1) ~ ~ ~ theta == 0.6)) +
  xlim(1, 20) +
  ylim(-0.5, 0.5)

p2 <- autoplot(acf(ma1_2, plot = FALSE)) +
  ggtitle(expression("Serial Correlation " * MA(1) ~ ~ ~ theta == -0.6)) +
  xlim(1, 20) +
  ylim(-0.5, 0.5)

grid.arrange(p1, p2, ncol = 1)
