### Source:https://www.geo.fu-berlin.de/en/v/soga-r/Introduction-to-R/index.html ###
set.seed(250)
### AR(1) Model ###
ar1_1 <- arima.sim(list(order = c(1, 0, 0), ar = 0.6), n = 100)
ar1_2 <- arima.sim(list(order = c(1, 0, 0), ar = -0.6), n = 100)
library(ggplot2)
library(ggfortify)
library(gridExtra)

p1 <- autoplot(ar1_1,
  ylab = "y",
  main = (expression(AR(1) ~ ~ ~ phi == 0.6))
)

p2 <- autoplot(ar1_2,
  ylab = "y",
  main = (expression(AR(1) ~ ~ ~ phi == -0.6))
)
grid.arrange(p1, p2, ncol = 1)

### The correlogram for the AR(1) Model ###
p1 <- autoplot(acf(ar1_1, plot = FALSE)) +
  ggtitle(expression("Serial Correlation " * AR(1) ~ ~ ~ phi == 0.6)) +
  xlim(1, 20) +
  ylim(-0.5, 0.5)
p2 <- autoplot(acf(ar1_2, plot = FALSE)) +
  ggtitle(expression("Serial Correlation " * AR(1) ~ ~ ~ phi == -0.6)) +
  xlim(1, 20) +
  ylim(-0.5, 0.5)
grid.arrange(p1, p2, ncol = 1)

### AR(2) Model and the correlogram ###
ar2_I <- arima.sim(list(
  order = c(2, 0, 0),
  ar = c(0.5, 0.3)
), n = 100)

ar2_II <- arima.sim(list(
  order = c(2, 0, 0),
  ar = c(-0.5, 0.3)
), n = 100)

ar2_III <- arima.sim(list(
  order = c(2, 0, 0),
  ar = c(1, -0.5)
), n = 100)

ar2_IV <- arima.sim(list(
  order = c(2, 0, 0),
  ar = c(-0.5, -0.3)
), n = 100)

p1 <- autoplot(acf(ar2_I, plot = FALSE, lag.max = 15)) +
  ggtitle(expression(AR(2) ~ ~ ~ phi[1] == 0.5 * ~ ~ phi[2] == 0.3))

p2 <- autoplot(acf(ar2_II, plot = FALSE, lag.max = 15)) +
  ggtitle(expression(AR(2) ~ ~ ~ phi[1] == -0.5 * ~ ~ phi[2] == 0.3))

p3 <- autoplot(acf(ar2_III, plot = FALSE, lag.max = 15)) +
  ggtitle(expression(AR(2) ~ ~ ~ phi[1] == 1 * ~ ~ phi[2] == -0.5))

p4 <- autoplot(acf(ar2_IV, plot = FALSE, lag.max = 15)) +
  ggtitle(expression(AR(2) ~ ~ ~ phi[1] == -0.5 * ~ ~ phi[2] == -0.3))

grid.arrange(p1, p3, p2, p4, ncol = 2)


### Compare with Exhibit 7.3 (p.542)                                       ###
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
