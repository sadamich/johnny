### Source:https://www.geo.fu-berlin.de/en/v/soga-r/Introduction-to-R/index.html ###

set.seed(250)

arma_20 <- arima.sim(list(
  order = c(2, 0, 0),
  ar = c(1.5, -0.75)
),
n = 200
) + 50

arma_01 <- arima.sim(list(
  order = c(0, 0, 1),
  ma = -0.7
),
n = 200
) + 50

arma_11 <- arima.sim(list(
  order = c(1, 0, 1),
  ar = 0.9,
  ma = -0.4
),
n = 200
) + 50
library(ggplot2)
library(ggfortify)
library(gridExtra)

p1 <- autoplot(arma_20) +
  ggtitle(expression(AR(2) ~ ~ ~ phi[1] == 1.5 * ~ ~ phi[2] == -0.75))

p2 <- autoplot(arma_01) +
  ggtitle(expression(MA(1) ~ ~ ~ theta[1] == -0.7))

p3 <- autoplot(arma_11) +
  ggtitle(expression(ARMA(1, 1) ~ ~ ~ phi[1] == 0.9 * ~ ~ theta[2] == -0.4))

grid.arrange(p1, p2, p3, ncol = 1)



## ACF
p1 <- autoplot(acf(arma_20,
  plot = FALSE,
  lag.max = 15
)) +
  ggtitle(expression(AR(2) ~ ~ ~ phi[1] == 1.5 * ~ ~ phi[2] == -0.75))

## PACF
p2 <- autoplot(acf(arma_20,
  plot = FALSE,
  lag.max = 15,
  type = "partial"
)) +
  ggtitle(expression(AR(2) ~ ~ ~ phi[1] == 1.5 * ~ ~ phi[2] == -0.75))

grid.arrange(p1, p2, ncol = 2)

## ACF
p1 <- autoplot(acf(arma_01,
  plot = FALSE,
  lag.max = 15
)) +
  ggtitle(expression(MA(1) ~ ~ ~ theta[1] == -0.7))

## PACF
p2 <- autoplot(acf(arma_01,
  plot = FALSE,
  lag.max = 15,
  type = "partial"
)) +
  ggtitle(expression(MA(1) ~ ~ ~ theta[1] == -0.7))
grid.arrange(p1, p2, ncol = 2)

## ACF
p1 <- autoplot(acf(arma_11,
  plot = FALSE,
  lag.max = 15
)) +
  ggtitle(expression(ARMA(1, 1) ~ ~ ~ phi[1] == 0.9 * ~ ~ theta[2] == -0.4))

## PACF
p2 <- autoplot(acf(arma_11,
  plot = FALSE,
  lag.max = 15,
  type = "partial"
)) +
  ggtitle(expression(ARMA(1, 1) ~ ~ ~ phi[1] == 0.9 * ~ ~ theta[2] == -0.4))

grid.arrange(p1, p2, ncol = 2)

### Compare with Exhibit 7.4 (p.545) and Exhibit 7.5 (p.549)               ###                      
### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###