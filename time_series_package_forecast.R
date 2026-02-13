### Package forecast ###
### https://cran.r-project.org/web/packages/forecast/refman/forecast.html ###
library(forecast)
### Data                                                                  ### 
head(wineind)
### Correlaton                                                            ###
Acf(wineind)
Pacf(wineind)
taperedacf(wineind, nsim = 50)
taperedpacf(wineind, nsim = 50)
library(ggplot2)
### Time series data: WWWusage  ###
str(WWWusage)
WWWusage |>
  Arima(order = c(3, 1, 0)) |>
  forecast(h = 20) |>
  autoplot()
### Time series data: AirPassengers ###
str(AirPassengers) 
# Fit model to first few years of AirPassengers data
air.model <- Arima(
  window(AirPassengers, end = 1956 + 11 / 12),
  order = c(0, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12),
  lambda = 0
)
plot(forecast(air.model, h = 48))
lines(AirPassengers)
# Apply fitted model to later data
air.model2 <- Arima(window(AirPassengers, start = 1957), model = air.model)
# Forecast accuracy measures on the log scale.
# in-sample one-step forecasts.
accuracy(air.model)
# out-of-sample one-step forecasts.
accuracy(air.model2)
# out-of-sample multi-step forecasts
accuracy(
  forecast(air.model, h = 48, lambda = NULL),
  log(window(AirPassengers, start = 1957))
)

### Box Cox Transformation ###
str(lynx)
lambda <- BoxCox.lambda(lynx)
lynx.fit <- ar(BoxCox(lynx, lambda))
plot(forecast(lynx.fit, h = 20, lambda = lambda))


fit <- auto.arima(WWWusage)
plot(forecast(fit, h = 20))

library(ggplot2)
ggAcf(wineind)
wineind |> Acf(plot = FALSE) |> autoplot()

wineind |> taperedacf(plot = FALSE) |> autoplot()
ggtaperedacf(wineind)
ggtaperedpacf(wineind)

ggCcf(mdeaths, fdeaths)

library(ggplot2)
co2 |>
  decompose() |>
  autoplot()
nottem |>
  stl(s.window = "periodic") |>
  autoplot()

library(seasonal)
seas(USAccDeaths) |> autoplot()

library(ggplot2)

lungDeaths <- cbind(mdeaths, fdeaths)
fit <- tslm(lungDeaths ~ trend + season)
fcast <- forecast(fit, h = 10)
plot(fcast)
autoplot(fcast)

carPower <- as.matrix(mtcars[, c("qsec", "hp")])
carmpg <- mtcars[, "mpg"]
fit <- lm(carPower ~ carmpg)
fcast <- forecast(fit, newdata = data.frame(carmpg = 30))
plot(fcast, xlab = "Year")
autoplot(fcast, xlab = rep("Year", 2))

### Check residuals ###
fit <- ets(WWWusage)
checkresiduals(fit)

findfrequency(USAccDeaths) # Monthly data
findfrequency(taylor) # Half-hourly data
findfrequency(lynx)


fit <- ets(WWWusage)
plot(WWWusage)
lines(fitted(fit), col = "red")
lines(fitted(fit, h = 2), col = "green")
lines(fitted(fit, h = 3), col = "blue")
legend("topleft", legend = paste("h =", 1:3), col = 2:4, lty = 1)