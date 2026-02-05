### Source: https://www.geo.fu-berlin.de/en/v/soga-r/Advances-statistics/Time-series-analysis/ARIMA-modelling-in-R/index.html ###
load(url("https://userpage.fu-berlin.de/soga/data/r-data/Earth_Surface_Temperature.RData"))
str(t_global)
library(xts)
t_global_year <- apply.yearly(t_global, mean)

temp_global <- t_global_year["1850/2000", "Monthly_Anomaly_Global"]
temp_global_test <- t_global_year["2001/2016", "Monthly_Anomaly_Global"]
library(xts)
t_global <- apply.yearly(t_global, mean)
### Plotting for identifying trends and/or outliers ###
plot.zoo(cbind(
  temp_global,
  temp_global_test
),
plot.type = "single",
col = c("black", "gray"),
main = "Earth Surface Temperature Anomalies",
ylab = "", xlab = ""
)
legend("topleft",
  legend = c(
    "training set 1850 - 2000",
    "test set 2001 - 2016"
  ),
  col = c("black", "gray"),
  lty = 1, cex = 0.65
)
### Bos Cox Transformation   ####
library(forecast)
lambda <- BoxCox.lambda(temp_global)
lambda

# transformed time series
temp_global_BC <- BoxCox(temp_global, lambda)

plot.zoo(cbind(
  temp_global,
  temp_global_BC
),
col = c("black", "gray"),
main = "Original vs. Box-Cox transformed time series",
ylab = "", xlab = ""
)

### Stationary check  ###
library(tseries)
kpss.test(temp_global)
temp_global_diff1 <- diff(temp_global)
kpss.test(temp_global_diff1)

plot(temp_global_diff1)
Acf(temp_global_diff1,
  main = "ACF for Differenced Series"
)
library(ggplot2)
library(ggfortify)
library(gridExtra)
### ACF and PACF ###
## ACF
p1 <- autoplot(Acf(temp_global_diff1,
  plot = FALSE,
  lag.max = 15
)) + ggtitle("ACF") + xlim(1, 15) + ylim(-0.4, 0.4)

## PACF
p2 <- autoplot(Acf(temp_global_diff1,
  plot = FALSE,
  lag.max = 15,
  type = "partial"
)) + ggtitle("PACF")

grid.arrange(p1, p2, ncol = 2)

### Model selection   ###
fit <- Arima(temp_global, order = c(3, 1, 0))
summary(fit)
print(paste("ARIMA(3, 1, 0) - AICc: ", round(fit$aicc, 2)))
fit_test <- Arima(temp_global, order = c(3, 1, 1))
print(paste("ARIMA(3, 1, 1) - AICc: ", round(fit_test$aicc, 2)))
fit_test <- Arima(temp_global, order = c(3, 1, 2))
print(paste("ARIMA(3, 1, 2) - AICc: ", round(fit_test$aicc, 2)))
fit_test <- Arima(temp_global, order = c(2, 1, 2))
print(paste("ARIMA(2, 1, 2) - AICc: ", round(fit_test$aicc, 2)))

fit_auto <- auto.arima(temp_global, seasonal = FALSE)
summary(fit_auto)

### Check residuals  ###
Acf(residuals(fit_auto))
### Autocorrelation Test ###
B_test <- Box.test(residuals(fit_auto),
  lag = 10,
  fitdf = 6,
  type = "Ljung"
)
B_test
### Forecast ###
# pot forecast
temp_forecast <- forecast(fit_auto, h = 16)
plot(temp_forecast)

# plot test time series of the period 2001 - 2016
lines(ts(coredata(temp_global_test),
  start = start(temp_forecast$mean)[1],
  frequency = 1
), col = "magenta")

