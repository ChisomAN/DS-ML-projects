install.packages(c("forecast", "tsutils", "ggplot2", "lmtest", "tseries"))
library(forecast)
library(tsutils)
library(ggplot2)
library(lmtest)
library(tseries)

climate <- read.csv("/Users/chizzycasa/Desktop/time series/new_train.csv")
str(climate)
summary(climate)

# Convert and clean date column
climate$date <- as.Date(climate$date, format = "%d-%m-%Y")
climate <- climate[!is.na(climate$date), ]
climate <- climate[order(climate$date), ]

# Aggregate to monthly average temperature using base R
climate$year_month <- format(climate$date, "%Y-%m")
df_monthly <- aggregate(temp ~ year_month, data = climate, FUN = mean, na.rm = TRUE)

# Convert to time series object
climate_ts <- ts(df_monthly$temp, start = c(1980, 1), frequency = 12)

plot(climate_ts, main = "Monthly Temperature Time Series", ylab = expression(Temperature~(degree*C)))

# TBATS for seasonality
tbats_fit <- tbats(climate_ts)
summary(tbats_fit)

# Central Moving Average
cma <- cmav(climate_ts)
plot(cma, type="l", col="blue", main="Central Moving Average")

decomp <- decompose(climate_ts)
autoplot(decomp)

seasplot(climate_ts)

hw_model <- HoltWinters(climate_ts)
plot(hw_model)
hw_model$alpha  # Interpret alpha

auto_fit <- auto.arima(climate_ts)
summary(auto_fit)

coeftest(auto_fit)

forecasted <- forecast(auto_fit, h = 12)
autoplot(forecasted)

accuracy(forecasted)

# AIC for ARIMA:
AIC(auto_fit)  # Lower AIC indicates better ARIMA performance

acf(climate_ts)  # Inspect autocorrelation to detect cycles beyond seasonal trend
pacf(climate_ts)  # Identify lag effects

summary(forecasted)
