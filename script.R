# --- Packages & imports ---

library(tseries)
library(readr)
library(tsibble)
library(lubridate)
library(ggplot2)
library(forecast)
library(patchwork)
library(dplyr)
library(lmtest)
library(zoo)

# Processing data
raw <- read_delim(
  "data.csv",
  delim = ";",
  col_names = FALSE,
  show_col_types = FALSE
)
raw_month <- raw[ grepl("^\\d{4}-\\d{2}$", raw$X1) , ]
min_date <- ymd(paste0(tail(raw_month$X1, 1), "-01")) # 1990-01-01
y <- ts(
  rev(as.numeric(raw_month$X2)),
  start = c(year(min_date), month(min_date)),
  frequency = 12
)

plot(y)

# Testing stationarity
adf.test(y)
pp.test(y)
kpss.test(y)

# Testing stationarity on first-order differenced series
y_diff <- diff(y)
plot(y_diff)
adf.test(y_diff)
pp.test(y_diff)
kpss.test(y_diff)

# Plot autocorrelation and partial autocorrelation of the differenced series
acf(zoo(coredata(y_diff)), plot = TRUE, main = 'ACF of the differenced series')
pacf(zoo(coredata(y_diff)), plot = TRUE, main = 'PACF of the differenced series')

# AIC and BIC minimization
max_p <- 7
max_q <- 13
results <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric())
for (p in 0:max_p) {
  for (q in 0:max_q) {
    cat("Fitting ARMA(", p, ",", q, ")...\n")
    try({
      model <- Arima(y_diff, order = c(p, 0, q), include.mean = TRUE)
      results <- rbind(results, data.frame(
        p = p,
        q = q,
        AIC = AIC(model),
        BIC = BIC(model)
      ))
    }, silent = TRUE)
  }
}
best_aic <- results[which.min(results$AIC), ]
best_bic <- results[which.min(results$BIC), ]
print("Best model according to AIC:")
print(best_aic) # ---> ARMA(4, 4)
print("Best model according to BIC:")
print(best_bic) # ---> ARMA(1, 1)

# Significance of the coefficients in ARMA(4, 4) and ARMA(1, 1)
fit_aic <- Arima(y_diff, order = c(4, 0, 4))
fit_aic
coeftest(fit_aic)
fit_bic <- Arima(y_diff, order = c(1, 0, 1))
fit_bic
coeftest(fit_bic)

# Uncorrelation test on the residuals of ARMA(1, 1)
residuals_fit <- residuals(fit_bic)
plot(residuals_fit)
Box.test(residuals_fit, lag = 24, type = "Ljung-Box", fitdf = 2)