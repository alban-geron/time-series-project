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


# Joint confidence region for Y(t+1) and Y(t+2)

fc <- forecast(fit_bic, h = 2)
f1 <- as.numeric(fc$mean[1])
f2 <- as.numeric(fc$mean[2])

sigma2 <- fit_bic$sigma2
phi    <- as.numeric(fit_bic$model$phi)
theta  <- as.numeric(fit_bic$model$theta)

# Compute the first MA weight for the stationary ARMA(1,1) on y_diff
psi1 <- phi + theta

# Variances and covariance of the 1-step and 2-step forecast errors for ΔY
var1  <- sigma2 * (1 + theta^2)        # Var(e_{t+1})
var2  <- sigma2 * (1 + psi1^2)         # Var(e_{t+2})
cov12 <- sigma2 * psi1                 # Cov(e_{t+1}, e_{t+2})

Sigma_d <- matrix(c(var1, cov12,
                    cov12, var2),
                  nrow = 2, byrow = TRUE)

# Convert forecasts of ΔY into forecasts of Y
y_last <- as.numeric(tail(y, 1))       # last observed Y_t
mu_y1  <- y_last + f1                  # E[Y_{t+1}]
mu_y2  <- y_last + f1 + f2             # E[Y_{t+2}]
center <- c(mu_y1, mu_y2)

# Transformation matrix
A <- matrix(c(1, 0,
              1, 1),
            nrow = 2, byrow = TRUE)

# Covariance matrix for the joint forecast error of (Y_{t+1}, Y_{t+2})
Sigma_y <- A %*% Sigma_d %*% t(A)

# Compute the points on the 95% confidence ellipse
chisq_val <- qchisq(0.95, df = 2)
eig <- eigen(Sigma_y)

# Scale eigenvectors by sqrt(eigenvalues * khi2(0.95))
transformation_matrix <- eig$vectors %*% diag(sqrt(eig$values * chisq_val))

theta_seq <- seq(0, 2 * pi, length.out = 200)
ellipse_coords <- t(sapply(theta_seq, function(t) {
  center + transformation_matrix %*% c(cos(t), sin(t))
}))

ellipse_df <- data.frame(
  Y_tp1 = ellipse_coords[, 1],
  Y_tp2 = ellipse_coords[, 2]
)

# Plot the 95% confidence ellipse
ggplot(ellipse_df, aes(x = Y_tp1, y = Y_tp2)) +
  geom_path(color = "steelblue", size = 1) +
  geom_point(aes(x = center[1], y = center[2]), color = "red", size = 2) +
  labs(
    x = expression(Y[t + 1]),
    y = expression(Y[t + 2]),
    title = ""
  ) +
  theme_minimal()
