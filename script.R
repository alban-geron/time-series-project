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