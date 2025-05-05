# --- Packages & imports ---

library(tseries)
library(readr)
library(tsibble)
library(lubridate)
library(ggplot2)
library(forecast)
library(patchwork)
library(dplyr)

raw <- read_delim(
  "data.csv",
  delim = ";",
  col_names = FALSE,
  show_col_types = FALSE
)



# --- Question 2 ---

raw_month <- raw[ grepl("^\\d{4}-\\d{2}$", raw$X1) , ]
min_date <- ymd(paste0(tail(raw_month$X1, 1), "-01"))
y <- ts(
  rev(as.numeric(raw_month$X2)),
  start = c(year(min_date), month(min_date)),
  frequency = 12
)

# Test ADF
adf.test(y)

# First difference (d = 1)
y_diff  <- diff(y)

# Seasonal difference (D = 1, period 12)
y_diffS <- diff(y, lag = 12)

# Both d = 1 et D = 1
y_stab <- diff(y_diffS)

cat("\nADF after first difference: p =", adf.test(y_diff)$p.value)
cat("\nADF after seasonal difference: p =", adf.test(y_diffS)$p.value)
cat("\nADF after d = 1 & D = 1 (both diffs): p =", adf.test(y_stab)$p.value, "\n")



# --- Question 3 ---

p1 <- autoplot(y) + ggtitle("Before differencing") # --> série clairement non stationnaire !!! contrairement à ce que dit ADF
p2 <- autoplot(y_stab) + ggtitle("After differencing")
p1 + p2