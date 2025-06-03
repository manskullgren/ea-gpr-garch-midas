# ea-gpr-garch-midas
Replication code for the thesis: "How Does Geopolitical Risk Influence FX Market Volatility?

if (!require("readxl")) install.packages("readxl")
if (!require("rugarch")) install.packages("rugarch")
if (!require("zoo")) install.packages("zoo")
if (!require("xts")) install.packages("xts")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(readxl)
library(rugarch)
library(zoo)
library(xts)
library(ggplot2)
library(dplyr)

setwd("/Users/XXX/Downloads/XXX")

fx_data <- read_excel("FX R Studio.xlsx")
gpr_data <- read_excel("GPR R Studio.xlsx")

fx_data$Date <- as.Date(as.character(fx_data$Date), format = "%Y-%m-%d")
fx_data <- fx_data %>% arrange(Date)

gpr_data$Date <- as.Date(as.character(gpr_data$Date), format = "%Y-%m-%d")
gpr_data <- gpr_data %>% arrange(Date)

run_garch_midas <- function(pair) {
  cat("Processing pair:", pair, "\n")
  
  fx_rate <- as.numeric(as.character(fx_data[[pair]]))
  fx_data_pair <- fx_data[!is.na(fx_rate), ]
  fx_rate <- as.numeric(as.character(fx_data_pair[[pair]]))
  fx_returns <- diff(log(fx_rate)) * 100
  fx_returns <- na.omit(fx_returns)
  fx_returns_xts <- xts(fx_returns, order.by = fx_data_pair$Date[-1])
  
  gpr_col <- paste0("GPR_", pair)
  gpr_series <- as.numeric(as.character(gpr_data[[gpr_col]]))
  gpr_data_pair <- gpr_data[!is.na(gpr_series), ]
  gpr_series <- as.numeric(as.character(gpr_data_pair[[gpr_col]]))
  gpr_xts <- xts(gpr_series, order.by = gpr_data_pair$Date)
  
  daily_index <- index(fx_returns_xts)
  gpr_daily <- merge(xts(order.by = daily_index), gpr_xts, all = TRUE)
  gpr_daily <- na.locf(gpr_daily)
  
  spec <- ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 1),
      external.regressors = as.matrix(gpr_daily)
    ),
    mean.model = list(
      armaOrder = c(1, 0)
    ),
    distribution.model = "norm"
  )
  
  fit <- ugarchfit(spec = spec, data = fx_returns_xts, solver = "hybrid")
  cat("Summary for", pair, ":\n")
  print(summary(fit))
  
  vol_series <- xts(fit@fit$sigma^2, order.by = index(fx_returns_xts))
  monthly_vol <- apply.monthly(vol_series, mean)
  
  gpr_xts_adj <- gpr_xts
  index(gpr_xts_adj) <- as.Date(as.yearmon(index(gpr_xts_adj)), frac = 1)
  vol_gpr <- merge(monthly_vol, gpr_xts_adj, join = "inner")
  colnames(vol_gpr) <- c("FX_Volatility", "GPR")
  vol_gpr <- na.omit(vol_gpr)
  
  gpr_numeric <- coredata(vol_gpr$GPR)
  vol_numeric <- coredata(vol_gpr$FX_Volatility)
  
  plot(gpr_numeric, vol_numeric,
       xlab = paste(pair, "GPR"),
       ylab = paste(pair, "FX Volatility (Monthly Avg)"),
       main = paste("Relationship between", pair, "GPR and FX Volatility"))
  abline(lm(vol_numeric ~ gpr_numeric), col = "red")
  
  plot.zoo(vol_gpr, plot.type = "single", col = c("black", "red"),
           ylab = "Value", xlab = "Time", main = paste("Time Series of", pair, "FX Volatility and GPR"))
  legend("topright", legend = c("FX Volatility", "GPR"), col = c("black", "red"), lty = 1)
  
  return(fit)
}

pairs <- c("EUR/USD", "EUR/JPY", "EUR/CHF", "EUR/GBP", "EUR/NOK", 
           "EUR/AUD", "EUR/CAD", "EUR/HKD", "EUR/SEK", "EUR/KOR")

fits <- lapply(pairs, run_garch_midas)
names(fits) <- pairs

for(pair in names(fits)){
  cat("====================================\n")
  cat("Summary for", pair, ":\n")
  print(summary(fits[[pair]]))
  
  cat("\nCoefficients for", pair, ":\n")
  print(coef(fits[[pair]]))
  
  cat("\nLog-likelihood for", pair, ":\n")
  print(likelihood(fits[[pair]]))
  
  cat("====================================\n\n")
}

params <- sapply(fits, function(fit) coef(fit))
params_df <- as.data.frame(t(params))
print(params_df)

lapply(fits, function(fit) fit@fit$matcoef)
