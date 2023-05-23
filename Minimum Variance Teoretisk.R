# Load libraries
library(tidyverse)
library(tidyquant)

# Define the market ticker symbol (Vanguard Energy Index)
market_ticker <- 'VDE'

# Define other tickers
tick <- c('XOM', 'CVX', 'BP', 'FCEL', 'DVN')

# Get the price data for all tickers including the market ticker
all_tickers <- c(tick, market_ticker)

price_data <- tq_get(all_tickers,
                     from = '2018-01-01',
                     to = '2022-05-31',
                     get = 'stock.prices')

log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

# Separate the market portfolio from the other assets
assets_ret_xts <- log_ret_xts[, tick]

mean_ret <- colMeans(assets_ret_xts)
cov_mat <- cov(assets_ret_xts) * 252

# calculate the inverse covariance matrix
inv_cov_mat <- solve(cov_mat)

# vector of 1s
ones <- matrix(1, nrow = ncol(assets_ret_xts), ncol = 1)

A <- t(ones) %*% inv_cov_mat %*% mean_ret
C <- t(ones) %*% inv_cov_mat %*% ones

# calculate weights for MVP
w_MVP <- (inv_cov_mat %*% ones) / C[1,1]


# Calculate return and risk for MVP
r_MVP <- t(w_MVP) %*% mean_ret
sigma_MVP <- sqrt(t(w_MVP) %*% cov_mat %*% w_MVP)

# print return and risk for MVP
print(paste0("Expected return for MVP: ", r_MVP))
print(paste0("Expected risk for MVP: ", sigma_MVP))