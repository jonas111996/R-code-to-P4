library(rlang)
library(tidyquant)
library(tidyverse)
library(plotly)
library(timetk)

# Define the market ticker symbol (Vanguard Energy Index)
market_ticker <- 'VDE'

# Define other tickers
tick <- c('XOM', 'CVX', 'BP', 'FCEL', 'DVN')

# Get the price data for all tickers including the market ticker
all_tickers <- c(tick, market_ticker)

price_data <- tq_get(all_tickers,
                     from = '2018-01-01',
                     to = '2023-05-08',
                     get = 'stock.prices')

log_ret_tidy <- price_data %>%
  filter(date <= as.Date('2022-05-08')) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

market_ret_xts <- log_ret_xts[, market_ticker]
assets_ret_xts <- log_ret_xts[, tick]

rf_data <- data.frame(date = time(assets_ret_xts),
                      rf = rep(0.02/252, length(time(assets_ret_xts))))
rf_xts <- tk_xts(rf_data, date_col = date)

assets_ret_xts <- merge(assets_ret_xts, rf_xts)

mean_ret <- colMeans(assets_ret_xts)

# Tangency portfolio weights
tangency_weights <- c('BP'=0.35847850, 'FCEL'=0.03934474, 'DVN'=0.04992146, 'XOM'=0.11580919, 'CVX'=0.30003983)

# Calculate the expected return of the tangency portfolio
tangency_return <- sum(mean_ret[names(tangency_weights)] * tangency_weights)

# Annualize the expected return
tangency_return_annual <- ((tangency_return + 1) ^ 252) - 1

print(paste0("The expected annual return for the Tangency Portfolio: ", tangency_return_annual))

