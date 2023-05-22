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
market_ret_xts <- log_ret_xts[, market_ticker]
assets_ret_xts <- log_ret_xts[, tick]

# Add a risk-free asset
rf_data <- data.frame(date = time(assets_ret_xts),
                      rf = rep(0.02/252, length(time(assets_ret_xts))))
rf_xts <- tk_xts(rf_data, date_col = date)

# Merge the risk-free asset to the log return data
assets_ret_xts <- merge(assets_ret_xts, rf_xts)

mean_ret <- colMeans(assets_ret_xts)
cov_mat <- cov(assets_ret_xts) * 252

print(round(cov_mat, 4))

num_port <- 50000
all_wts <- matrix(nrow = num_port, ncol = length(tick)+1) # add one for the risk-free asset
port_returns <- vector('numeric', length = num_port)
port_risk <- vector('numeric', length = num_port)
sharpe_ratio <- vector('numeric', length = num_port)

# Function to generate random weights with shorting
generate_weights <- function(n_assets) {
  wts <- runif(n_assets, min = -1, max = 1)
  wts <- wts / sum(abs(wts))
  return(wts)
}

for (i in seq_along(port_returns)) {
  wts <- generate_weights(length(tick)+1) # add one for the risk-free asset
  all_wts[i, ] <- wts
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1) ^ 252) - 1
  port_returns[i] <- port_ret
  
  port_sd <- sqrt(t(wts) %*% (cov_mat %*% wts))
  port_risk[i] <- port_sd
  
  sr <- (port_ret - 0.02) / port_sd # subtract risk-free rate from portfolio return
  sharpe_ratio[i] <- sr
}

portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- c(colnames(assets_ret_xts)[-ncol(assets_ret_xts)], "rf") # rename last
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

head(portfolio_values)

min_var <- portfolio_values[which.min(portfolio_values$Risk), ]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio), ]

p <- min_var %>%
  gather(XOM:DVN, key = Asset, value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset, Weights),  y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

ggplotly(p)

p <- max_sr %>%
  gather(XOM:DVN, key = Asset, value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset, Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

# Tangency Portfolio Weights
ggplotly(p)

p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk, y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk, y = Return), data = max_sr, color = 'red')

# Capital Market Line (CML)
# Risk-free rate
risk_free_rate <- 0.02

# Calculating CML
cml_df <- tibble(Risk = seq(0, 0.3, 0.001))
cml_df$Return <- risk_free_rate + cml_df$Risk * (max_sr$Return - risk_free_rate) / max_sr$Risk

# Adding CML to the plot
p <- p +
  geom_line(data = cml_df, aes(x = Risk, y = Return), color = 'blue') +
  annotate(geom = 'text', x = 0.2, y = 0.09, label = 'Capital Market Line', color = 'blue')

ggplotly(p)
