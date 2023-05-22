library(rlang)
library(tidyquant)
library(tidyverse)
library(plotly)
library(timetk)

# Define the market ticker symbol (Vanguard Energy Index)
market_ticker <- 'VDE'

tick <- c('XOM', 'CVX', 'BP', 'FCEL', 'DVN')

# Fetch price data for assets and market portfolio separately
asset_data <- tq_get(tick,
                     from = '2018-01-01',
                     to = '2022-05-31',
                     get = 'stock.prices')

market_data <- tq_get(market_ticker,
                      from = '2018-01-01',
                      to = '2022-05-31',
                      get = 'stock.prices')

# Calculate daily log returns
log_ret_tidy_assets <- asset_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

log_ret_tidy_market <- market_data %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log') %>%
  mutate(symbol = market_ticker)  # Add the symbol column manually

# Convert to xts format, was in tidy format before
log_ret_xts_assets <- log_ret_tidy_assets %>%
  spread(symbol, value = ret) %>%
  tk_xts()

log_ret_xts_market <- log_ret_tidy_market %>%
  spread(symbol, value = ret) %>%
  tk_xts()

# Calculate the annualized returns (Colmeans calculate the mean - 252 financial days in a year)
ann_ret <- colMeans(log_ret_xts_assets) * 252

# Calculate the beta for each asset
beta_values <- sapply(1:ncol(log_ret_xts_assets), function(i) {
  cov(log_ret_xts_assets[,i], log_ret_xts_market[,1]) / var(log_ret_xts_market[,1])
})

names(beta_values) <- colnames(log_ret_xts_assets) # name the beta values

# Add risk-free rate
rf <- 0.02

# Calculate the expected market return
E_Rm <- mean(colMeans(log_ret_xts_market) * 252)

# Create a dataframe for plotting
asset_df <- data.frame(
  Ticker = names(beta_values),
  Beta = beta_values,
  Return = ann_ret
)

# Calculate the Security Market Line (SML) function
sml <- function(beta) rf + beta * (E_Rm - rf)


# Create the plot
ggplot(asset_df, aes(x = Beta, y = Return)) +
  geom_point() +
  geom_text(aes(label = Ticker), vjust = -1, hjust = -0.5) +  # Add labels to data points
  stat_function(fun = sml, color = "red", linetype = "dashed") +  # Add SML line
  labs(x = "Beta", y = "Annualized Returns") +
  xlim(-1, 2) +  
  ylim(-0.2, 0.4) +
  theme_minimal()
