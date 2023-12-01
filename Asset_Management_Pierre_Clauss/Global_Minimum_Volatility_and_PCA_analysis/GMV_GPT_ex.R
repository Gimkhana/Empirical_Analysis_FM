library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)

# Define the stock tickers you want to fetch data for
stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN")

# Fetch historical stock data from Yahoo Finance
start_date <- "2020-01-01"  # Replace with your desired start date
end_date <- "2022-12-31"    # Replace with your desired end date
stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)

# Extract adjusted closing prices (which are typically used for returns)
stock_returns <- lapply(stock_tickers, function(ticker) {
  Ad(get(ticker))
})

# Combine stock returns into a single data frame
fin_return <- do.call(cbind, stock_returns)

# Rename columns to match your analysis
colnames(fin_return) <- stock_tickers

# Explore the data using DataExplorer
plot_intro(fin_return)

# Statistical data analysis of each column (sector)
summary(fin_return)

# Advanced analysis of the returns for each sector
plot_density(fin_return)
plot_qq(fin_return)

# Calculate the covariance matrix
cov_matrix <- cov(fin_return)

# Modelling part
n <- ncol(fin_return)
T <- nrow(fin_return)
e <- rep(1, n)
perio <- 12

# Computation of sigma
Sigma <- cov_matrix * (T - 1) / (T - n - 2) * perio

# Computation of C
C <- t(e) %*% solve(Sigma) %*% e

# Anticipated volatility
sigmag <- sqrt(1 / C)

# Computation of optimal weights "w"
omega <- 1 / as.numeric(C) * solve(Sigma) %*% e

# Plot GMV unbiased portfolio
barplot(as.numeric(omega), col = 'black')