# Import necessary libraries
library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)
library(xts)  # Load the xts library for working with time series data

# Define the stock tickers you want to fetch data for
stock_tickers <- c("SPY", "BNDX", "VTI", "DBC", "GLD")

# Fetch historical stock data from Yahoo Finance
start_date <- "2019-01-01"  # Replace with your desired start date
end_date <- "2023-08-31"    # Replace with your desired end date

# Use getSymbols to fetch data for all stock tickers
getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)

# Calculate daily returns
daily_returns <- lapply(stock_tickers, function(ticker) {
  dailyReturn(Cl(get(ticker)))
})

# Combine daily returns into a single data frame
daily_returns_df <- do.call(cbind, daily_returns)

# Calculate monthly returns from daily returns
monthly_returns <- to.monthly(daily_returns_df)

# Data exploration for the monthly return time series
plot_intro(monthly_returns)

# Calculate Cumulative Returns
cumulative_returns <- cumprod(1 + monthly_returns)

# Plot Cumulative Return Performance
library(ggplot2)
autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of the investment universe") +
  ylab("Cumulative Return") +
  xlab("Year")

# Statistical summary of returns for each asset class

summary(monthly_returns)

# Advanced statistical analysis for the dataset

plot_density(monthly_returns)
plot_qq(monthly_returns)

# Correlation analysis

library(corrplot)
corrplot( cor(monthly_returns), type='lower', method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)


#2. Modelling of the portfolio

library(scales)

#Shrinking the dataset into two different subsamples

# Specify the number of rows for the first subsample
num_rows_subsample1 <- 20

# Calculate the total number of rows in your dataset
total_rows <- nrow(monthly_returns)

# Create the first subsample
subsample1 <- monthly_returns[1:num_rows_subsample1, ]
plot_intro(subsample1)

# Create the second subsample by excluding the rows in the first subsample
subsample2 <- monthly_returns[(num_rows_subsample1 + 1):total_rows, ]
plot_intro(subsample2)

#Computation parameters

n <- ncol(subsample1)
T <- nrow(subsample1)
e <- rep(1, n)
perio <- 12
rf <- 0

mu <- colMeans(subsample1) * perio - rf
Sigma <- cov(subsample1) * (T - 1) / (T - n - 2) * perio
A <- t(e) %*% solve(Sigma) %*% mu
omega <- 1 / as.numeric(A) * solve(Sigma) %*% mu

returns = mu %*% omega

# Plot of Tangency portfolio

barnames <- c("SPY", "BNDX", "VTI", "DBC", "GLD")
barplot(
  as.numeric(omega),
  col = 'black',
  names.arg = barnames,
  ylim = c(-0.5, 1.5)
)


# Implementing Black-Litterman approach

# Weighting of the views 

Q <- numeric(5)
Q[1] <- 0.02 # positive view for US equity index (SPY)
Q[2] <- 0.04 # positive view for Global bond index (BNDX)
Q[3] <- 0.06 # negative view for global equity index (VTI)
Q[4] <- 0.08 # negative view for commodity index (DBC)
Q[5] <- 0.1 # negative view for the gold index (GLD)
tau <- 0.95


# Mixed estimation of returns
Omega <- diag(diag(Sigma), n, n)
mu_mixed <-
  solve(solve(tau * Sigma) + solve(Omega)) %*% (solve(tau * Sigma) %*% mu +
                                                  solve(Omega) %*% Q)

# Tactical allocation with views directly
A_Q <- t(e) %*% solve(Sigma) %*% Q
omega_Q <- 1 / as.numeric(A_Q) * solve(Sigma) %*% Q
# barplot(as.numeric(omega_Q), col = 'black', names.arg = barnames, ylim = c(0,1))

# Tactical allocation with mixed estimation
A_mixed <- t(e) %*% solve(Sigma) %*% mu_mixed
omega_mixed <- 1 / as.numeric(A_mixed) * solve(Sigma) %*% mu_mixed
barplot(as.numeric(omega_mixed), col = 'black', names.arg = barnames, ylim = c(0,1))

