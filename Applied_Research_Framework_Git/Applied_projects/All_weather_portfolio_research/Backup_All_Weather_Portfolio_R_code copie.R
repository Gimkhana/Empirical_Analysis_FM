library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)
library(tinytex)

# Define the stock tickers you want to fetch data for
stock_tickers <- c("VTI", "EMGF", "IEF", "DBC", "GLD")

# Fetch historical stock data from Yahoo Finance
start_date <- "2019-01-01"  # Replace with your desired start date
end_date <- "2023-08-31"    # Replace with your desired end date

# Use getSymbols to fetch data for all stock tickers
stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)

# Extract adjusted closing prices (which are typically used for returns)
stock_returns <- lapply(stock_tickers, function(ticker) {
  to.monthly(Ad(get(ticker)), indexAt = 'lastof', OHLC = FALSE)
})

# Combine stock returns into a single data frame
fin_prices <- do.call(cbind, stock_returns)

# Eliminate missing data in time series
fin_return <- na.omit(fin_prices)  # Remove rows with missing values

# Computation of returns
arith_fin_returns = diff(fin_return)/lag(fin_return)
head(arith_fin_returns, n=3)
arith_fin_returns <- arith_fin_returns[-1, ]

# Data exploration for the return time series. In order to ensure that the data is properly imported, with non missing values and proper data cleaning procedure, we can implement this line of code in order to learn more about data.
plot_intro(arith_fin_returns)

# Calculate Cumulative Returns
cumulative_returns <- cumprod(1 + arith_fin_returns)

# Plot Cumulative Return Performance
library(ggplot2)
autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of the investment universe") +
  ylab("Cumulative Return") +
  xlab("Year")

# Statistical summary of returns for each asset class
summary(arith_fin_returns)

# Advanced statistical analysis for the dataset

plot_density(arith_fin_returns)
plot_qq(arith_fin_returns)

# Correlation analysis
corrplot(cor(arith_fin_returns), type='lower', method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

##Unbiased Global Minimum Variance (GMV) portfolio

#Sampling of the data
num_rows_subsample1 <- 20
total_rows <- nrow(arith_fin_returns)

#The first sub-sample will cover the first 20 trading month covered in the data set.
subsample1 <- arith_fin_returns[1:num_rows_subsample1, ]
plot_intro(subsample1)

#The second sub-sample will cover the rest of the data set, covering the equivalent of 36 trading month.
subsample2 <- arith_fin_returns[(num_rows_subsample1 + 1):total_rows, ]
plot_intro(subsample2)

#Definition of parameters
n <- ncol(subsample1)
T <- nrow(subsample1)
e <- rep(1, n)
perio <- 12

#Compute Sigma (unbiased covariance matrix)
#We proceed with a regularization procedure: In order to compute an inverse, we can add a small positive value to the diagonal of the matrix to make it invertible. This technique is known as "Tikhonov regularization" or "ridge regression".

Sigma <- cov(subsample1) * (T - 1) / (T - n - 2) * perio
lambda <- 1e-4  # Small regularization parameter
Sigma_reg <- Sigma + diag(lambda, ncol(Sigma))
C <- t(e) %*% solve(Sigma_reg) %*% e

#Anticipated volatility can be computed
sigmag <- sqrt(1 / C)

#After computing the parameters required to implement the unbiased GMV portfolio, we can compute omega, representing the weightings of the portfolio as follows.
omega_gmv <- 1 / as.numeric(C) * solve(Sigma_reg) %*% e

#Plot GMV portfolio weights
barplot(as.numeric(omega_gmv), col = 'black', ylim = c(-0.5, 1))

#PCA one factor

# valp <- eigen(Sigma_reg)$values
# vecp <- eigen(Sigma_reg)$vectors
# vp1 <- vecp[, 1]
# lambda1 <- valp[1]
# varepsilon1 <- diag(Sigma_reg) - vp1 ^ 2 * lambda1
# Sigma_epsilon1 <- diag(varepsilon1, n, n)
# Sigma1 <- (lambda1 * vp1 %*% t(vp1) + Sigma_epsilon1)
# C1 <- t(e) %*% solve(Sigma1) %*% e
# sigmag1 <- sqrt(1 / C1)
# omega1 <- 1 / as.numeric(C1) * solve(Sigma1) %*% e

#plotting the weights for the one factor model
# barplot(as.numeric(omega1), col = 'black')

##PCA portfolio analysis

#PCA for three factor 
valp <- eigen(Sigma_reg)$values
vecp <- eigen(Sigma_reg)$vectors

vp3 <- cbind(vecp[, 1], vecp[, 2], vecp[, 3])
lambda3 <- diag(c(valp[1], valp[2], valp[3]), 3, 3)
varepsilon3 <- diag(Sigma_reg) - vp3 ^ 2 %*% diag(lambda3)
Sigma_epsilon3 <- diag(as.numeric(varepsilon3), n, n)
Sigma3 <- (vp3 %*% lambda3 %*% t(vp3) + Sigma_epsilon3)
C3 <- t(e) %*% solve(Sigma3) %*% e
sigmag3 <- sqrt(1 / C3)
omega3 <- 1 / as.numeric(C3) * solve(Sigma3) %*% e

#plotting the weights for the three factor model
barplot(as.numeric(omega3), col = 'black')


## Implementing the Tangency portfolio (TP)

# Specify the number of rows for the first subsample
num_rows_subsample1 <- 20

# Calculate the total number of rows in your dataset
total_rows <- nrow(arith_fin_returns)

# Create the first subsample
subsample1 <- arith_fin_returns[1:num_rows_subsample1, ]
plot_intro(subsample1)

# Create the second subsample by excluding the rows in the first subsample
subsample2 <- arith_fin_returns[(num_rows_subsample1 + 1):total_rows, ]
plot_intro(subsample2)

#Parameters
n <- ncol(subsample1)
T <- nrow(subsample1)
e <- rep(1, n)
perio <- 12
rf <- 0

#Parameters estimation
mu <- colMeans(subsample1) * perio - rf
Sigma <- cov(subsample1) * (T - 1) / (T - n - 2) * perio
A <- t(e) %*% solve(Sigma) %*% mu
omega <- 1 / as.numeric(A) * solve(Sigma) %*% mu
returns = mu %*% omega

# Plot of Tangency portfolio
barnames <- c("VTI", "EMGF", "IEF", "DBC", "GLD")
barplot(
  as.numeric(omega),
  col = 'Black',
  names.arg = barnames,
  ylim = c(-0.5, 1)
)

#Views vector
Q <- numeric(5)
Q[1] <- 0.05 # Strongly overweight for International equities (TICKER: VTI)
Q[2] <- 0.01 # Relatively overweight for Emerging Markets (TICKER: EMGF)
Q[3] <- 0.05 # Strongly overweight for US short term maturity bond (TICKER: IEF)
Q[4] <- 0.00 # Neutral for Commodities (TICKER: DBC)
Q[5] <- -0.02 # Relatively underweight for Gold (TICKER: GLD)
tau <- 0.95

#Mixed estimation of returns
Omega <- diag(diag(Sigma), n, n)
mu_mixed <-
  solve(solve(tau * Sigma) + solve(Omega)) %*% (solve(tau * Sigma) %*% mu +
                                                  solve(Omega) %*% Q)

#Tactical allocation with views directly
A_Q <- t(e) %*% solve(Sigma) %*% Q
omega_Q <- 1 / as.numeric(A_Q) * solve(Sigma) %*% Q
#barplot(as.numeric(omega_Q), col = 'black', names.arg = barnames, ylim = c(-0.5,1.5))

#Tactical allocation with mixed estimation
A_mixed <- t(e) %*% solve(Sigma) %*% mu_mixed
omega_mixed <- 1 / as.numeric(A_mixed) * solve(Sigma) %*% mu_mixed

#Plot for the mixed view allocation
barplot(as.numeric(omega_mixed), col = 'black', names.arg = barnames, ylim = c(-1,1))