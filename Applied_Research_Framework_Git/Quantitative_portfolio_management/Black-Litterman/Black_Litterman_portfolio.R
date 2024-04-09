##################################################################

# Youssef LOURAOUI

# ESSEC Business School

# January 19, 2024

# Illustration of the Black-Litterman (BL) portfolio with RStudio

##################################################################


#Script to download the necessary packages
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of required packages
packages <- c("tidyverse", "quantmod", "DataExplorer", "corrplot", "ggplot2","scales", "quadprog")

# Apply the function to each package
lapply(packages, check_and_install)

#Import the libraries for the analysis
library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)
library(ggplot2)
library(quadprog)


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

# Data exploration for the return time series
plot_intro(arith_fin_returns)

# Calculate Cumulative Returns
cumulative_returns <- cumprod(1 + arith_fin_returns)

# Plot Cumulative Return Performance

autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of the investment universe") +
  ylab("Cumulative Return") +
  xlab("Year")

# Statistical summary of returns for each asset class
summary(arith_fin_returns)

# Correlation analysis
corrplot( cor(arith_fin_returns), type='lower', method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

#Definition of parameters
n <- ncol(arith_fin_returns)
T <- nrow(arith_fin_returns)
e <- rep(1, n)
perio <- 12

# Compute Sigma (unbiased covariance matrix)
Sigma <- cov(arith_fin_returns) * (T - 1) / (T - n - 2) * perio
lambda <- 1e-4  # Small regularization parameter
Sigma_reg <- Sigma + diag(lambda, ncol(Sigma))

#The parameter sigma, representing the unbiased covariance matrix (similar to the procedure implemented when computing the GMV portfolio), can be computed using the following procedure

mu <- colMeans(arith_fin_returns) * perio - rf
Sigma <- cov(arith_fin_returns) * (T - 1) / (T - n - 2) * perio
A <- t(e) %*% solve(Sigma) %*% mu
omega <- 1 / as.numeric(A) * solve(Sigma) %*% mu

returns = mu %*% omega


# Creation of the view vector of expected returns
Q <- numeric(5)
Q[1] <- 0.05 # Strongly overweight for International equities (TICKER: VTI)
Q[2] <- 0.01 # Relatively overweight for Emerging Markets (TICKER: EMGF)
Q[3] <- 0.05 # Strongly overweight for US short term maturity bond (TICKER: IEF)
Q[4] <- 0.00 # Neutral for Commodities (TICKER: DBC)
Q[5] <- -0.02 # Relatively underweight for Gold (TICKER: GLD)
tau <- 0.95

# Mixed estimation of returns

Omega <- diag(diag(Sigma), n, n)
mu_mixed <-
  solve(solve(tau * Sigma) + solve(Omega)) %*% (solve(tau * Sigma) %*% mu +
                                                  solve(Omega) %*% Q)

# Tactical allocation with views directly

A_Q <- t(e) %*% solve(Sigma) %*% Q
omega_Q <- 1 / as.numeric(A_Q) * solve(Sigma) %*% Q
#barplot(as.numeric(omega_Q), col = 'black', names.arg = barnames, ylim = c(-0.5,1.5))

#Tactical allocation with mixed estimation
A_mixed <- t(e) %*% solve(Sigma) %*% mu_mixed
omega_mixed <- 1 / as.numeric(A_mixed) * solve(Sigma) %*% mu_mixed

# Define the problem for quadprog
Dmat <- 2 * Sigma  # Quadratic programming expects 0.5 * x' * D * x, hence 2 * Sigma
dvec <- -as.vector(mu_mixed)  # Negative because 'quadprog' minimizes and we want to maximize returns

# Constraints: weights sum to 1 and are non-negative
Amat <- cbind(1, diag(n))
bvec <- c(1, rep(0, n))

# Solve the problem using quadprog
qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)

# Extract the weights for the Black-Litterman portfolio with non-negative weights
omega_bl <- qp$solution

# Print the weights
print(omega_bl)

# Plot of the Black-Litterman portfolio with non-negative weights
barplot(omega_bl, col = 'steelblue', names.arg = stock_tickers, ylim = c(0, max(omega_bl)))
title(main = "BL portfolio weights", ylab = "Weight")

