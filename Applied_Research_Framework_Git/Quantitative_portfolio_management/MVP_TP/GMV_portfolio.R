############################################################################

# Youssef LOURAOUI

# ESSEC Business School

# January 19, 2024

# Illustration of the Global Minimum Variance (GMV) portfolio with RStudio

###########################################################################


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

# Define the stock tickers you want to fetch data for
stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "PYPL", "CMCSA", "ADBE", "ASML")

# Fetch historical stock data from Yahoo Finance
start_date <- "2019-01-01"  # Replace with your desired start date
end_date <- "2023-08-31"    # Replace with your desired end date
stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)

# Extract adjusted closing prices (which are typically used for returns)
stock_returns <- lapply(stock_tickers, function(ticker) {
  to.monthly(Ad(get(ticker)), indexAt = 'lastof', OHLC = FALSE)
})

# Combine stock returns into a single data frame
fin_prices <- do.call(cbind, stock_returns)

# Rename columns to match your analysis
colnames(fin_prices) <- stock_tickers

# Eliminated missing data in time series
fin_return <- na.omit(fin_prices)  # Remove rows with missing values

#Computation of arythmetic returns
arith_fin_returns = diff(fin_return)/lag(fin_return)
head(arith_fin_returns, n=3)
arith_fin_returns <- arith_fin_returns[-1, ]

#Checking for the data structure
plot_intro(arith_fin_returns)

# Calculate Cumulative Returns
cumulative_returns <- cumprod(1 + arith_fin_returns)

# Plot Cumulative Return Performance

autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of the investment universe") +
  ylab("Cumulative Return") +
  xlab("Year")

#Covariance matrix of the assets selected in the investment universe
cov_matrix <- cov(arith_fin_returns)

#Correlation matrix of the assets selected in the investment universe
corrplot(cor(arith_fin_returns), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

#Statistical analysis of the data
summary(arith_fin_returns)

#Definition of parameters
n <- ncol(arith_fin_returns)
T <- nrow(arith_fin_returns)
e <- rep(1, n)
perio <- 12

# Compute Sigma (unbiased covariance matrix)
Sigma <- cov(arith_fin_returns) * (T - 1) / (T - n - 2) * perio
lambda <- 1e-4  # Small regularization parameter
Sigma_reg <- Sigma + diag(lambda, ncol(Sigma))

# Define the problem for quadprog
Dmat <- 2 * Sigma_reg  # Quadratic programming expects 0.5 * x' * D * x, hence 2 * Sigma
dvec <- rep(0, n)  # Zero because we want to minimize variance without regard for expected returns

# Constraints: weights sum to 1 and are non-negative
Amat <- cbind(1, diag(n))
bvec <- c(1, rep(0, n))

# Solve the problem using quadprog
qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)

# Extract the weights for the GMV portfolio
omega_gmv <- qp$solution

# Print the weights
print(omega_gmv)

# Plot of the GMV portfolio with non-negative weights
barplot(omega_gmv, col = 'steelblue', names.arg = stock_tickers, ylim = c(0, max(omega_gmv)))
title(main = "GMV portfolio weights", ylab = "Weight")





