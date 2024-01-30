##################################################################

# Youssef LOURAOUI

# ESSEC Business School

# January 19, 2024

# Illustration of the Tangency portfolio with RStudio

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

#We implement the parameters of the model in order to compute the Tangency Portfolio
n <- ncol(arith_fin_returns)
T <- nrow(arith_fin_returns)
e <- rep(1, n)
perio <- 12
rf <- 0

# Weights calculation using quadratic programming
library(quadprog)

# Define the problem
Dmat <- 2 * Sigma  # Quadratic programming expects 0.5 * x' * D * x, hence 2 * Sigma
dvec <- -mu  # Negative because 'quadprog' minimizes and we want to maximize returns

# Constraints: weights sum to 1 and are non-negative
Amat <- cbind(1, diag(n))
bvec <- c(1, rep(0, n))

# Solve the problem
qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)

# Extract the weights
tangency_weights <- qp$solution

# Print the weights
print(tangency_weights)

# Calculate expected portfolio return using the tangency weights
tangency_return <- sum(mu * tangency_weights)

# Plot of Tangency portfolio with non-negative weights
barplot(
  tangency_weights,
  col = 'steelblue',
  names.arg = barnames,
  ylim = c(0, 1)  # Change based on the range of your weights
)

# Add titles and labels
title(main = "Tangency portfolio weights", ylab = "Weight")
