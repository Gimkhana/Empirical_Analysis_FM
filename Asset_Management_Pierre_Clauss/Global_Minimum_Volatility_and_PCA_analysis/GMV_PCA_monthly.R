#Enhanced GMV and PCA portfolio 

# Import necessary libraries
library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)

# Define the stock tickers you want to fetch data for
stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "PYPL", "CMCSA", "ADBE", "ASML",
                   "CSCO", "PEP", "AVGO", "TMUS", "INTC", "TXN", "AMGN", "NFLX", "SBUX", "AMAT")

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

# Convert the xts object to a data frame
#price_as_df <- as.data.frame(fin_prices)

# Eliminated missing data in time series
fin_return <- na.omit(fin_prices)  # Remove rows with missing values

#Computation of returns
arith_fin_returns = diff(fin_return)/lag(fin_return)
head(arith_fin_returns, n=3)
arith_fin_returns <- arith_fin_returns[-1, ]

#Data exploration for the return timeseries

plot_intro(arith_fin_returns)

# Calculate Cumulative Returns
cumulative_returns <- cumprod(1 + arith_fin_returns)

# Plot Cumulative Return Performance
library(ggplot2)
autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of Stocks") +
  ylab("Cumulative Return") +
  xlab("Year")


#plotting price in time series

library(ggplot2)
autoplot(fin_return, facets = NULL) +
  ggtitle("Daily Closing Prices") +
  ylab("Closing Price Per Share") +
  xlab("Year")



#plotting return in time series

library(ggplot2)
autoplot(arith_fin_returns, facets = NULL) +
  ggtitle("Daily Closing Prices") +
  ylab("Closing Price Per Share") +
  xlab("Year")


# Calculate the covariance matrix
cov_matrix <- cov(arith_fin_returns)

#Compute the correlation matrix of return to capture the relationships between stocks 
corrplot(cor(arith_fin_returns), type='lower', title = "Correlation matrix in the US equity market", 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

#Statistical data analysis of each column (asset class)

summary(arith_fin_returns)

#Advanced analysis of the returns for each sector

plot_density(arith_fin_returns)
plot_qq(arith_fin_returns)


#Shrinking the dataset into two different subsamples

# Specify the number of rows for the first subsample
num_rows_subsample1 <- 400

# Calculate the total number of rows in your dataset
total_rows <- nrow(arith_fin_returns)

# Create the first subsample
subsample1 <- arith_fin_returns[1:num_rows_subsample1, ]
plot_intro(subsample1)

# Create the second subsample by excluding the rows in the first subsample
subsample2 <- arith_fin_returns[(num_rows_subsample1 + 1):total_rows, ]
plot_intro(subsample2)


# Modelling part

n <- ncol(subsample1)
T <- nrow(subsample1)
e <- rep(1, n)
perio <- 12

# Computation of sigma
Sigma <- cov(subsample1) * (T - 1) / (T - n - 2) * perio

# Computation of C
C <- t(e) %*% solve(Sigma) %*% e

# Anticipated volatility
sigmag <- sqrt(1 / C)

# Computation of optimal weights "w" for GMV portfolio
omega_gmv <- 1 / as.numeric(C) * solve(Sigma) %*% e

# Plot GMV unbiased portfolio
barplot(as.numeric(omega_gmv), col = 'black')




# Perform PCA using prcomp (using subsample 2)

pca_result <- prcomp(subsample2, center = TRUE, scale = TRUE)

# Extract principal component loadings
loadings <- pca_result$rotation

# Variance explained by each principal component
var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)

# Select the number of principal components to include
num_components <- 3  # Adjust as needed

# Weights for the PCA portfolio
weights_pca <- loadings[, 1:num_components] / rowSums(loadings[, 1:num_components])

# Plot PCA portfolio weights

my_colors <- c("red", "blue", "green") # vector of colours
barplot(weights_pca, beside = TRUE, col = my_colors,
        names.arg = colnames(weights_pca),
        legend.text = colnames(weights_pca),
        main = "PCA Portfolio Weights")


#Enhanced GMV and PCA portfolio 

# Import necessary libraries
library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)

# Define the stock tickers you want to fetch data for
stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "PYPL", "CMCSA", "ADBE", "ASML",
                   "CSCO", "PEP", "AVGO", "TMUS", "INTC", "TXN", "AMGN", "NFLX", "SBUX", "AMAT")

# Fetch historical stock data from Yahoo Finance
start_date <- "2019-01-01"  # Replace with your desired start date
end_date <- "2023-08-31"    # Replace with your desired end date
stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)

# Extract adjusted closing prices (which are typically used for returns)
stock_returns <- lapply(stock_tickers, function(ticker) {
  Ad(get(ticker))
})

# Combine stock returns into a single data frame
fin_prices <- do.call(cbind, stock_returns)

# Rename columns to match your analysis
colnames(fin_prices) <- stock_tickers

# Convert the xts object to a data frame
#price_as_df <- as.data.frame(fin_prices)

# Eliminated missing data in time series
fin_return <- na.omit(fin_prices)  # Remove rows with missing values

#Computation of returns
arith_fin_returns = diff(fin_return)/lag(fin_return)
head(arith_fin_returns, n=3)
arith_fin_returns <- arith_fin_returns[-1, ]

#Data exploration for the return timeseries

plot_intro(arith_fin_returns)

# Calculate Cumulative Returns
cumulative_returns <- cumprod(1 + arith_fin_returns)

# Plot Cumulative Return Performance
library(ggplot2)
autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of Stocks") +
  ylab("Cumulative Return") +
  xlab("Year")


#plotting price in time series

library(ggplot2)
autoplot(fin_return, facets = NULL) +
  ggtitle("Daily Closing Prices") +
  ylab("Closing Price Per Share") +
  xlab("Year")



#plotting return in time series

library(ggplot2)
autoplot(arith_fin_returns, facets = NULL) +
  ggtitle("Daily Closing Prices") +
  ylab("Closing Price Per Share") +
  xlab("Year")


# Calculate the covariance matrix
cov_matrix <- cov(arith_fin_returns)

#Compute the correlation matrix of return to capture the relationships between stocks 
corrplot(cor(arith_fin_returns), type='lower', title = "Correlation matrix in the US equity market", 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

#Statistical data analysis of each column (asset class)

summary(arith_fin_returns)

#Advanced analysis of the returns for each sector

plot_density(arith_fin_returns)
plot_qq(arith_fin_returns)


#Shrinking the dataset into two different subsamples

# Specify the number of rows for the first subsample
num_rows_subsample1 <- 19

# Calculate the total number of rows in your dataset
total_rows <- nrow(arith_fin_returns)

# Create the first subsample
subsample1 <- arith_fin_returns[1:num_rows_subsample1, ]
plot_intro(subsample1)

# Create the second subsample by excluding the rows in the first subsample
subsample2 <- arith_fin_returns[(num_rows_subsample1 + 1):total_rows, ]
plot_intro(subsample2)


# Modelling part

n <- ncol(subsample1)
T <- nrow(subsample1)
e <- rep(1, n)
perio <- 12

# Computation of sigma
Sigma <- cov(subsample1) * (T - 1) / (T - n - 2) * perio

# Computation of C
C <- t(e) %*% solve(Sigma) %*% e

# Anticipated volatility
sigmag <- sqrt(1 / C)

# Computation of optimal weights "w" for GMV portfolio
omega_gmv <- 1 / as.numeric(C) * solve(Sigma) %*% e

# Plot GMV unbiased portfolio
barplot(as.numeric(omega_gmv), col = 'black')




# # Perform PCA using prcomp (using subsample 2)
# 
# pca_result <- prcomp(subsample2, center = TRUE, scale = TRUE)
# 
# # Extract principal component loadings
# loadings <- pca_result$rotation
# 
# # Variance explained by each principal component
# var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
# 
# # Select the number of principal components to include
# num_components <- 3  # Adjust as needed
# 
# # Weights for the PCA portfolio
# weights_pca <- loadings[, 1:num_components] / rowSums(loadings[, 1:num_components])
# 
# # Plot PCA portfolio weights
# 
# my_colors <- c("red", "blue", "green") # vector of colours
# barplot(weights_pca, beside = TRUE, col = my_colors,
#         names.arg = colnames(weights_pca),
#         legend.text = colnames(weights_pca),
#         main = "PCA Portfolio Weights")



#PCA one factor

valp <- eigen(Sigma)$values
vecp <- eigen(Sigma)$vectors
vp1 <- vecp[, 1]
lambda1 <- valp[1]
varepsilon1 <- diag(Sigma) - vp1 ^ 2 * lambda1
Sigma_epsilon1 <- diag(varepsilon1, n, n)
Sigma1 <- (lambda1 * vp1 %*% t(vp1) + Sigma_epsilon1)
C1 <- t(e) %*% solve(Sigma1) %*% e
sigmag1 <- sqrt(1 / C1)
omega1 <- 1 / as.numeric(C1) * solve(Sigma1) %*% e
barplot(as.numeric(omega1), col = 'black')


#PCA for three factor 

vp3 <- cbind(vecp[, 1], vecp[, 2], vecp[, 3])
lambda3 <- diag(c(valp[1], valp[2], valp[3]), 3, 3)
varepsilon3 <- diag(Sigma) - vp3 ^ 2 %*% diag(lambda3)
Sigma_epsilon3 <- diag(as.numeric(varepsilon3), n, n)
Sigma3 <- (vp3 %*% lambda3 %*% t(vp3) + Sigma_epsilon3)
C3 <- t(e) %*% solve(Sigma3) %*% e
sigmag3 <- sqrt(1 / C3)
omega3 <- 1 / as.numeric(C3) * solve(Sigma3) %*% e
barplot(as.numeric(omega3), col = 'black')



