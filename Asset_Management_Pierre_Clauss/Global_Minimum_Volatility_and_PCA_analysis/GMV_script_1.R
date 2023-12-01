# # Import necessary libraries
# library(tidyverse)
# library(quantmod)
# library(DataExplorer)
# library(corrplot)
# install.packages("princomp")
# library(princomp)  # For PCA
# 
# 
# # Define the stock tickers you want to fetch data for
# stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN")
# 
# # Fetch historical stock data from Yahoo Finance
# start_date <- "2020-01-01"  # Replace with your desired start date
# end_date <- "2022-12-31"    # Replace with your desired end date
# stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)
# 
# # Extract adjusted closing prices (which are typically used for returns)
# stock_returns <- lapply(stock_tickers, function(ticker) {
#   Ad(get(ticker))
# })
# 
# # Combine stock returns into a single data frame
# fin_return <- do.call(cbind, stock_returns)
# 
# # Rename columns to match your analysis
# colnames(fin_return) <- stock_tickers
# 
# # Calculate the covariance matrix
# cov_matrix <- cov(fin_return)
# 
# # Modelling part
# n <- ncol(fin_return)
# T <- nrow(fin_return)
# e <- rep(1, n)
# perio <- 12
# 
# # Computation of sigma
# Sigma <- cov_matrix * (T - 1) / (T - n - 2) * perio
# 
# # Computation of C
# C <- t(e) %*% solve(Sigma) %*% e
# 
# # Anticipated volatility
# sigmag <- sqrt(1 / C)
# 
# # Computation of optimal weights "w" for GMV portfolio
# omega_gmv <- 1 / as.numeric(C) * solve(Sigma) %*% e
# 
# # Plot GMV unbiased portfolio
# barplot(as.numeric(omega_gmv), col = 'black')
# 
# # Principal Component Analysis (PCA) for portfolio optimization
# 
# # Perform PCA
# pca <- princomp(fin_return, cor = TRUE)
# 
# # Variance explained by each principal component
# var_explained <- pca$sdev^2 / sum(pca$sdev^2)
# 
# # Select the number of principal components to include
# num_components <- 2  # Adjust as needed
# 
# # Weights for the PCA portfolio
# weights_pca <- pca$loadings[, 1:num_components] / rowSums(pca$loadings[, 1:num_components])
# 
# # Plot PCA portfolio weights
# barplot(weights_pca, beside = TRUE, col = c("red", "blue"), 
#         names.arg = colnames(weights_pca), 
#         legend.text = colnames(weights_pca), 
#         main = "PCA Portfolio Weights")
# 
# 
# 
# # Import necessary libraries
# library(tidyverse)
# library(quantmod)
# library(DataExplorer)
# library(corrplot)
# 
# # Define the stock tickers you want to fetch data for
# stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN")
# 
# # Fetch historical stock data from Yahoo Finance
# start_date <- "2020-01-01"  # Replace with your desired start date
# end_date <- "2022-12-31"    # Replace with your desired end date
# stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)
# 
# # Extract adjusted closing prices (which are typically used for returns)
# stock_returns <- lapply(stock_tickers, function(ticker) {
#   Ad(get(ticker))
# })
# 
# # Combine stock returns into a single data frame
# fin_return <- do.call(cbind, stock_returns)
# 
# # Rename columns to match your analysis
# colnames(fin_return) <- stock_tickers
# 
# # Calculate the covariance matrix
# cov_matrix <- cov(fin_return)
# 
# # Modelling part
# n <- ncol(fin_return)
# T <- nrow(fin_return)
# e <- rep(1, n)
# perio <- 12
# 
# # Computation of sigma
# Sigma <- cov_matrix * (T - 1) / (T - n - 2) * perio
# 
# # Computation of C
# C <- t(e) %*% solve(Sigma) %*% e
# 
# # Anticipated volatility
# sigmag <- sqrt(1 / C)
# 
# # Computation of optimal weights "w" for GMV portfolio
# omega_gmv <- 1 / as.numeric(C) * solve(Sigma) %*% e
# 
# # Plot GMV unbiased portfolio
# barplot(as.numeric(omega_gmv), col = 'black')
# help("barplot")
# 
# # Perform PCA using prcomp
# pca_result <- prcomp(fin_return, center = TRUE, scale = TRUE)
# 
# # Extract principal component loadings
# loadings <- pca_result$rotation
# 
# # Variance explained by each principal component
# var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
# 
# # Select the number of principal components to include
# num_components <- 2  # Adjust as needed
# 
# # Weights for the PCA portfolio
# weights_pca <- loadings[, 1:num_components] / rowSums(loadings[, 1:num_components])
# 
# # Plot PCA portfolio weights
# barplot(weights_pca, beside = TRUE, col = c("red", "blue"), 
#         names.arg = colnames(weights_pca), 
#         legend.text = colnames(weights_pca), 
#         main = "PCA Portfolio Weights")


# # Import necessary libraries
# library(tidyverse)
# library(quantmod)
# library(DataExplorer)
# library(corrplot)
# install.packages("princomp")
# library(princomp)  # For PCA
# 
# 
# # Define the stock tickers you want to fetch data for
# stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN")
# 
# # Fetch historical stock data from Yahoo Finance
# start_date <- "2020-01-01"  # Replace with your desired start date
# end_date <- "2022-12-31"    # Replace with your desired end date
# stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)
# 
# # Extract adjusted closing prices (which are typically used for returns)
# stock_returns <- lapply(stock_tickers, function(ticker) {
#   Ad(get(ticker))
# })
# 
# # Combine stock returns into a single data frame
# fin_return <- do.call(cbind, stock_returns)
# 
# # Rename columns to match your analysis
# colnames(fin_return) <- stock_tickers
# 
# # Calculate the covariance matrix
# cov_matrix <- cov(fin_return)
# 
# # Modelling part
# n <- ncol(fin_return)
# T <- nrow(fin_return)
# e <- rep(1, n)
# perio <- 12
# 
# # Computation of sigma
# Sigma <- cov_matrix * (T - 1) / (T - n - 2) * perio
# 
# # Computation of C
# C <- t(e) %*% solve(Sigma) %*% e
# 
# # Anticipated volatility
# sigmag <- sqrt(1 / C)
# 
# # Computation of optimal weights "w" for GMV portfolio
# omega_gmv <- 1 / as.numeric(C) * solve(Sigma) %*% e
# 
# # Plot GMV unbiased portfolio
# barplot(as.numeric(omega_gmv), col = 'black')
# 
# # Principal Component Analysis (PCA) for portfolio optimization
# 
# # Perform PCA
# pca <- princomp(fin_return, cor = TRUE)
# 
# # Variance explained by each principal component
# var_explained <- pca$sdev^2 / sum(pca$sdev^2)
# 
# # Select the number of principal components to include
# num_components <- 2  # Adjust as needed
# 
# # Weights for the PCA portfolio
# weights_pca <- pca$loadings[, 1:num_components] / rowSums(pca$loadings[, 1:num_components])
# 
# # Plot PCA portfolio weights
# barplot(weights_pca, beside = TRUE, col = c("red", "blue"), 
#         names.arg = colnames(weights_pca), 
#         legend.text = colnames(weights_pca), 
#         main = "PCA Portfolio Weights")
# 
# 
# 
# # Import necessary libraries
# library(tidyverse)
# library(quantmod)
# library(DataExplorer)
# library(corrplot)
# 
# # Define the stock tickers you want to fetch data for
# stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN")
# 
# # Fetch historical stock data from Yahoo Finance
# start_date <- "2020-01-01"  # Replace with your desired start date
# end_date <- "2022-12-31"    # Replace with your desired end date
# stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)
# 
# # Extract adjusted closing prices (which are typically used for returns)
# stock_returns <- lapply(stock_tickers, function(ticker) {
#   Ad(get(ticker))
# })
# 
# # Combine stock returns into a single data frame
# fin_return <- do.call(cbind, stock_returns)
# 
# # Rename columns to match your analysis
# colnames(fin_return) <- stock_tickers
# 
# # Calculate the covariance matrix
# cov_matrix <- cov(fin_return)
# 
# # Modelling part
# n <- ncol(fin_return)
# T <- nrow(fin_return)
# e <- rep(1, n)
# perio <- 12
# 
# # Computation of sigma
# Sigma <- cov_matrix * (T - 1) / (T - n - 2) * perio
# 
# # Computation of C
# C <- t(e) %*% solve(Sigma) %*% e
# 
# # Anticipated volatility
# sigmag <- sqrt(1 / C)
# 
# # Computation of optimal weights "w" for GMV portfolio
# omega_gmv <- 1 / as.numeric(C) * solve(Sigma) %*% e
# 
# # Plot GMV unbiased portfolio
# barplot(as.numeric(omega_gmv), col = 'black')
# help("barplot")
# 
# # Perform PCA using prcomp
# pca_result <- prcomp(fin_return, center = TRUE, scale = TRUE)
# 
# # Extract principal component loadings
# loadings <- pca_result$rotation
# 
# # Variance explained by each principal component
# var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
# 
# # Select the number of principal components to include
# num_components <- 2  # Adjust as needed
# 
# # Weights for the PCA portfolio
# weights_pca <- loadings[, 1:num_components] / rowSums(loadings[, 1:num_components])
# 
# # Plot PCA portfolio weights
# barplot(weights_pca, beside = TRUE, col = c("red", "blue"), 
#         names.arg = colnames(weights_pca), 
#         legend.text = colnames(weights_pca), 
#         main = "PCA Portfolio Weights")


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
stock_data <- lapply(stock_tickers, function(ticker) {
  getSymbols(ticker, from = start_date, to = end_date, auto.assign = FALSE)
})

# Extract adjusted closing prices (which are typically used for returns)
stock_returns <- lapply(stock_data, function(stock) {
  daily_adj_close <- Ad(stock)
  lagged_adj_close <- lag(daily_adj_close)
  daily_returns <- (daily_adj_close - lagged_adj_close) / lagged_adj_close
  colnames(daily_returns) <- stock_tickers
  daily_returns
})

# Combine stock returns into a single data frame
fin_return <- do.call(cbind, stock_returns)

# Eliminate rows with missing values
fin_return <- na.omit(fin_return)  # Remove rows with missing values

plot(fin_return, main="Monthly Closing Prices", 
     legend.loc="topleft")

lineChart(fin_return, main="Monthly Closing Prices", 
     multi.panel=TRUE)



##check list
# Check for missing values
any(is.na(fin_return_as_df))

# Check for non-numeric values
any(!is.numeric(fin_return_as_df))



# Calculate the covariance matrix
cov_matrix <- cov(fin_return)

#Compute the correlation matrix of return to capture the relationships between stocks 
corrplot(cor(fin_return), type='lower', title = "Correlation matrix in the US equity market", 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)
help("corrplot")

#Explore the data 

#Explore the data using the package data explorer

plot_intro(fin_return)

#Explore the data using the package data explorer

plot_intro(fin_return_as_df)

#Statistical data analysis of each column (sector)

summary(fin_return)

#Advanced analysis of the returns for each sector

plot_density(fin_return)

plot_qq(fin_return)


# #shrink the dataset into two subsamples
# 
# # Convert fin_return to a data frame
# fin_return_df <- as.data.frame(fin_return)
# 
# # Define the date range for fin_return_learning
# start_date_learning <- as.Date("2020-01-01")
# end_date_learning <- as.Date("2021-12-31")
# 
# # Define the date range for fin_return_backtest
# start_date_backtest <- as.Date("2022-01-01")
# end_date_backtest <- as.Date("2022-12-31")
# 
# # Subset the data based on date ranges
# fin_return_learning <- fin_return_df[fin_return_df$Date >= start_date_learning & fin_return_df$Date <= end_date_learning, ]
# fin_return_backtest <- fin_return_df[fin_return_df$Date >= start_date_backtest & fin_return_df$Date <= end_date_backtest, ]
# 
# #Explore the data using the package data explorer
# 
# plot_intro(fin_return_learning)
# plot_intro(fin_return_backtest)

# Modelling part
n <- ncol(fin_return)
T <- nrow(fin_return)
e <- rep(1, n)
perio <- 12

# Computation of sigma
Sigma <- cov(fin_return) * (T - 1) / (T - n - 2) * perio

# Computation of C
C <- t(e) %*% solve(Sigma) %*% e

# Anticipated volatility
sigmag <- sqrt(1 / C)

# Computation of optimal weights "w" for GMV portfolio
omega_gmv <- 1 / as.numeric(C) * solve(Sigma) %*% e

# Plot GMV unbiased portfolio
barplot(as.numeric(omega_gmv), col = 'black')

# Perform PCA using prcomp
pca_result <- prcomp(fin_return, center = TRUE, scale = TRUE)

# Extract principal component loadings
loadings <- pca_result$rotation

# Variance explained by each principal component
var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)

# Select the number of principal components to include
num_components <- 2  # Adjust as needed

# Weights for the PCA portfolio
weights_pca <- loadings[, 1:num_components] / rowSums(loadings[, 1:num_components])

# Plot PCA portfolio weights
barplot(weights_pca, beside = TRUE, col = c("red", "blue"), 
        names.arg = colnames(weights_pca), 
        legend.text = colnames(weights_pca), 
        main = "PCA Portfolio Weights")

