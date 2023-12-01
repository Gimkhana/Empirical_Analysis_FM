# # Import necessary libraries
# library(tidyverse)
# library(quantmod)
# library(DataExplorer)
# library(corrplot)
# library(scales)
# 
# # Define the stock tickers you want to fetch data for
# stock_tickers <- c("^GSPC", "BNDX", "TLT", "DBC", "IEF")
# 
# # Fetch historical stock data from Yahoo Finance
# start_date <- "2019-01-01"  # Replace with your desired start date
# end_date <- "2023-08-31"    # Replace with your desired end date
# 
# # Use getSymbols to fetch data for all stock tickers
# stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)
# 
# # Extract adjusted close prices (which are typically used for returns)
# adj_close_data <- lapply(stock_tickers, function(ticker) {
#   Cl(get(ticker))
# })
# 
# # Combine the adjusted close data into a single data frame
# fin_prices <- as.data.frame(do.call(cbind, adj_close_data))
# 
# # Eliminate missing data in time series
# fin_return <- na.omit(fin_prices)  # Remove rows with missing values
# 
# # Computation of returns
# arith_fin_returns = diff(fin_return)/lag(fin_return)
# head(arith_fin_returns, n=3)
# arith_fin_returns <- arith_fin_returns[-1, ]
# 
# # Data exploration for the return time series
# plot_intro(arith_fin_returns)



# Import necessary libraries
library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)

# Define the stock tickers you want to fetch data for
stock_tickers <- c("^GSPC", "BNDX", "TLT", "DBC", "IEF")


# Fetch historical stock data from Yahoo Finance
start_date <- "2019-01-01"  # Replace with your desired start date
end_date <- "2023-08-31"    # Replace with your desired end date
stock_data <- getSymbols(stock_tickers, from = start_date, to = end_date, auto.assign = TRUE)


# Extract adjusted closing prices (which are typically used for returns)
stock_returns <- lapply(stock_tickers, function(ticker) {
  Ad(get(ticker))
})


# Combine the adjusted close data into a single data frame
fin_prices <- do.call(cbind, stock_data)

# Eliminate missing data in time series
fin_return <- na.omit(fin_prices)  # Remove rows with missing values

# Computation of returns
arith_fin_returns = diff(fin_return)/lag(fin_return)
head(arith_fin_returns, n=3)
arith_fin_returns <- arith_fin_returns[-1, ]

# Data exploration for the return time series
plot_intro(arith_fin_returns)

