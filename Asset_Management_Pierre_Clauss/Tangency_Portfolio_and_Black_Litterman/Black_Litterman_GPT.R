# # Load necessary libraries
# library(PerformanceAnalytics)
# library(quadprog)
# library(quantmod)
# 
# # Define the stock tickers and market index
# stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "PYPL", "CMCSA", "ADBE", "ASML",
#                    "CSCO", "PEP", "AVGO", "TMUS", "INTC", "TXN", "AMGN", "NFLX", "SBUX", "AMAT")
# market_index <- "^GSPC"  # S&P 500 Index
# 
# # Fetch historical stock and market data from Yahoo Finance
# start_date <- "2019-01-01"
# end_date <- "2023-08-31"
# 
# # Create an empty data frame to store stock data
# stock_data <- data.frame()
# 
# # # Fetch each stock's data and merge it into the stock_data data frame
# # for (ticker in stock_tickers) {
# #   stock <- getSymbols(ticker, from = start_date, to = end_date, auto.assign = FALSE)
# #   stock_data <- merge(stock_data, stock, by = "Date", all = TRUE)
# # }
# 
# # Create an empty data frame to store stock data
# stock_data <- data.frame(Date = index(getSymbols(stock_tickers[1], from = start_date, to = end_date, auto.assign = FALSE)))
# 
# # Loop through stock tickers and fetch data
# for (ticker in stock_tickers) {
#   tryCatch(
#     {
#       # Fetch data for the current ticker
#       stock <- getSymbols(ticker, from = start_date, to = end_date, auto.assign = FALSE)
#       
#       # Extract adjusted closing prices (which are typically used for returns)
#       stock_returns <- Ad(stock)
#       
#       # Rename the column to the stock ticker
#       colnames(stock_returns) <- ticker
#       
#       # Merge the data into the main data frame using the "Date" column
#       stock_data <- merge(stock_data, stock_returns, by = "Date", all = TRUE)
#     },
#     error = function(e) {
#       # Print an error message for the ticker that caused the issue
#       cat("Error fetching data for ticker:", ticker, "\n")
#     }
#   )
# }
# 
# 
# 
# 
# # Fetch market data
# market_data <- getSymbols(market_index, from = start_date, to = end_date, auto.assign = FALSE)
# 
# # Extract adjusted closing prices (which are typically used for returns)
# stock_returns <- Return.calculate(stock_data, method = "log")
# market_returns <- Return.calculate(market_data, method = "log")
# 
# # Calculate stock and market returns
# stock_returns <- Return.calculate(stock_data, method = "log")
# market_returns <- Return.calculate(market_data, method = "log")
# 
# # Calculate expected returns and covariance matrix
# expected_returns <- colMeans(stock_returns, na.rm = TRUE) * 252  # Annualize returns
# cov_matrix <- cov(stock_returns) * 252  # Annualize covariance matrix
# 
# # Define risk-free rate (e.g., 10-year US Treasury yield)
# rf_rate <- 0.015  # Replace with the appropriate risk-free rate
# 
# # Define market capitalization weights (you can adjust these)
# market_caps <- c(2.47e12, 2.46e12, 1.86e12, 1.56e12, 867.78e9, 809.37e9, 277.54e9, 264.65e9, 267.79e9, 311.43e9,
#                  273.68e9, 198.95e9, 229.33e9, 146.18e9, 192.28e9, 172.24e9, 138.15e9, 127.03e9, 292.95e9, 150.72e9,
#                  130.44e9)
# 
# # Calculate market weights
# market_weights <- market_caps / sum(market_caps)
# 
# # Calculate market return
# market_return <- colMeans(market_returns, na.rm = TRUE) * 252  # Annualize market return
# 
# # Calculate market risk premium
# market_rp <- market_return - rf_rate
# 
# # Define investor's views and uncertainties
# views <- c(0.02, 0.03)  # Expected return views for AAPL and MSFT
# view_uncertainties <- c(0.02, 0.02)  # Uncertainties of the views
# 
# # Calculate the Black-Litterman expected returns and covariances
# omega <- diag(view_uncertainties^2)
# P <- matrix(0, nrow = 2, ncol = length(stock_tickers))
# P[1, which(stock_tickers == "AAPL")] <- 1
# P[2, which(stock_tickers == "MSFT")] <- 1
# 
# Q <- views
# 
# tau <- 0.025  # Tau parameter (you can adjust this)
# 
# BL_expected_returns <- solve(tau * cov_matrix) %*% (solve(tau * cov_matrix) %*% expected_returns + 
#                                                       t(P) %*% solve(omega) %*% (Q - P %*% expected_returns))
# BL_cov_matrix <- cov_matrix + solve(tau * cov_matrix) - solve(tau * cov_matrix) %*% 
#   t(P) %*% solve(omega) %*% P %*% solve(tau * cov_matrix)
# 
# # Perform portfolio optimization using the Black-Litterman expected returns and covariances
# port_returns <- seq(0.01, 0.20, by = 0.01)  # Define a range of portfolio returns
# portfolios <- matrix(0, nrow = length(port_returns), ncol = length(stock_tickers))
# 
# for (i in 1:length(port_returns)) {
#   res <- solve.QP(Dmat = BL_cov_matrix, dvec = rep(0, length(stock_tickers)), 
#                   Amat = cbind(market_weights, rep(1, length(stock_tickers))),
#                   bvec = c(market_rp, 1, port_returns[i]), meq = 2)
#   portfolios[i,] <- res$solution
# }
# 
# # Visualize the efficient frontier
# eff_frontier <- data.frame(Return = port_returns, Risk = sqrt(diag(portfolios %*% cov_matrix %*% t(portfolios))))
# 
# # Plot the efficient frontier
# library(ggplot2)
# ggplot(eff_frontier, aes(x = Risk, y = Return)) +
#   geom_line() +
#   geom_point(data = data.frame(Return = BL_expected_returns, Risk = sqrt(diag(BL_cov_matrix))), 
#              aes(x = sqrt(diag(BL_cov_matrix)), y = BL_expected_returns), color = "red", size = 2) +
#   labs(x = "Portfolio Risk (Standard Deviation)", y = "Portfolio Expected Return") +
#   geom_text(data = data.frame(Return = BL_expected_returns, Risk = sqrt(diag(BL_cov_matrix))), 
#             aes(x = sqrt(diag(BL_cov_matrix)), y = BL_expected_returns, 
#                 label = names(stock_tickers)), hjust = -0.2, vjust = -0.5) +
#   theme_minimal()
