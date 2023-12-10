
library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)

##Project 1: Global Minimum Volatility portfolio (GMV) on an Equity Portfolio

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

# Computation of returns
arith_fin_returns = diff(fin_return)/lag(fin_return)
head(arith_fin_returns, n=3)
arith_fin_returns <- arith_fin_returns[-1, ]

# Data structure check
plot_intro(arith_fin_returns)

# Calculate Cumulative Returns
cumulative_returns <- cumprod(1 + arith_fin_returns)

# Plot Cumulative Return Performance
library(ggplot2)
autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of the investment universe") +
  ylab("Cumulative Return") +
  xlab("Year")

# Covariance matrix calculation
cov_matrix <- cov(arith_fin_returns)

# Correlation analysis
corrplot(cor(arith_fin_returns), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

# Summary statistics
summary(arith_fin_returns)


# Advanced statistical analysis for the dataset
plot_density(arith_fin_returns)
plot_qq(arith_fin_returns)


##Modelling of project 1

#Construction of subsamoples
num_rows_subsample1 <- 20
total_rows <- nrow(arith_fin_returns)

#Subsample 1
subsample1 <- arith_fin_returns[1:num_rows_subsample1, ]
plot_intro(subsample1)

#Subsample 2
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
omega_gmv <- 1 / as.numeric(C) * solve(Sigma_reg) %*% e

#Plot of the GMV portfolio
barplot(as.numeric(omega_gmv), col = 'black', ylim = c(-0.5, 1))

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

## Project 2: Tangency Portfolio and Black-Litterman Approach

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
library(corrplot)
corrplot( cor(arith_fin_returns), type='lower', method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

##Modelling of project 2

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

#Q vector of views
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


#Project 3: Portfolio Insurance Strategies

# Create a function to simulate a scenario using given parameters
simulate_scenario <- function(rf_scenario, n_sim = 10000, stress = 0.20, sigma = 0.20, mu = 0.05, strike = 100) {
  # Define constants
  delta <- 1 / 12
  mat <- 1
  tol <- 0
  
  # Set a seed for reproducibility
  set.seed(1234)
  
  # Calculate the number of simulation steps
  n <- mat / delta
  # Generate a random uniform sequence of length n
  u <- runif(n)
  
  # Calculate moments on the simulation path
  rf1 <- (1 + rf_scenario) ^ delta - 1
  mu1 <- (1 + mu) ^ delta - 1
  sigma1 <- sigma * sqrt(delta)
  
  # Simulate stock returns
  r <- qnorm(u, mu1, sigma1)
  # Simulate equity price paths
  equity <- cumprod(c(strike, 1 + r))
  rPRN <- qnorm(u, rf1, sigma1)
  equityRNP <- cumprod(c(strike, 1 + rPRN))
  
  # Simulate the call option price
  d0 <- ((rf_scenario - sigma^2 / 2) * (mat - (0:(n-1)) * delta) + log(equity[-(n + 1)] / strike)) / 
    (sigma * sqrt(mat - (0:(n-1)) * delta))
  d1 <- d0 + sigma * sqrt(mat - (0:(n-1)) * delta)
  call <- c(equity[-(n + 1)] * pnorm(d1) - strike * exp(-rf_scenario * (mat - (0:(n-1)) * delta)) * pnorm(d0),
            max(0, equityRNP[n + 1] - strike))
  
  # Simulate the floor (minimum guaranteed value)
  floor <- strike / (1 + rf_scenario) ^ (mat - 0:n * delta)
  
  # Simulate OBPI (Option Based Portfolio Insurance) 
  gearing <- (strike - floor[1]) / call[1]
  obpi_call <- c(strike, floor[-1] + gearing * call[-1])
  
  # Simulate CPPI (Constant Proportion Portfolio Insurance)
  cppi <- numeric(n + 1)
  cushion <- numeric(n + 1)
  multiplier <- numeric(n + 1)
  expo_exante <- numeric(n + 1)
  expo_expost <- numeric(n + 1)
  
  cppi[1] <- strike
  cushion[1] <- cppi[1] - floor[1]
  multiplier[1] <- 1 / stress
  expo_exante[1] <- multiplier[1] * cushion[1]
  expo_expost[1] <- expo_exante[1]
  boundary_inf <- multiplier[1] * (1 - tol)
  boundary_sup <- multiplier[1] * (1 + tol)
  
  # Loop to calculate CPPI values
  for (i in 1:n) {
    expo_exante[i + 1] <- expo_expost[i] * equity[i + 1] / equity[i]
    cppi[i + 1] <- expo_exante[i + 1] + (cppi[i] - expo_expost[i]) * floor[i + 1] / floor[i]
    cushion[i + 1] <- cppi[i + 1] - floor[i + 1]
    multiplier[i + 1] <- expo_exante[i + 1] / cushion[i + 1]
    expo_expost[i + 1] <- ifelse(multiplier[i + 1] < boundary_inf || multiplier[i + 1] > boundary_sup, multiplier[1], multiplier[i + 1]) * 
      ifelse(cushion[i + 1] < 0, 0, cushion[i + 1])
  }
  
  # Return the results as a list
  return(list(equity = equity, floor = floor, obpi_call = obpi_call, cppi = cppi))
}


# Sensitivity analysis: generate results for variations in each parameter
rf_values <- seq(0.02, 0.10, by = 0.01)
stress_values <- seq(0.10, 0.30, by = 0.01)
sigma_values <- seq(0.10, 0.30, by = 0.01)
mu_values <- seq(0.01, 0.09, by = 0.01)
strike_values <- seq(90, 110, by = 1)

results_rf <- lapply(rf_values, function(rf_scenario) simulate_scenario(0.02, rf_scenario = rf_scenario))
results_stress <- lapply(stress_values, function(stress) simulate_scenario(0.02, stress))
results_sigma <- lapply(sigma_values, function(sigma) simulate_scenario(0.02, stress = 0.20, sigma = sigma))
results_mu <- lapply(mu_values, function(mu) simulate_scenario(0.02, stress = 0.20, mu = mu))
results_strike <- lapply(strike_values, function(strike) simulate_scenario(0.02, strike = strike))

#Plotting of the results
plot_results <- function(results, title) {
  t <- 1:length(results$equity)
  
  # Convert the results into a data frame for plotting
  df <- data.frame(
    t = rep(t, 4),
    Value = c(results$equity, results$floor, results$obpi_call, results$cppi),
    Type = factor(rep(c("Equity", "Floor", "OBPI", "CPPI"), each=length(t)))
  )
  
  # Check if 'df' exists and is a data frame
  if (!exists("df") || !is.data.frame(df)) {
    stop("Data frame 'df' does not exist or is not a data frame.")
  }
  
  # Ensure the data frame has the necessary columns
  if (!all(c("t", "Value", "Type") %in% names(df))) {
    stop("Data frame 'df' does not contain the required columns.")
  }
  
  # Generate the plot
  ggplot(df, aes(x = t, y = Value, color = Type)) +
    geom_line() +
    labs(title = title, x = "Time", y = "Value", color = "Legend") +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', color = "white"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5.5, 5.5, 5.5, 5.5)
    )
}

#Sensitivity analysis of each plot
plot_results(results_rf[[2]], "Sensitivity analysis: Risk-free rate variation")
plot_results(results_stress[[2]], "Sensitivity analysis: Stress value variation")
plot_results(results_sigma[[2]], "Sensitivity analysis: Volatility variation")
plot_results(results_mu[[2]], "Sensitivity analysis: Return (mu) variation")
plot_results(results_strike[[2]], "Sensitivity analysis: Strike price variation")

#Monte Carlo simulation
simulate_scenario <- function(rf_scenario, n_sim = 10000, stress = 0.20, sigma = 0.20, mu = 0.05, strike = 100) {
  delta <- 1 / 12  # monthly steps
  mat <- 1  # one year
  n <- mat / delta  # number of steps
  
  # Matrices to store simulation results
  equity_paths <- matrix(nrow = n_sim, ncol = n + 1)
  obpi_calls <- matrix(nrow = n_sim, ncol = n + 1)
  cppi_values <- matrix(nrow = n_sim, ncol = n + 1)
  
  # Outer loop for each simulation
  for (sim in 1:n_sim) {
    u <- runif(n)  # uniform random numbers for the entire path
    
    # Calculate monthly returns and simulate price path
    rf1 <- (1 + rf_scenario) ^ delta - 1
    mu1 <- (1 + mu) ^ delta - 1
    sigma1 <- sigma * sqrt(delta)
    
    r <- qnorm(u, mean = mu1, sd = sigma1)  # convert to normal distribution
    equity <- cumprod(c(strike, 1 + r))  # simulate equity path
    
    # Calculate option prices along the path
    d1 <- (log(equity[-(n+1)] / strike) + (rf_scenario + sigma^2 / 2) * (mat - (0:(n-1)) * delta)) / (sigma * sqrt(mat - (0:(n-1)) * delta))
    d2 <- d1 - sigma * sqrt(mat - (0:(n-1)) * delta)
    call <- equity[-(n+1)] * pnorm(d1) - strike * exp(-rf_scenario * (mat - (0:(n-1)) * delta)) * pnorm(d2)
    
    floor <- strike / (1 + rf_scenario) ^ (mat - 0:n * delta)  # simulate floor values
    
    # OBPI strategy
    gearing <- (strike - floor[1]) / call[1]
    obpi_call <- c(strike, floor[-1] + gearing * call[-1])
    
    # CPPI strategy
    cppi <- numeric(n + 1)
    cppi[1] <- strike
    multiplier <- 1 / stress
    for (i in 1:n) {
      cushion <- cppi[i] - floor[i]
      expo_exante <- multiplier * cushion
      cppi[i + 1] <- expo_exante * equity[i + 1] / equity[i] + (cppi[i] - expo_exante) * (1 + rf_scenario)^delta
    }
    
    # Store the results of this simulation
    equity_paths[sim, ] <- equity
    obpi_calls[sim, ] <- obpi_call
    cppi_values[sim, ] <- cppi
  }
  
  # Average the results across all simulations
  list(
    average_equity = rowMeans(equity_paths),
    average_obpi_call = rowMeans(obpi_calls),
    average_cppi = rowMeans(cppi_values)
  )
}

# Run the Monte Carlo simulation with specified parameters
set.seed(123)  # Setting seed for reproducibility
mc_results <- simulate_scenario(rf_scenario = 0.02, n_sim = 10000)

# Histogram for final equity values
hist(mc_results$average_equity, main="Histogram of Final Equity Values", xlab="Value", col="red", breaks=50)

# Histogram for final OBPI strategy values
hist(mc_results$average_obpi_call, main="Histogram of Final OBPI Strategy Values", xlab="Value", col="blue", breaks=50)

# Histogram for final CPPI strategy values
hist(mc_results$average_cppi, main="Histogram of Final CPPI Strategy Values", xlab="Value", col="green", breaks=50)
