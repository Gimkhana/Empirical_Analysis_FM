
library(tidyverse)
library(tibble)

# Parameters
mu <- 0.05  # Expected return
sigma <- 0.20  # Volatility
initial_price <- 100
risk_free_rate <- 0.02
time_horizon <- 1 # in years
time_steps <- 12 # trading month in a year
trials <- 10000

# Simulate Equity Prices
simulate_equity_prices <- function(mu, sigma, S0, T, N, trials) {
  dt <- T / N  # Time step size
  drift <- (mu - 0.5 * sigma^2) * dt
  diffusion <- sigma * sqrt(dt)
  
  # Creating a matrix of random shocks
  random_shocks <- matrix(rnorm(N * trials), nrow = N, ncol = trials)
  
  # Calculating return matrix
  return_matrix <- drift + diffusion * random_shocks
  
  # Calculating cumulative returns
  cumulative_returns <- apply(return_matrix, 2, cumsum)
  
  # Generating price paths
  price_matrix <- S0 * exp(cumulative_returns)
  
  return(price_matrix)
}

#Check for the data dimensions to ensure propoer matrix handling 
equity_prices <- simulate_equity_prices(mu, sigma, initial_price, time_horizon, time_steps, trials)
print(dim(equity_prices))

if(is.matrix(equity_prices) && ncol(equity_prices) == trials) {
  plot(NULL, xlim = c(1, time_steps), ylim = range(equity_prices), 
       xlab = "Time (Days)", ylab = "Price", main = "Simulated Equity Price Paths")
  for(i in 1:min(10, trials)) {
    lines(1:time_steps, equity_prices[,i], col = "blue", lwd = 1)
  }
} else {
  print("equity_prices is not a matrix with expected dimensions.")
}

#OBPI and CPPI Monte Carlo modelling

# Parameters
mu <- 0.05  # Expected return
sigma <- 0.20  # Volatility
initial_price <- 100
risk_free_rate <- 0.02
time_horizon <- 1 # in years
time_steps <- 12 # trading month in a year
trials <- 10000

# Black-Scholes Option Pricing Function
black_scholes_call <- function(S, K, T, r, sigma) {
  d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  call_price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  return(call_price)
}

# OBPI Strategy Implementation
obpi_strategy <- function(equity_prices, K, T, r, sigma) {
  call_prices <- apply(equity_prices, 2, function(S) black_scholes_call(S, K, T, r, sigma))
  return(call_prices)
}

# CPPI Strategy Implementation
cppi_strategy <- function(equity_prices, floor, multiplier) {
  asset_value <- matrix(nrow = nrow(equity_prices), ncol = ncol(equity_prices))
  asset_value[1,] <- initial_price
  for (t in 2:nrow(asset_value)) {
    cushion <- pmax(asset_value[t-1,] - floor, 0)
    exposure_to_risky_asset <- cushion * multiplier
    exposure_to_risk_free_asset <- asset_value[t-1,] - exposure_to_risky_asset
    asset_value[t,] <- exposure_to_risky_asset * equity_prices[t,]/equity_prices[t-1,] + exposure_to_risk_free_asset * (1 + risk_free_rate/12)
  }
  return(asset_value)
}

# Apply OBPI Strategy
option_strike_price <- 100
obpi_results <- obpi_strategy(equity_prices, option_strike_price, time_horizon, risk_free_rate, sigma)

# Apply CPPI Strategy
cppi_floor <- 100
cppi_multiplier <- 4
cppi_results <- cppi_strategy(equity_prices, cppi_floor, cppi_multiplier)

# Visualization
# Plot some of the simulated paths
plot(NULL, xlim = c(1, time_steps), ylim = range(equity_prices), 
     xlab = "Time (Days)", ylab = "Price", main = "Simulated Equity Price Paths")
for(i in 1:min(10, trials)) {
  lines(1:time_steps, equity_prices[,i], col = "red", lwd = 1)
}
# Plot some of the simulated paths for OBPI and CPPI

plot(NULL, xlim = c(1, time_steps), ylim = range(cppi_results), 
     xlab = "Time (Days)", ylab = "Price", main = "Simulated CPPI Price Paths")
for(i in 1:min(10, trials)) {
  lines(1:time_steps, cppi_results[,i], col = "green", lwd = 1)
} 

plot(NULL, xlim = c(1, time_steps), ylim = range(obpi_results), 
     xlab = "Time (Days)", ylab = "Price", main = "Simulated OBPI Price Paths")
for(i in 1:min(10, trials)) {
  lines(1:time_steps, obpi_results[,i], col = "blue", lwd = 1)
} 

# Histogram for final equity prices
hist(equity_prices[nrow(equity_prices), ], main="Histogram of Final Equity Prices", xlab="Price", col="red", breaks=50)

# Histogram for final OBPI strategy values
hist(obpi_results[nrow(obpi_results), ], main="Histogram of Final OBPI Strategy Values", xlab="Value", col="blue", breaks=50)

# Histogram for final CPPI strategy values
hist(cppi_results[nrow(cppi_results), ], main="Histogram of Final CPPI Strategy Values", xlab="Value", col="green", breaks=50)

# Histogram for final equity prices
summary_equity <- summary(equity_prices[nrow(equity_prices), ])
print(summary_equity)

# Histogram for final OBPI strategy values
summary_obpi <- summary(obpi_results[nrow(obpi_results), ])
print(summary_obpi)

# Histogram for final CPPI strategy values
summary_cppi <- summary(cppi_results[nrow(cppi_results), ])
print(summary_cppi)


#Return and volatility calculation for each variable

# Function to calculate montly returns
calculate_monthly_returns <- function(price_matrix) {
  monthly_returns <- (price_matrix[-1,] / price_matrix[-nrow(price_matrix),]) - 1
  return(monthly_returns)
}

# Calculate monthly returns for each strategy
monthly_returns_equity <- calculate_monthly_returns(equity_prices)
monthly_returns_obpi <- calculate_monthly_returns(obpi_results)
monthly_returns_cppi <- calculate_monthly_returns(cppi_results)

# Compute mean of monthly returns
mean_return_equity <- mean(monthly_returns_equity)
mean_return_obpi <- mean(monthly_returns_obpi)
mean_return_cppi <- mean(monthly_returns_cppi)

#Compute monthly volatility 
vol_equity <- mean_return_equity*sqrt(12)
vol_obpi <- mean_return_obpi*sqrt(12)
vol_cppi <- mean_return_cppi*sqrt(12)


# Creating a data frame to display results
results <- data.frame(
  Strategy = c("Equity", "OBPI", "CPPI"),
  Mean_Return = c(mean(return_equity), mean(return_obpi), mean(return_cppi)),
  Volatility = c(vol_equity, vol_obpi, vol_cppi)
)

# Annualize the mean returns
annualized_mean_return_equity <- (1 + mean_return_equity) ^ time_steps - 1
annualized_mean_return_obpi <- (1 + mean_return_obpi) ^ time_steps - 1
annualized_mean_return_cppi <- (1 + mean_return_cppi) ^ time_steps - 1

#Annualize volatility 
annualized_vol_equity <- annualized_mean_return_equity*sqrt(12)
annualized_vol_obpi <- annualized_mean_return_obpi*sqrt(12)
annualized_vol_cppi <- annualized_mean_return_cppi*sqrt(12)


#Consolidate the calculation into a dataframe
results <- data.frame(
  Strategy = c("Equity", "OBPI", "CPPI"),
  Mean_Annualized_Return = c(annualized_mean_return_equity, annualized_mean_return_obpi, annualized_mean_return_cppi),
  Volatility = c(annualized_vol_equity, annualized_vol_obpi, annualized_vol_cppi)
)

#Print the table of results
print(results)



