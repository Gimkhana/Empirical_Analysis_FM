library(tidyverse)
library(quantmod)

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

# Assuming mc_results contains the full simulation data for equity_paths, obpi_calls, and cppi_values

# Histogram for final equity values
hist(mc_results$average_equity, main="Histogram of Final Equity Values", xlab="Value", col="red", breaks=50)

# Histogram for final OBPI strategy values
hist(mc_results$average_obpi_call, main="Histogram of Final OBPI Strategy Values", xlab="Value", col="blue", breaks=50)

# Histogram for final CPPI strategy values
hist(mc_results$average_cppi, main="Histogram of Final CPPI Strategy Values", xlab="Value", col="green", breaks=50)

