# Load the required packages
library(tidyverse)
library(scales)

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

# Define a function to plot the results
plot_results <- function(results, title) {
  t <- 1:length(results$equity)
  
  # Convert the results into a data frame for plotting
  df <- data.frame(
    t = rep(t, 4),
    Value = c(results$equity, results$floor, results$obpi_call, results$cppi),
    Type = factor(rep(c("Equity", "Floor", "OBPI", "CPPI"), each=length(t)))
  )
  
  # Generate the plot
  ggplot(df, aes(x=t, y=Value, color=Type)) +
    geom_line() +
    labs(title=title, x="Time", y="Value", color="Legend") +
    scale_color_brewer(palette="Dark2") +
    theme_minimal()
}

# Call the plot_results function for each sensitivity analysis
plot_results(results_rf[[2]], "Sensitivity analysis: Risk-free rate variation")
plot_results(results_stress[[2]], "Sensitivity analysis: Stress value variation")
plot_results(results_sigma[[2]], "Sensitivity analysis: Volatility variation")
plot_results(results_mu[[2]], "Sensitivity analysis: Return (mu) variation")
plot_results(results_strike[[2]], "Sensitivity analysis: Strike price variation")

# Create a function to plot the 3D surface plot
plot_surface_plot <- function(rf_scenario, sigma_scenario, call_option_price, title = "3D Surface Plot of Call Option Price", xlab = "Risk-free Rate", ylab = "Volatility", zlab = "Call Option Price") {
  # Create a grid of risk-free rates and volatilities
  rf_grid <- seq(from = 0.005, to = 0.055, by = 0.005)
  sigma_grid <- seq(from = 0.10, to = 0.30, by = 0.05)
  
  # Calculate the call option price for each combination of risk-free rate and volatility
  call_option_price_grid[i, j] <- unlist(simulate_scenario(rf_scenario = rf_grid[i], sigma = sigma_grid[j]))
  for (i in 1:length(rf_grid)) {
    for (j in 1:length(sigma_grid)) {
      call_option_price_grid[i, j] <- simulate_scenario(rf_scenario = rf_grid[i], sigma = sigma_grid[j])
    }
  }
  
  # Create the 3D surface plot
  persp(sigma_grid, rf_grid, call_option_price_grid, col = heat.colors(100), axis.args = list(x = xlab, y = ylab, z = zlab), title = title)
}

# Plot the 3D surface plot
plot_surface_plot(rf_scenario = 0.02, sigma_scenario = 0.20, call_option_price = call_option_price)





# Monte Carlo Simulation

# Load the required libraries again (it seems redundant, as these were loaded at the start)
library(tidyverse)
library(scales)

# Improved simulate_scenario function
simulate_scenario <- function(rf_scenario, n_sim = 10000, stress = 0.20, sigma = 0.20, mu = 0.05, strike = 100) {
  delta <- 1 / 12
  mat <- 1
  tol <- 0
  
  set.seed(1234)
  
  n <- mat / delta
  u <- runif(n)
  
  # Moments on simulation path
  rf1 <- (1 + rf_scenario) ^ delta - 1
  mu1 <- (1 + mu) ^ delta - 1
  sigma1 <- sigma * sqrt(delta)
  
  # Monte Carlo simulation logic
  results <- replicate(n_sim, {
    # Stock simulation
    r <- qnorm(u, mu1, sigma1)
    equity <- cumprod(c(strike, 1 + r))
    
    # Call simulation
    d0 <- ((rf_scenario - sigma^2 / 2) * (mat - (0:(n-1)) * delta) + log(equity[-(n + 1)] / strike)) / 
      (sigma * sqrt(mat - (0:(n-1)) * delta))
    d1 <- d0 + sigma * sqrt(mat - (0:(n-1)) * delta)
    call <- equity[-(n + 1)] * pnorm(d1) - strike * exp(-rf_scenario * (mat - (0:(n-1)) * delta)) * pnorm(d0)
    
    # Floor simulation
    floor <- strike / (1 + rf_scenario) ^ (mat - 0:n * delta)
    
    # OBPI simulation
    gearing <- (strike - floor[1]) / call[1]
    obpi_call <- c(strike, floor[-1] + gearing * call[-1])
    
    # CPPI simulation
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
    
    for (i in 1:n) {
      expo_exante[i + 1] <- expo_expost[i] * equity[i + 1] / equity[i]
      cppi[i + 1] <- expo_exante[i + 1] + (cppi[i] - expo_expost[i]) * floor[i + 1] / floor[i]
      cushion[i + 1] <- cppi[i + 1] - floor[i + 1]
      multiplier[i + 1] <- expo_exante[i + 1] / cushion[i + 1]
      expo_expost[i + 1] <- ifelse(multiplier[i + 1] < boundary_inf || multiplier[i + 1] > boundary_sup, multiplier[1], multiplier[i + 1]) * 
        ifelse(cushion[i + 1] < 0, 0, cushion[i + 1])
    }
    
    return(list(equity = tail(equity, 1), obpi_call = tail(obpi_call, 1), cppi = tail(cppi, 1)))
  }, simplify = "data.frame")
  
  return(results)
}

# Perform Monte Carlo Simulation with 10,000 scenarios
mc_results <- simulate_scenario(0.02, n_sim = 10000)

# Plot histograms
par(mfrow=c(3,1)) # Setting layout to 3x1
hist(mc_results$equity, main="Histogram of Equity Terminal Values", col="blue", breaks=50, xlab="Value")
hist(mc_results$obpi_call, main="Histogram of OBPI Terminal Values", col="red", breaks=50, xlab="Value")
hist(mc_results$cppi, main="Histogram of CPPI Terminal Values", col="green", breaks=50, xlab="Value")