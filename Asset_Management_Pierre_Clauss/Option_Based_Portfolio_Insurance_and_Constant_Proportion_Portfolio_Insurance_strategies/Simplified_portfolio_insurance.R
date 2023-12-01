# Packages
library(tidyverse)
library(scales)

# Simulation Function for Different Scenarios
simulate_scenario <- function(rf_scenario) {
  # Constants
  mu <- 0.05
  sigma <- 0.20
  delta <- 1 / 12
  mat <- 1
  strike <- 100
  stress <- 0.20
  tol <- 0
  
  set.seed(1234)
  
  n <- mat / delta
  u <- runif(n)
  
  # Moments on simulation path
  rf1 <- (1 + rf_scenario) ^ delta - 1
  mu1 <- (1 + mu) ^ delta - 1
  sigma1 <- sigma * sqrt(delta)
  
  # Stock simulation
  r <- qnorm(u, mu1, sigma1)
  rPRN <- qnorm(u, rf1, sigma1)
  equity <- cumprod(c(strike, 1 + r))
  equityRNP <- cumprod(c(strike, 1 + rPRN))
  
  # Call simulation
  d0 <- ((rf_scenario - sigma^2 / 2) * (mat - (0:(n-1)) * delta) + log(equity[-(n + 1)] / strike)) / 
    (sigma * sqrt(mat - (0:(n-1)) * delta))
  d1 <- d0 + sigma * sqrt(mat - (0:(n-1)) * delta)
  call <- c(equity[-(n + 1)] * pnorm(d1) - strike * exp(-rf_scenario * (mat - (0:(n-1)) * delta)) * pnorm(d0),
            max(0, equityRNP[n + 1] - strike))
  
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
  
  return(list(equity = equity, floor = floor, obpi_call = obpi_call, cppi = cppi))
}

# Starting scenario
scenario_0 <- simulate_scenario(0.02)
plot(scenario_0$equity, type = 'l', ylab = 'Strategies values', xlab = 'Months', ylim = c(70, 130))
lines(scenario_0$floor, lty = 2)
lines(scenario_0$obpi_call, col = 'blue')
lines(scenario_0$cppi, col = 'green')
legend(1, 130, c("Equity", "Floor", "OBPI", "CPPI"), col = c("black", "black", "blue", "green"), lty = c(1, 2, 1, 1))

# Scenario 1: Increase in interest rates
scenario_1 <- simulate_scenario(0.055)
plot(scenario_1$equity, main = "Scenario 1: Risk-free rate goes up", type = 'l', ylab = 'Strategies values', xlab = 'Months', ylim = c(70, 130))
lines(scenario_1$floor, lty = 2)
lines(scenario_1$obpi_call, col = 'blue')
lines(scenario_1$cppi, col = 'green')
legend(1, 130, c("Equity", "Floor", "OBPI", "CPPI"), col = c("black", "black", "blue", "green"), lty = c(1, 2, 1, 1))

# Scenario 2: Decrease in interest rates
scenario_2 <- simulate_scenario(0.005)
plot(scenario_2$equity, main = "Scenario 2: Risk-free rate goes down", type = 'l', ylab = 'Strategies values', xlab = 'Months', ylim = c(70, 130))
lines(floor, lty = 2)
lines(obpi_call, col = 'blue')
lines(cppi, col = 'green')
leg.txt <- c("Equity", "Floor", "OBPI", "CPPI")
legend(1, 130, leg.txt, col = c("black", "black", "blue", "green"), lty = c(1, 2, 1, 1))












# # Modify the simulate_scenario function to accept sigma, mu, and strike as parameters
# simulate_scenario <- function(rf_scenario, stress = 0.20, sigma = 0.20, mu = 0.05, strike = 100) {
#   # Constants
#   mu <- 0.05
#   sigma <- 0.20
#   delta <- 1 / 12
#   mat <- 1
#   strike <- 100
#   stress <- 0.20
#   tol <- 0
#   
#   set.seed(1234)
#   
#   n <- mat / delta
#   u <- runif(n)
#   
#   # Moments on simulation path
#   rf1 <- (1 + rf_scenario) ^ delta - 1
#   mu1 <- (1 + mu) ^ delta - 1
#   sigma1 <- sigma * sqrt(delta)
#   
#   # Stock simulation
#   r <- qnorm(u, mu1, sigma1)
#   rPRN <- qnorm(u, rf1, sigma1)
#   equity <- cumprod(c(strike, 1 + r))
#   equityRNP <- cumprod(c(strike, 1 + rPRN))
#   
#   # Call simulation
#   d0 <- ((rf_scenario - sigma^2 / 2) * (mat - (0:(n-1)) * delta) + log(equity[-(n + 1)] / strike)) / 
#     (sigma * sqrt(mat - (0:(n-1)) * delta))
#   d1 <- d0 + sigma * sqrt(mat - (0:(n-1)) * delta)
#   call <- c(equity[-(n + 1)] * pnorm(d1) - strike * exp(-rf_scenario * (mat - (0:(n-1)) * delta)) * pnorm(d0),
#             max(0, equityRNP[n + 1] - strike))
#   
#   # Floor simulation
#   floor <- strike / (1 + rf_scenario) ^ (mat - 0:n * delta)
#   
#   # OBPI simulation
#   gearing <- (strike - floor[1]) / call[1]
#   obpi_call <- c(strike, floor[-1] + gearing * call[-1])
#   
#   # CPPI simulation
#   cppi <- numeric(n + 1)
#   cushion <- numeric(n + 1)
#   multiplier <- numeric(n + 1)
#   expo_exante <- numeric(n + 1)
#   expo_expost <- numeric(n + 1)
#   
#   cppi[1] <- strike
#   cushion[1] <- cppi[1] - floor[1]
#   multiplier[1] <- 1 / stress
#   expo_exante[1] <- multiplier[1] * cushion[1]
#   expo_expost[1] <- expo_exante[1]
#   boundary_inf <- multiplier[1] * (1 - tol)
#   boundary_sup <- multiplier[1] * (1 + tol)
#   
#   for (i in 1:n) {
#     expo_exante[i + 1] <- expo_expost[i] * equity[i + 1] / equity[i]
#     cppi[i + 1] <- expo_exante[i + 1] + (cppi[i] - expo_expost[i]) * floor[i + 1] / floor[i]
#     cushion[i + 1] <- cppi[i + 1] - floor[i + 1]
#     multiplier[i + 1] <- expo_exante[i + 1] / cushion[i + 1]
#     expo_expost[i + 1] <- ifelse(multiplier[i + 1] < boundary_inf || multiplier[i + 1] > boundary_sup, multiplier[1], multiplier[i + 1]) * 
#       ifelse(cushion[i + 1] < 0, 0, cushion[i + 1])
#   }
#   
#   return(list(equity = equity, floor = floor, obpi_call = obpi_call, cppi = cppi))
# }
# 
# # Sensitivity Analysis on Different Parameters
# stress_values <- seq(0.10, 0.30, by = 0.01)
# sigma_values <- seq(0.10, 0.30, by = 0.01)
# mu_values <- seq(0.01, 0.09, by = 0.01)
# strike_values <- seq(90, 110, by = 1)
# 
# results_stress <- lapply(stress_values, function(stress) simulate_scenario(0.02, stress))
# results_sigma <- lapply(sigma_values, function(sigma) simulate_scenario(0.02, stress = 0.20, sigma = sigma))
# results_mu <- lapply(mu_values, function(mu) simulate_scenario(0.02, stress = 0.20, mu = mu))
# results_strike <- lapply(strike_values, function(strike) simulate_scenario(0.02, strike = strike))
# 
# # Plotting the results
# par(mfrow = c(4, 2))  # Use a 4x2 plotting area for 4 parameters and 2 strategies
# 
# # OBPI Sensitivity Plots
# plot(stress_values, sapply(results_stress, function(res) tail(res$obpi_call, 1)), 
#      type = 'b', col = 'blue', xlab = 'Stress Value', ylab = 'OBPI Value at Maturity', main = "OBPI Sensitivity to Stress Value")
# 
# plot(sigma_values, sapply(results_sigma, function(res) tail(res$obpi_call, 1)), 
#      type = 'b', col = 'blue', xlab = 'Volatility (Sigma)', ylab = 'OBPI Value at Maturity', main = "OBPI Sensitivity to Sigma")
# 
# plot(mu_values, sapply(results_mu, function(res) tail(res$obpi_call, 1)), 
#      type = 'b', col = 'blue', xlab = 'Return (Mu)', ylab = 'OBPI Value at Maturity', main = "OBPI Sensitivity to Mu")
# 
# plot(strike_values, sapply(results_strike, function(res) tail(res$obpi_call, 1)), 
#      type = 'b', col = 'blue', xlab = 'Strike Price', ylab = 'OBPI Value at Maturity', main = "OBPI Sensitivity to Strike Price")
# 
# # CPPI Sensitivity Plots
# plot(stress_values, sapply(results_stress, function(res) tail(res$cppi, 1)), 
#      type = 'b', col = 'green', xlab = 'Stress Value', ylab = 'CPPI Value at Maturity', main = "CPPI Sensitivity to Stress Value")
# 
# plot(sigma_values, sapply(results_sigma, function(res) tail(res$cppi, 1)), 
#      type = 'b', col = 'green', xlab = 'Volatility (Sigma)', ylab = 'CPPI Value at Maturity', main = "CPPI Sensitivity to Sigma")
# 
# plot(mu_values, sapply(results_mu, function(res) tail(res$cppi, 1)), 
#      type = 'b', col = 'green', xlab = 'Return (Mu)', ylab = 'CPPI Value at Maturity', main = "CPPI Sensitivity to Mu")
# 
# plot(strike_values, sapply(results_strike, function(res) tail(res$cppi, 1)), 
#      type = 'b', col = 'green', xlab = 'Strike Price', ylab = 'CPPI Value at Maturity', main = "CPPI Sensitivity to Strike Price")











# Packages
library(tidyverse)
library(scales)

# Simulation Function for Different Scenarios
simulate_scenario <- function(rf_scenario, stress = 0.20, sigma = 0.20, mu = 0.05, strike = 100) {
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
  
  # Stock simulation
  r <- qnorm(u, mu1, sigma1)
  rPRN <- qnorm(u, rf1, sigma1)
  equity <- cumprod(c(strike, 1 + r))
  equityRNP <- cumprod(c(strike, 1 + rPRN))
  
  # Call simulation
  d0 <- ((rf_scenario - sigma^2 / 2) * (mat - (0:(n-1)) * delta) + log(equity[-(n + 1)] / strike)) / 
    (sigma * sqrt(mat - (0:(n-1)) * delta))
  d1 <- d0 + sigma * sqrt(mat - (0:(n-1)) * delta)
  call <- c(equity[-(n + 1)] * pnorm(d1) - strike * exp(-rf_scenario * (mat - (0:(n-1)) * delta)) * pnorm(d0),
            max(0, equityRNP[n + 1] - strike))
  
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
  
  return(list(equity = equity, floor = floor, obpi_call = obpi_call, cppi = cppi))
}

# Function to plot results
plot_results <- function(scenario, title) {
  plot(scenario$equity, main = title, type = 'l', ylab = 'Strategies values', xlab = 'Months', ylim = c(70, 130))
  lines(scenario$floor, lty = 2)
  lines(scenario$obpi_call, col = 'blue')
  lines(scenario$cppi, col = 'green')
  legend(1, 130, c("Equity", "Floor", "OBPI", "CPPI"), col = c("black", "black", "blue", "green"), lty = c(1, 2, 1, 1))
}

# Starting scenario
par(mfrow = c(2, 2))  # Using 2x2 for now. Adjust as needed.
scenario_0 <- simulate_scenario(0.02)
plot_results(scenario_0, "Starting Scenario")

# Scenario 1: Increase in interest rates
scenario_1 <- simulate_scenario(0.055)
plot_results(scenario_1, "Scenario 1: Risk-free rate goes up")

# Scenario 2: Decrease in interest rates
scenario_2 <- simulate_scenario(0.005)
plot_results(scenario_2, "Scenario 2: Risk-free rate goes down")

# Sensitivity Analysis on Different Parameters (and plotting some of them)
stress_values <- seq(0.10, 0.30, by = 0.01)
results_stress <- lapply(stress_values, function(stress) simulate_scenario(0.02, stress))
# Plotting the results of the first stress scenario for illustration
plot_results(results_stress[[1]], "Scenario 3: Stress variation")

# Additional sensitivity analyses can be plotted similarly.

# Reset the plot layout
par(mfrow = c(1, 1))










