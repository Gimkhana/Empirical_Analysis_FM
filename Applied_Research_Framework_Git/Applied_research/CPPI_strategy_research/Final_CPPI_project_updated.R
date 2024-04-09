##############################################################################################################

# Comparative analysis of CPPI and OBPI portfolio insurance strategies under different modelling techniques: 
# Black-Scholes versus Lévy jump diffusion process

# Youssef LOURAOUI \\ M2 Risk and Asset Management \\ Université Paris-Saclay \\ youssef.louraoui@essec.edu
# Hassan Khalil\\ M2 Quantitative Finance \\ Université Paris-Saclay \\ hassan.khalil@etud.univ-evry.fr
# Najwa Kandali\\ M2 Quantitative Finance \\ Université Paris-Saclay \\ najwa.kandali@etud.univ-evry.fr
# Cyril Benezet \\ Professor ENSIIE \\ cyril.benezet@ensiie.fr

##############################################################################################################
#NB: Make sure to run the code by block and not run the entire code as it can lead to problems in execution. Each block is marked with an '------' separation.

#Import the necessary packages to run the RStudio script

library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tibble)
library(rugarch) # Load necessary library for GARCH modeling

### 0. Data processing and statistical analysis of the risky asset (market portfolio) and risk-free rate
#NB: Run each part of the RStudio script independently to avoid errors 

# Define the tickers you want to fetch data for
index_ticker <- c("^GSPC")
bond_ticker <- c("DGS10")

# Fetch historical data from Yahoo Finance using the Quantmod package
start_date <- "2013-01-01"  # Replace with your desired start date
end_date <- "2023-01-01"    # Replace with your desired end date
index_data <- getSymbols(index_ticker, from = start_date, to = end_date, auto.assign = TRUE)
bond_data <- getSymbols(bond_ticker, src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Ensure bond data is in the same frequency (daily)
bond_data_daily <- na.approx(bond_data)/100  # Interpolates NA values

# Extract adjusted closing prices (which are typically used for returns)
index_price <- Cl(get(index_data))

# Extract the last value of the index level and bond yield
last_index_value <- tail(index_price, 1)
last_bond_yield <- tail(bond_data_daily, 1)

# Print the last values
print(last_index_value)
print(last_bond_yield)

#Computation of returns for the index
index_returns = diff(index_price)/lag(index_price)
head(index_returns, n=3)
arith_index_returns <- index_returns[-1, ]

# Align the datasets by date
aligned_data <- na.omit(merge(index_price, bond_data_daily))

# Align the datasets by date on the basis of return and yield
aligned_data_mod <- na.omit(merge(arith_index_returns, bond_data_daily))

# Rename columns for clarity
colnames(aligned_data_mod) <- c("Index_Return", "Bond_Yield")
head(aligned_data, n=3)
head(aligned_data_mod, n=3)

# Convert the xts object to a tibble for easier manipulation
aligned_data_df <- as_tibble(data.frame(date = index(aligned_data_mod), coredata(aligned_data_mod)), rownames = "date")

# Ensure unique column names
names(aligned_data_df) <- make.names(names(aligned_data_df), unique = TRUE)

# Define a threshold for identifying jumps
threshold <- 3 * sd(aligned_data_df$Index_Return, na.rm = TRUE)

# Identify jumps using the filter function
jumps <- aligned_data_df %>% 
  filter(abs(Index_Return) > threshold)

# Estimate lambda, k, and delta
lambda_index <- nrow(jumps) / (nrow(aligned_data_df) / 252)
k_index <- mean(log(1 + jumps$Index_Return))
delta_index <- sd(log(1 + jumps$Index_Return))

# Print the estimates
print(paste("Lambda (Jump Intensity):", lambda_index))
print(paste("K (Average Log Jump Size):", k_index))
print(paste("Delta (Jump Size Volatility):", delta_index))


# Analysis of the time series
plot_intro(aligned_data_mod)

#S&P500 time series plot
index_price_df <- data.frame(Date = index(index_price), Price = coredata(index_price))

ggplot(index_price_df, aes(x = Date, y = GSPC.Close)) +
  geom_line(color = "blue") +
  labs(title = "S&P 500 Index Price",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#US 10Y Treasury yields time series plot
bond_data_daily_df <- data.frame(Date = index(bond_data_daily), Yield = coredata(bond_data_daily))

ggplot(bond_data_daily_df, aes(x = Date, y = DGS10)) +
  geom_line(color = "red") +
  labs(title = "US Treasury 10-Year Yield",
       x = "Date",
       y = "Yield (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Correlation analysis
corrplot(cor(aligned_data_mod), type='full', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)
help("corrplot")
cov_matrix <- cov(aligned_data_mod)
cov2cor(cov_matrix)

# Basic statistics for each factors
summary(aligned_data_mod*100)

# # Subset the data for the last 10 years
# end_date <- as.Date("2023-01-01")  # Adjust this date if needed
# start_date <- end_date - years(10)
# 
# # Filter the data for the last 10 years
# last_10_years_data <- aligned_data_mod[as.Date(rownames(aligned_data_mod)) >= start_date & as.Date(rownames(aligned_data_mod)) <= end_date, ]

# Calculate the relevant statistics for the risky and risk-free asset
average_index_level <- mean(aligned_data$GSPC.Close, na.rm = TRUE)
average_bond_yield <- mean(aligned_data$DGS10, na.rm = TRUE)
average_index_return <- mean(aligned_data_mod$Index_Return, na.rm = TRUE)*252

volatility_index_level <- sd(aligned_data$GSPC.Close, na.rm = TRUE)
volatility_bond_yield <- sd(aligned_data$DGS10, na.rm = TRUE)
volatility_index_return <- sd(aligned_data_mod$Index_Return, na.rm = TRUE)*sqrt(252)


# Print the averages
print(paste("Average Index level for last 10 years:", average_index_level))
print(paste("Average Bond Yield for last 10 years:", average_bond_yield))


#Advanced statistical analysis of the time series behavior
plot_density(aligned_data_mod)
plot_qq(aligned_data_mod)


# Fit GARCH model on S&P 500 returns to assess volatility
# Assuming 'arith_fin_returns' is already loaded and contains 'SPX Index' returns
sp500_returns <- na.omit(aligned_data_mod$Index_Return)

# Define the GARCH(1,1) model specification
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                         distribution.model = "norm")

# Fit the GARCH model
garch_fit <- ugarchfit(spec = garch_spec, data = sp500_returns)

# Summary of GARCH model
summary(garch_fit)

# Extract the conditional standard deviation (volatility) from the fitted GARCH model
fitted_volatility <- sigma(garch_fit)

# Convert the actual returns and fitted volatility to a data frame for plotting
plot_data_sp500 <- data.frame(Date = index(sp500_returns), 
                              actual_returns_sp500 = as.numeric(sp500_returns), 
                              fitted_volatility_sp500 = as.numeric(fitted_volatility))

# Create the plot with actual returns and fitted volatility
ggplot(plot_data_sp500, aes(x = Date)) +
  geom_line(aes(y = actual_returns_sp500, colour = "Actual Returns")) +
  geom_line(aes(y = fitted_volatility_sp500, colour = "Fitted Volatility")) +
  scale_colour_manual("", 
                      breaks = c("Actual Returns", "Fitted Volatility"),
                      values = c("blue", "red")) +
  labs(y = "Value", title = "S&P 500 Actual Returns and Fitted GARCH") +
  theme_minimal()

# # Fit GARCH model on US Treasury 10Y bond yield returns to assess volatility
# # Assuming 'arith_fin_returns' is already loaded and contains 'SPX Index' returns
# US10_yields <- na.omit(aligned_data_mod$Bond_Yield)
# 
# # Define the GARCH(1,1) model specification
# garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
#                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
#                          distribution.model = "norm")
# 
# # Fit the GARCH model
# garch_fit <- ugarchfit(spec = garch_spec, data = US10_yields)
# 
# # Summary of GARCH model
# summary(garch_fit)
# 
# # Extract the conditional standard deviation (volatility) from the fitted GARCH model
# fitted_volatility <- sigma(garch_fit)
# 
# # Convert the actual returns and fitted volatility to a data frame for plotting
# plot_US10_yields <- data.frame(Date = index(US10_yields), 
#                               actual_US10_yields = as.numeric(US10_yields), 
#                               fitted_US10_yields = as.numeric(fitted_volatility))
# 
# # Create the plot with actual returns and fitted volatility
# ggplot(plot_US10_yields, aes(x = Date)) +
#   geom_line(aes(y = actual_US10_yields, colour = "Actual Returns")) +
#   geom_line(aes(y = fitted_US10_yields, colour = "Fitted Volatility")) +
#   scale_colour_manual("", 
#                       breaks = c("Actual Returns", "Fitted Volatility"),
#                       values = c("blue", "red")) +
#   labs(y = "Value", title = "10Y US Treasury yields Actual Returns and Fitted GARCH") +
#   theme_minimal()

------------------------------------------------------------------------------------------------------------------------------------------------------------
#NB: Make sure to run the code by block and not run the entire code as it can lead to problems in execution. Each block is marked with an '------' separation.
  
  
### 1.Implementation and comparison of CPPI under Black Scholes and jump diffusion model (Levy process)

# Run each part of the RStudio script independently to avoid errors 
# Function to simulate Black-Scholes and Lévy Jump Diffusion Models
simulate_models <- function(S0, T, r, sigma, lambda, k, delta, floor, multiplier, steps) {
  # Black-Scholes Simulation
  black_scholes_simulation <- function() {
    dt <- T / steps
    drift <- (r - 0.5 * sigma^2) * dt
    diffusion <- sigma * sqrt(dt)
    S <- numeric(steps)
    S[1] <- S0
    for (i in 2:steps) {
      S[i] <- S[i-1] * exp(drift + diffusion * rnorm(1))
    }
    return(S)
  }
  
  # Lévy Jump Diffusion Simulation
  levy_jump_diffusion_simulation <- function() {
    S <- black_scholes_simulation()
    for (i in 2:steps) {
      if (runif(1) < lambda) {
        S[i] <- S[i] * exp(k + delta * rnorm(1))
      }
    }
    return(S)
  }
  
  # CPPI Strategy Function
  cppi_simulation <- function(S) {
    cushion <- pmax(S - floor, 0)
    exposure <- multiplier * cushion
    cash <- S - exposure
    portfolio <- exposure * S + cash
    return(portfolio)
  }
  
  # Simulate both models
  bs_prices <- black_scholes_simulation()
  levy_prices <- levy_jump_diffusion_simulation()
  
  # Apply CPPI Strategy
  bs_portfolio <- cppi_simulation(bs_prices)
  levy_portfolio <- cppi_simulation(levy_prices)
  
  return(list(bs_portfolio = bs_portfolio, levy_portfolio = levy_portfolio))
}


# Parameters (these are example values, adjust as needed)
#When optimizing parameters for a CPPI strategy in equity modeling, it's important to set realistic values that reflect the current market conditions and the investor's risk profile. Here's an overview of what each parameter represents and how you might adjust it:

# S0: The initial index price. This should be set to the current price of the equity index or index you are modeling.
# T: The time horizon of the investment in years. For a typical equity investment, you could use a horizon of 1 year as you have, or extend it to match the investment plan (e.g., 5 years, 10 years, etc.).
# r: The risk-free interest rate. This should reflect the current yield on government securities with a maturity that matches your time horizon T. As of my last update in April 2023, you might want to check the yield on U.S. Treasury securities for the latest rate.
# sigma: The volatility (standard deviation) of the index's returns. This is typically estimated from historical data. For large equity indices, an annualized volatility in the range of 15% to 30% could be realistic, depending on the market.
# lambda: The intensity of the jump process (for Lévy processes). This represents the average number of jumps per year and can be estimated from historical data or market expectations.
# k: The average logarithmic jump size. If k is negative, it means that jumps are typically downwards, which is often realistic for equity markets (reflecting the market adage that "indexs take the stairs up and the elevator down").
# delta: The standard deviation of the jump size. This should be estimated from historical data about market jumps or fitted from a model that captures the tail risk.
# multiplier: The CPPI multiplier, which determines how aggressively the portfolio responds to changes in the cushion (the difference between the asset value and the floor). A common range might be 3 to 6, with higher values indicating a more aggressive strategy.
# floor: The protection level of the CPPI strategy, typically set as a percentage of the initial investment. The choice of floor depends on the investor's risk tolerance. A common floor might be 80% to 95% of S0.
# steps: The number of time steps in the simulation, which typically corresponds to trading days in a year. For daily modeling, 252 is a standard number representing the average number of trading days in a year.

# Parameters (adjusted for a more realistic scenario)
S0 <- as.numeric(aligned_data$GSPC.Close[2518])  # Current price of the equity index
T <- 10  # Investment horizon in years
r <- average_bond_yield  # Current risk-free interest rate
sigma <- volatility_index_return  # Annualized volatility 
lambda <- lambda_index  # Intensity of the jump process 
k <- k_index  # Average log jump size
delta <- delta_index  # Jump size volatility
multiplier <- 4  # CPPI multiplier (4)
floor <- S0 * 0.8  # CPPI floor (80% of the initial investment)
steps <- 252  # Number of simulation steps (daily steps for 1 year)

---------------------------------------------------------------------------------------------------------------------------------------------------
# To compute empirical values for the parameters of a jump process (lambda, k, and delta) using a time series of a risky asset (like the S&P 500 index), you would typically perform a statistical analysis that involves identifying and quantifying the jumps within the asset’s price data. These parameters are part of a jump-diffusion model, which is a complex stochastic process used in financial mathematics.

# Here's a general outline of how you might approach this:
 
# Identify Jumps in the Price Data: This involves differentiating between the normal market fluctuations (captured by the continuous part of the model) and significant jumps.
# Estimate the Jump Intensity (lambda): This is the average number of jumps per year. You would count the number of identified jumps over the period and divide by the number of years to get an annualized figure.
# Estimate the Average Jump Size (k): This would be the average of the logarithmic returns on days identified as jumps.
# Estimate the Jump Size Volatility (delta): This is the standard deviation of the jump sizes.
# Your existing code does a great job of fetching and processing the data, but it doesn't include the identification and analysis of jumps. To add this functionality, you would need to:
   
# Analyze the daily returns and identify days with jumps (which might be defined as returns exceeding a certain threshold).
# Calculate the parameters based on these identified jumps.

----------------------------------------------------------------------------------------------------------------------------------------------------
#NB: Make sure to run the code by block and not run the entire code as it can lead to problems in execution. Each block is marked with an '------' separation.
  
# Run the simulation
simulation_results <- simulate_models(S0, T, r, sigma, lambda, k, delta, floor, multiplier, steps)
#summary of the simulation values
summary(simulation_results$bs_portfolio)
summary(simulation_results$levy_portfolio)


# Plotting for comparison between both modeling techniques
plot(simulation_results$bs_portfolio, 
     type = "l", 
     col = "blue", 
     main = "Black-Scholes vs Lévy Model",
     xlab = "Time Step", 
     ylab = "Portfolio Value",
     ylim = c(min(c(simulation_results$bs_portfolio, simulation_results$levy_portfolio)), 
              max(c(simulation_results$bs_portfolio, simulation_results$levy_portfolio)))
)

# Add lines for the Lévy model
lines(simulation_results$levy_portfolio, type = "l", col = "red")

# Add a legend
legend("topleft", 
       legend = c("Black-Scholes", "Lévy"), 
       col = c("blue", "red"), 
       lty = 1, 
       bty = "n", # No box around the legend
       cex = 0.8 # Legend text size
)


------------------------------------------------------------------------------------------------------------------------------------------------------------
#NB: Make sure to run the code by block and not run the entire code as it can lead to problems in execution. Each block is marked with an '------' separation.
  
### 2. Implementation and comparison of the CPPI and OBPI under Black-Scholes model framework with sensitivity analysis and Monte Carlo simulation


# Create a function to simulate a scenario using given parameters
simulate_scenario <- function(rf_scenario, n_sim = 10000, stress = 0.20, sigma = volatility_index_return, mu = average_index_return, strike = S0) {
  # Define constants
  delta <- 1 / 252
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
  
  # Simulate index returns
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
rf_values <- seq(average_bond_yield, 0.045, by = 0.01)
stress_values <- seq(0.15, 0.20, by = 0.01)
sigma_values <- seq(volatility_index_return, 0.2, by = 0.02)
mu_values <- seq(average_index_return, 0.2, by = 0.02)
strike_values <- seq(average_index_level, 3400, by = 200)

results_rf <- lapply(rf_values, function(rf_scenario) simulate_scenario(0.02, rf_scenario = rf_scenario))
results_stress <- lapply(stress_values, function(stress) simulate_scenario(0.02, stress))
results_sigma <- lapply(sigma_values, function(sigma) simulate_scenario(0.02, stress = 0.20, sigma = sigma))
results_mu <- lapply(mu_values, function(mu) simulate_scenario(0.02, stress = 0.20, mu = mu))
results_strike <- lapply(strike_values, function(strike) simulate_scenario(0.02, strike = strike))

plot_results <- function(results, title) {
  # Validate input
  required_names <- c("equity", "floor", "obpi_call", "cppi")
  for (name in required_names) {
    if (!is.null(results[[name]]) && length(results[[name]]) > 0) {
      next
    } else {
      stop(paste("Missing or empty component in 'results':", name))
    }
  }
  
  t <- seq_along(results$equity)
  
  # Convert the results into a data frame for plotting
  df <- data.frame(
    t = rep(t, length(required_names)),
    Value = unlist(lapply(required_names, function(name) results[[name]])),
    Type = factor(rep(required_names, each = length(t)))
  )
  
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

plot_results(results_rf[[2]], "Risk-free rate variation")
plot_results(results_stress[[2]], "Stress value variation")
plot_results(results_mu[[2]], "Return (mu) variation")
plot_results(results_strike[[2]], "Strike price variation")

#Alternative plotting technique 
# # Assuming you have a list of results and corresponding titles
# results_list <- list(results_rf, results_stress, results_sigma, results_mu, results_strike)
# titles_list <- c("Risk-free Rate Variation", "Stress Value Variation", "Sigma Variation", "Mu Variation", "Strike Value Variation")

# # Loop to plot all results
# lapply(seq_along(results_list), function(i) {
#   plot_results(results_list[[i]][[1]], titles_list[i])  # Assuming the first element of each list is the desired result
# })
# 
# lapply(seq_along(results_list), function(i) {
#   plot_results(results_list[[i]][[2]], titles_list[i])  # Assuming the first element of each list is the desired result
# })
# 
# lapply(seq_along(results_list), function(i) {
#   plot_results(results_list[[i]][[3]], titles_list[i])  # Assuming the first element of each list is the desired result
# })
# 
# lapply(seq_along(results_list), function(i) {
#   plot_results(results_list[[i]][[4]], titles_list[i])  # Assuming the first element of each list is the desired result
# })
# 
# lapply(seq_along(results_list), function(i) {
#   plot_results(results_list[[i]][[5]], titles_list[i])  # Assuming the first element of each list is the desired result
# })


------------------------------------------------------------------------------------------------------------------------------------
#NB: Make sure to run the code by block and not run the entire code as it can lead to problems in execution. Each block is marked with an '------' separation.
  
## Monte Carlo simulation for the OBPI and CPPI strategy relying on a MC simulation
  
  simulate_scenario <- function(rf_scenario, n_sim = 10000, stress = 0.20, sigma = volatility_index_return, mu = average_index_return, strike = S0) {
    delta <- 1 / 252  # Daily steps
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
      positive_equity <- pmax(equity, 0.01)
      d1 <- (log(positive_equity[-(n+1)] / strike) + (rf_scenario + sigma^2 / 2) * (mat - (0:(n-1)) * delta)) / (sigma * sqrt(mat - (0:(n-1)) * delta))
      d2 <- d1 - sigma * sqrt(mat - (0:(n-1)) * delta)
      
      # Ensure positive values for call options
      call <- positive_equity[-(n+1)] * pnorm(d1) - strike * exp(-rf_scenario * (mat - (0:(n-1)) * delta)) * pnorm(d2)
      
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

# Function to calculate return and risk
calculate_return_risk <- function(simulation_values) {
  # Return is the mean of the values
  expected_return <- mean(simulation_values)
  
  # Risk is the standard deviation of the values
  risk <- sd(simulation_values)
  
  return(list(expected_return = expected_return, risk = risk))
}

# Run the Monte Carlo simulation with specified parameters
set.seed(123)  # Setting seed for reproducibility
mc_results <- simulate_scenario(rf_scenario = average_bond_yield, n_sim = 10000)

initial_value <- S0 # The initial value of the asset goes here

calculate_return_risk <- function(simulation_values, initial_value) {
  # Calculate returns in percentage terms
  returns <- ((simulation_values - initial_value) / initial_value) * 100
  
  # Return is the mean of the percentage returns
  expected_return <- mean(returns)
  
  # Risk is the standard deviation of the percentage returns
  risk <- sd(returns)
  
  return(list(expected_return = expected_return, risk = risk))
}

# Assuming mc_results$average_obpi_call and mc_results$average_cppi represent the final asset values
# of the OBPI and CPPI strategies, respectively.

# Calculate return and risk for OBPI
obpi_result <- calculate_return_risk(mc_results$average_obpi_call, initial_value)
print(paste("OBPI return:", obpi_result$expected_return, "%"))
print(paste("OBPI risk (Standard Deviation):", obpi_result$risk, "%"))

# Calculate return and risk for CPPI
cppi_result <- calculate_return_risk(mc_results$average_cppi, initial_value)
print(paste("CPPI return:", cppi_result$expected_return, "%"))
print(paste("CPPI risk (Standard Deviation):", cppi_result$risk, "%"))

# Histogram for final equity values
hist(mc_results$average_equity, main="Histogram of Equity Values", xlab="Value", col="red", breaks=50)

# Histogram for final OBPI strategy values
hist(mc_results$average_obpi_call, main="Histogram of OBPI Strategy Values", xlab="Value", col="blue", breaks=50)

# Histogram for final CPPI strategy values
hist(mc_results$average_cppi, main="Histogram of CPPI Strategy Values", xlab="Value", col="green", breaks=50)
