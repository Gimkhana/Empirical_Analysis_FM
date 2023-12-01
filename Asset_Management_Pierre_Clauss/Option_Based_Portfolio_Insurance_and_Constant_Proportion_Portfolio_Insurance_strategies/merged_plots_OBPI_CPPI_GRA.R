# Packages
library(tidyverse)
library(scales)

# Initial variables
mu <- 0.05
sigma <- 0.20
delta <- 1 / 12
mat <- 1
strike <- 100
tol <- 0
set.seed(1234)
n <- mat / delta

# Sensitivity values
rf_values <- c(0.005, 0.015, 0.03)
stress_values <- c(0.10, 0.15, 0.25)

# Initialize an empty dataframe to store all results
all_data <- data.frame()

# For each combination of rf and stress
for (current_rf in rf_values) {
  for (current_stress in stress_values) {
    
    # Update the rf and stress values
    rf <- current_rf
    stress <- current_stress
    
    # Simulation with the current rf and stress values
    u <- runif(n)
    rf1 <- (1 + rf) ^ delta - 1
    mu1 <- (1 + mu) ^ delta - 1
    sigma1 <- sigma * sqrt(delta)
    r <- qnorm(u, mu1, sigma1)
    rPRN <- qnorm(u, rf1, sigma1)
    
    equity <- numeric(n + 1)
    equityRNP <- numeric(n + 1)
    equity[1] <- strike
    equityRNP[1] <- strike
    
    for (i in 1:n) {
      equity[i + 1] <- equity[i] * (1 + r[i])
      equityRNP[i + 1] <- equityRNP[i] * (1 + rPRN[i])
    }
    
    call <- numeric(n + 1)
    call[n + 1] <- max(0, equityRNP[n + 1] - strike)
    for (i in 1:n) {
      d0 <- ((rf - sigma ^ 2 / 2) * (mat - (i - 1) * delta) + log(equityRNP[i] / strike)) / (sigma * sqrt(mat - (i - 1) * delta))
      d1 <- d0 + sigma * sqrt(mat - (i - 1) * delta)
      call[i] <- equityRNP[i] * pnorm(d1) - strike * exp(-rf * (mat - (i - 1) * delta)) * pnorm(d0)
    }
    
    floor <- numeric(n + 1)
    for (i in 1:(n + 1)) {
      floor[i] <- strike / (1 + rf) ^ (mat - (i - 1) * delta)
    }
    
    obpi_call <- numeric(n + 1)
    obpi_call[1] <- strike
    gearing <- (obpi_call[1] - floor[1]) / call[1]
    for (i in 1:n) {
      obpi_call[i + 1] <- floor[i + 1] + gearing * call[i + 1]
    }
    
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
      expo_expost[i + 1] <- ifelse(multiplier[i + 1] < boundary_inf || multiplier[i + 1] > boundary_sup, multiplier[1], multiplier[i + 1]) * ifelse(cushion[i + 1] < 0, 0, cushion[i + 1])
    }
    
    # Create a data frame for current simulation
    current_data <- data.frame(
      Month = 1:(n + 1),
      Equity = equity,
      Floor = floor,
      OBPI = obpi_call,
      CPPI = cppi,
      rf = current_rf,
      stress = current_stress
    )
    
    # Bind to the master data frame
    all_data <- rbind(all_data, current_data)
  }
}

# Melt data for ggplot
melted_data <- all_data %>%
  gather(key = "Strategy", value = "Value", -Month, -rf, -stress)

# Plotting with subplots
ggplot(melted_data, aes(x = Month, y = Value, color = Strategy)) +
  geom_line() +
  facet_grid(rf ~ stress, scales = "free", labeller = label_both) +
  labs(y = "Strategy Value", x = "Months", title = "Sensitivity Analysis for rf and stress") +
  theme_minimal() +
  scale_color_manual(values = c("black", "blue", "green", "red")) +
  theme(legend.position = "bottom")
