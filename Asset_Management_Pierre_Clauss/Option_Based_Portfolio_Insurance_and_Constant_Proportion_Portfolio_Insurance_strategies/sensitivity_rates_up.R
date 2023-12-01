##Scenario 1: Increase in interest rates 


# Initial variables
mu <- 0.05
sigma <- 0.20

# Simulation methodology
delta <- 1 / 12
mat <- 1
rf_1 <- 0.055
strike <- 100
stress <- 0.20
tol <- 0

set.seed(1234)

n <- mat / delta
u <- runif(n)

# Moments on simulation path
rf1 <- (1 + rf_1) ^ delta - 1
mu1 <- (1 + mu) ^ delta - 1
sigma1 <- sigma * sqrt(delta)

# Stock simulation
r <- qnorm(u, mu1, sigma1)
rPRN <- qnorm(u, rf1, sigma1)

equity <- numeric(n + 1)
equityRNP <- numeric(n + 1)
equity[1] <- strike
equityRNP[1] <- strike

for (i in 1:n)
{
  equity[i + 1] <- equity[i] * (1 + r[i])
  equityRNP[i + 1] <- equityRNP[i] * (1 + rPRN[i])
}

# Call simulation
call <- numeric(n + 1)
call[n + 1] <- max(0, equityRNP[n + 1] - strike)

for (i in 1:n)
{
  d0 <-
    ((rf_1 - sigma ^ 2 / 2) * (mat - (i - 1) * delta) + log(equityRNP[i] / strike)) /
    (sigma * sqrt(mat - (i - 1) * delta))
  d1 <- d0 + sigma * sqrt(mat - (i - 1) * delta)
  call[i] <-
    equityRNP[i] * pnorm(d1) - strike * exp(-rf_1 * (mat - (i - 1) * delta)) *
    pnorm(d0)
}

# Floor simulation
floor <- numeric(n + 1)

for (i in 1:(n + 1))
{
  floor[i] <- strike / (1 + rf_1) ^ (mat - (i - 1) * delta)
}

# OBPI simulation
obpi_call <- numeric(n + 1)
obpi_call[1] <- strike
gearing <- (obpi_call[1] - floor[1]) / call[1]

for (i in 1:n)
{
  obpi_call[i + 1] <- floor[i + 1] + gearing * call[i + 1]
}

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

for (i in 1:n)
{
  expo_exante[i + 1] <- expo_expost[i] * equity[i + 1] / equity[i]
  cppi[i + 1] <-
    expo_exante[i + 1] + (cppi[i] - expo_expost[i]) * floor[i + 1] / floor[i]
  cushion[i + 1] <- cppi[i + 1] - floor[i + 1]
  multiplier[i + 1] <- expo_exante[i + 1] / cushion[i + 1]
  expo_expost[i + 1] <-
    ifelse(
      multiplier[i + 1] < boundary_inf ||
        multiplier[i + 1] > boundary_sup,
      multiplier[1],
      multiplier[i + 1]
    ) * ifelse(cushion[i + 1] < 0, 0, cushion[i + 1])
}

plot(equity, type = 'l', ylab = 'Strategies values', xlab = 'Months', ylim = c(70, 130))
lines(floor, lty = 2)
lines(obpi_call, col = 'blue')
lines(cppi, col = 'green')
leg.txt <- c("Equity", "Floor", "OBPI", "CPPI")
legend(1, 130, leg.txt, col = c("black", "black", "blue", "green"), lty = c(1, 2, 1, 1))
