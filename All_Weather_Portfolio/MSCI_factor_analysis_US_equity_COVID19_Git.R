#Packages 

library(tidyverse)
library(quantmod)
library(DataExplorer)
library(corrplot)
library(scales)
library(readxl)
library(xts)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tseries)
library(rugarch)
library(lmtest)
library(car)

# Load the data
data <- read_excel("...pathto/folder/Factor_investing_data.xlsx")

# Convert to time series format
data_xts <- xts(data[-1], order.by = as.Date(data$Date))

# Eliminated missing data in time series
data_xtsna <- na.omit(data_xts)  # Remove rows with missing values

# Preliminary Plot
autoplot(data_xtsna$`SPX Index`, title = "S&P 500 index price over time") + theme_minimal()
autoplot(data_xtsna$`VIX index`, title = "VIX index price over time") + theme_minimal()
autoplot(data_xtsna$`Size`, title = "MSCI Size price over time") + theme_minimal()
autoplot(data_xtsna$`Value`, title = "MSCI Value price over time") + theme_minimal()
autoplot(data_xtsna$`Quality`, title = "MSCI Quality price over time") + theme_minimal()
autoplot(data_xtsna$`Momentum`, title = "MSCI Momentum price over time") + theme_minimal()
autoplot(data_xtsna$`Minimum Volatility`, title = "MSCI Minimum Volatility price over time") + theme_minimal()

#return analysis of each factors
arith_fin_returns = diff(data_xtsna)/lag(data_xtsna)
head(arith_fin_returns, n=3)
arith_fin_returns <- arith_fin_returns[-1, ]

plot_intro(arith_fin_returns) #plotting retunrs and assess stationarity in the data

# Remove VIX Index from return analysis
arith_fin_returns_without_vix <- arith_fin_returns[, !colnames(arith_fin_returns) %in% c("VIX index")]

# Calculate cumulative returns excluding VIX Index
cumulative_returns <- cumprod(1 + arith_fin_returns_without_vix)

# Plot Cumulative Return Performance excluding VIX Index
autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of the investment universe (Excluding VIX Index)") +
  ylab("Cumulative Return") +
  xlab("Year")

#correlation analysis
corrplot(cor(arith_fin_returns), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

# Basic statistics for each factors
summary(arith_fin_returns)
plot_density(arith_fin_returns)
plot_qq(arith_fin_returns)



###Subsampling by periods to assess each period and its impact on factor performance
# Define the number of rows for each subsample based on the given dates
num_rows_origins <- which(index(arith_fin_returns) == ymd("2020-12-31"))
num_rows_incubation <- which(index(arith_fin_returns) == ymd("2020-01-17"))
num_rows_outbreak <- which(index(arith_fin_returns) == ymd("2020-02-21"))
num_rows_fever <- which(index(arith_fin_returns) == ymd("2020-03-20"))
num_rows_treatment_end <- which(index(arith_fin_returns) == ymd("2020-04-15"))

# Extract the subsamples based on the defined row numbers
subsample_origins <- arith_fin_returns[1:num_rows_origins, ]
subsample_incubation <- arith_fin_returns[(num_rows_origins + 1):num_rows_incubation, ]
subsample_outbreak <- arith_fin_returns[(num_rows_incubation + 1):num_rows_outbreak, ]
subsample_fever <- arith_fin_returns[(num_rows_outbreak + 1):num_rows_fever, ]
subsample_treatment <- arith_fin_returns[(num_rows_fever + 1):num_rows_treatment_end, ]

#data check for each subsampling 
plot_intro(subsample_origins)
plot_intro(subsample_incubation)
plot_intro(subsample_outbreak)
plot_intro(subsample_fever)
plot_intro(subsample_treatment)


#We perform the plot analysis for each subsample
cumulative_returns_origins <- cumprod(1 + subsample_origins)
autoplot(cumulative_returns_origins, facets = NULL) +
  ggtitle("Cumulative Return Performance during the origins of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

cumulative_returns_origins <- cumprod(1 + subsample_incubation)
autoplot(cumulative_returns_origins, facets = NULL) +
  ggtitle("Cumulative Return Performance during the incubation of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

cumulative_returns_origins <- cumprod(1 + subsample_outbreak)
autoplot(cumulative_returns_origins, facets = NULL) +
  ggtitle("Cumulative Return Performance during the outbreak of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

cumulative_returns_origins <- cumprod(1 + subsample_fever)
autoplot(cumulative_returns_origins, facets = NULL) +
  ggtitle("Cumulative Return Performance during the fever period of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

cumulative_returns_origins <- cumprod(1 + subsample_treatment)
autoplot(cumulative_returns_origins, facets = NULL) +
  ggtitle("Cumulative Return Performance during the treatment period of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

#correlation matrix
cor(subsample_origins)
cor(subsample_incubation)
cor(subsample_outbreak)
cor(subsample_fever)
cor(subsample_treatment)

#on correlation impact across subsamples
corrplot(cor(subsample_origins), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

corrplot(cor(subsample_incubation), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

corrplot(cor(subsample_outbreak), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

corrplot(cor(subsample_fever), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

corrplot(cor(subsample_treatment), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)



# Load necessary libraries
# Install necessary packages if not already installed
# if (!require("tidyverse")) install.packages("tidyverse")
# if (!require("quantmod")) install.packages("quantmod")
# if (!require("tseries")) install.packages("tseries")
# if (!require("rugarch")) install.packages("rugarch")
# if (!require("lmtest")) install.packages("lmtest")
# if (!require("car")) install.packages("car")
# if (!require("readxl")) install.packages("readxl")
# if (!require("xts")) install.packages("xts")
# if (!require("lubridate")) install.packages("lubridate")
# if (!require("ggplot2")) install.packages("ggplot2")


# Load necessary library for GARCH modeling
library(rugarch)

# Fit GARCH model on S&P 500 returns to assess volatility
# Assuming 'arith_fin_returns' is already loaded and contains 'SPX Index' returns
sp500_returns <- na.omit(arith_fin_returns$`SPX Index`)

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



# Fit GARCH model on MSCI Size returns to assess volatility
# Assuming 'arith_fin_returns' is already loaded and contains 'SPX Index' returns
size_returns <- na.omit(arith_fin_returns$`Size`)

# Define the GARCH(1,1) model specification
garch_spec_size <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                         distribution.model = "norm")

# Fit the GARCH model
garch_fit_size <- ugarchfit(spec = garch_spec_size, data = size_returns)

# Summary of GARCH model
summary(garch_fit_size)

# Extract the conditional standard deviation (volatility) from the fitted GARCH model
fitted_volatility_size <- sigma(garch_fit_size)

# Convert the actual returns and fitted volatility to a data frame for plotting
plot_data_size <- data.frame(Date = index(size_returns), 
                        actual_returns_size = as.numeric(size_returns), 
                        fitted_volatility_size = as.numeric(fitted_volatility_size))

# Create the plot with actual returns and fitted volatility
ggplot(plot_data_size, aes(x = Date)) +
  geom_line(aes(y = actual_returns_size, colour = "Actual Returns")) +
  geom_line(aes(y = fitted_volatility_size, colour = "Fitted Volatility")) +
  scale_colour_manual("", 
                      breaks = c("Actual Returns", "Fitted Volatility"),
                      values = c("blue", "red")) +
  labs(y = "Value", title = "MSCI Size actual returns and fitted GARCH") +
  theme_minimal()


# Fit GARCH model on MSCI Quality returns to assess volatility
# Assuming 'arith_fin_returns' is already loaded and contains 'SPX Index' returns
quality_returns <- na.omit(arith_fin_returns$`Quality`)

# Define the GARCH(1,1) model specification
garch_spec_quality <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                              mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                              distribution.model = "norm")

# Fit the GARCH model
garch_fit_quality <- ugarchfit(spec = garch_spec_quality, data = quality_returns)

# Summary of GARCH model
summary(garch_fit_quality)

# Extract the conditional standard deviation (volatility) from the fitted GARCH model
fitted_volatility_quality <- sigma(garch_fit_quality)

# Convert the actual returns and fitted volatility to a data frame for plotting
plot_data_quality <- data.frame(Date = index(size_returns), 
                        actual_returns_quality = as.numeric(quality_returns), 
                        fitted_volatility_quality = as.numeric(fitted_volatility_quality))

# Create the plot with actual returns and fitted volatility
ggplot(plot_data_quality, aes(x = Date)) +
  geom_line(aes(y = actual_returns_quality, colour = "Actual Returns")) +
  geom_line(aes(y = fitted_volatility_quality, colour = "Fitted Volatility")) +
  scale_colour_manual("", 
                      breaks = c("Actual Returns", "Fitted Volatility"),
                      values = c("blue", "red")) +
  labs(y = "Value", title = "MSCI Quality actual returns and fitted GARCH") +
  theme_minimal()



# Fit GARCH model on MSCI Value returns to assess volatility
# Assuming 'arith_fin_returns' is already loaded and contains 'SPX Index' returns
value_returns <- na.omit(arith_fin_returns$`Value`)

# Define the GARCH(1,1) model specification
garch_spec_value <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                                 mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                                 distribution.model = "norm")

# Fit the GARCH model
garch_fit_value <- ugarchfit(spec = garch_spec_value, data = value_returns)

# Summary of GARCH model
summary(garch_fit_value)

# Extract the conditional standard deviation (volatility) from the fitted GARCH model
fitted_volatility_value <- sigma(garch_fit_value)

# Convert the actual returns and fitted volatility to a data frame for plotting
plot_data_value <- data.frame(Date = index(size_returns), 
                        actual_returns_value = as.numeric(value_returns), 
                        fitted_volatility_value = as.numeric(fitted_volatility_value))

# Create the plot with actual returns and fitted volatility
ggplot(plot_data_value, aes(x = Date)) +
  geom_line(aes(y = actual_returns_value, colour = "Actual Returns")) +
  geom_line(aes(y = fitted_volatility_value, colour = "Fitted Volatility")) +
  scale_colour_manual("", 
                      breaks = c("Actual Returns", "Fitted Volatility"),
                      values = c("blue", "red")) +
  labs(y = "Value", title = "MSCI Value actual returns and fitted GARCH") +
  theme_minimal()



# Fit GARCH model on MSCI Minimum Volatility returns to assess volatility
# Assuming 'arith_fin_returns' is already loaded and contains 'SPX Index' returns
minvol_returns <- na.omit(arith_fin_returns$`Minimum Volatility`)

# Define the GARCH(1,1) model specification
garch_spec_minvol <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                               mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                               distribution.model = "norm")

# Fit the GARCH model
garch_fit_minvol <- ugarchfit(spec = garch_spec_minvol, data = minvol_returns)

# Summary of GARCH model
summary(garch_fit_minvol)

# Extract the conditional standard deviation (volatility) from the fitted GARCH model
fitted_volatility_minvol <- sigma(garch_fit_minvol)

# Convert the actual returns and fitted volatility to a data frame for plotting
plot_data_minvol <- data.frame(Date = index(size_returns), 
                        actual_returns_minvol = as.numeric(minvol_returns), 
                        fitted_volatility_minvol = as.numeric(fitted_volatility_minvol))

# Create the plot with actual returns and fitted volatility
ggplot(plot_data_minvol, aes(x = Date)) +
  geom_line(aes(y = actual_returns_minvol, colour = "Actual Returns")) +
  geom_line(aes(y = fitted_volatility_minvol, colour = "Fitted Volatility")) +
  scale_colour_manual("", 
                      breaks = c("Actual Returns", "Fitted Volatility"),
                      values = c("blue", "red")) +
  labs(y = "Value", title = "MSCI MinVol actual returns and fitted GARCH") +
  theme_minimal()

# Fit GARCH model on MSCI momentum returns to assess volatility
# Assuming 'arith_fin_returns' is already loaded and contains 'SPX Index' returns
momentum_returns <- na.omit(arith_fin_returns$`Momentum`)

# Define the GARCH(1,1) model specification
garch_spec_momentum <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                                  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                                  distribution.model = "norm")

# Fit the GARCH model
garch_fit_momentum <- ugarchfit(spec = garch_spec_momentum, data = momentum_returns)

# Summary of GARCH model
summary(garch_fit_momentum)

# Extract the conditional standard deviation (volatility) from the fitted GARCH model
fitted_volatility_momentum <- sigma(garch_fit_momentum)

# Convert the actual returns and fitted volatility to a data frame for plotting
plot_data_momentum <- data.frame(Date = index(size_returns), 
                                 actual_returns_momentum = as.numeric(momentum_returns), 
                                 fitted_volatility_momentum = as.numeric(fitted_volatility_momentum))

# Create the plot with actual returns and fitted volatility
ggplot(plot_data_momentum, aes(x = Date)) +
  geom_line(aes(y = actual_returns_momentum, colour = "Actual Returns")) +
  geom_line(aes(y = fitted_volatility_momentum, colour = "Fitted Volatility")) +
  scale_colour_manual("", 
                      breaks = c("Actual Returns", "Fitted Volatility"),
                      values = c("blue", "red")) +
  labs(y = "Value", title = "MSCI Momentum actual returns and fitted GARCH") +
  theme_minimal()



###Multiple regression for each factor

# Load the necessary libraries

library(broom)
library(lmtest)
library(sandwich)
library(car)
library(xts)
library(lubridate)

# Assuming 'arith_fin_returns' is loaded and is an xts object with factor returns and VIX, S&P 500 indices

# Convert xts object to a data frame for regression
df_returns <- fortify.zoo(arith_fin_returns)

# Create a binary variable for pre and post COVID-19 periods
# For simplicity, let's assume the post COVID-19 period starts from February 2020
df_returns$COVID_Impact <- if_else(df_returns$Index >= ymd("2020-03-11"), 1, 0)

# Fit a multiple linear regression to assess the impact of COVID-19 on equity factors
# Using 'Value' as an example factor; repeat for 'Size', 'Quality', 'Momentum', 'Minimum Volatility'
fit_value <- lm(Value ~ `SPX Index` + `VIX index` + COVID_Impact, data = df_returns)
summary(fit_value)

fit_size <- lm(Size ~ `SPX Index` + `VIX index` + COVID_Impact, data = df_returns)
summary(fit_size)

fit_quality <- lm(Quality ~ `SPX Index` + `VIX index` + COVID_Impact, data = df_returns)
summary(fit_quality)

fit_momentum <- lm(Momentum ~ `SPX Index` + `VIX index` + COVID_Impact, data = df_returns)
summary(fit_momentum)

fit_minvol <- lm(`Minimum Volatility` ~ `SPX Index` + `VIX index` + COVID_Impact, data = df_returns)
summary(fit_minvol)


# Check for autocorrelation in residuals using Durbin-Watson test
durbinWatsonTest(fit_value)
durbinWatsonTest(fit_size)
durbinWatsonTest(fit_quality)
durbinWatsonTest(fit_momentum)
durbinWatsonTest(fit_minvol)

# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(fit_value) # VIF values greater than 5 or 10 indicate high multicollinearity
vif(fit_size)
vif(fit_quality)
vif(fit_momentum)
vif(fit_minvol)


###Multiple linear regression
# Perform regression for each period
# Define the start and end dates for each period
periods <- list(
  origins = c(ymd("2019-11-01"), ymd("2020-12-31")),
  incubation = c(ymd("2020-01-02"), ymd("2020-01-17")),
  outbreak = c(ymd("2020-01-20"), ymd("2020-02-21")),
  fever = c(ymd("2020-02-24"), ymd("2020-03-20")),
  treatment = c(ymd("2020-03-23"), ymd("2020-04-15"))
)

# Function to run regression for each period
run_regression <- function(start_date, end_date, data, factor) {
  period_data <- data %>% filter(Index >= start_date & Index <= end_date)
  lm(reformulate(c("`SPX Index`", "`VIX index`"), response = factor), data = period_data)
}

# Apply the regression analysis for each period for each factor
period_results_value <- map(periods, ~{
  run_regression(.x[1], .x[2], df_returns, "Value")
})

period_results_size <- map(periods, ~{
  run_regression(.x[1], .x[2], df_returns, "Size")
})

period_results_quality <- map(periods, ~{
  run_regression(.x[1], .x[2], df_returns, "Quality")
})

period_results_momentum <- map(periods, ~{
  run_regression(.x[1], .x[2], df_returns, "Momentum")
})

period_results_minvol <- map(periods, ~{
  run_regression(.x[1], .x[2], df_returns, "Minimum Volatility")
})


###Statistical analysis of each period results for each factor 
# Check the results for each period
map(period_results_value, tidy)
map(period_results_size, tidy)
map(period_results_quality, tidy)
map(period_results_momentum, tidy)
map(period_results_minvol, tidy)


# Assuming 'fit_value', 'fit_size', 'fit_quality', 'fit_momentum', 'fit_minvol' are your fitted models

# Check model summary
summary(fit_value)
summary(fit_size)
summary(fit_quality)
summary(fit_momentum)
summary(fit_minvol)


# Residual diagnostics
# Autocorrelation
durbinWatsonTest(fit_value)
durbinWatsonTest(fit_size)
durbinWatsonTest(fit_quality)
durbinWatsonTest(fit_momentum)
durbinWatsonTest(fit_minvol)


# Multicollinearity
vif(fit_value)
vif(fit_size)
vif(fit_quality)
vif(fit_momentum)
vif(fit_minvol)

# Normality of residuals
qqnorm(residuals(fit_value))
qqline(residuals(fit_value))

qqnorm(residuals(fit_size))
qqline(residuals(fit_size))

qqnorm(residuals(fit_quality))
qqline(residuals(fit_quality))

qqnorm(residuals(fit_momentum))
qqline(residuals(fit_momentum))

qqnorm(residuals(fit_minvol))
qqline(residuals(fit_minvol))


# Homoscedasticity
plot(fitted(fit_value), residuals(fit_value))
abline(h = 0, col = "red")

plot(fitted(fit_size), residuals(fit_size))
abline(h = 0, col = "red")

plot(fitted(fit_quality), residuals(fit_quality))
abline(h = 0, col = "red")

plot(fitted(fit_momentum), residuals(fit_momentum))
abline(h = 0, col = "red")

plot(fitted(fit_minvol), residuals(fit_minvol))
abline(h = 0, col = "red")


# Subsample analysis
# Assuming 'period_results_value', 'period_results_size', ... contain regression results for each period
map(period_results_value, summary)
map(period_results_size, summary)
map(period_results_quality, summary)
map(period_results_momentum, summary)
map(period_results_minvol, summary)




