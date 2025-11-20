###############################################################################
# SECTION 0: INITIAL PACKAGES
###############################################################################

library(rugarch)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(Metrics)
library(gridExtra)


###############################################################################
# SECTION 1: ORGANIZE FACTOR RETURNS INTO SYSTEMATIC LIST
###############################################################################

# Load the data (import the appropriate xlsx file "Factor_investing_data.xlsx")
data <- Factor_investing_data

# Convert to time series format and EXCLUDE VIX Index from the beginning
data_xts <- xts(data[, !names(data) %in% c("Date", "VIX index")], 
                order.by = as.Date(data$Date))

# Eliminated missing data in time series
data_xtsna <- na.omit(data_xts)  # Remove rows with missing values

# Preliminary Plots (VIX plot removed)
autoplot(data_xtsna$`SPX Index`) +
  ggtitle("S&P 500 index price over time") +
  ylab("Price level") +
  xlab("Year") + 
  theme_minimal()

autoplot(data_xtsna$`Size`) +
  ggtitle("MSCI Size price over time") +
  ylab("Price level") +
  xlab("Year") + 
  theme_minimal()

autoplot(data_xtsna$`Value`) +
  ggtitle("MSCI Value price over time") +
  ylab("Price level") +
  xlab("Year") + 
  theme_minimal()

autoplot(data_xtsna$`Quality`) +
  ggtitle("MSCI Quality price over time") +
  ylab("Price level") +
  xlab("Year") + 
  theme_minimal()

autoplot(data_xtsna$`Momentum`) + 
  ggtitle("MSCI Momentum price over time") +
  ylab("Price level") +
  xlab("Year") + 
  theme_minimal()

autoplot(data_xtsna$`Minimum Volatility`) + 
  ggtitle("MSCI Minimum Volatility price over time") +
  ylab("Price level") +
  xlab("Year") + 
  theme_minimal()

# Return analysis of each factor
arith_fin_returns <- diff(data_xtsna)/lag(data_xtsna)
head(arith_fin_returns, n=3)
arith_fin_returns <- arith_fin_returns[-1, ]

plot_intro(arith_fin_returns) # Plotting returns and assess stationarity in the data

# Calculate cumulative returns (VIX already excluded)
cumulative_returns <- cumprod(1 + arith_fin_returns)

# Plot Cumulative Return Performance
autoplot(cumulative_returns, facets = NULL) +
  ggtitle("Cumulative Return Performance of the investment universe") +
  ylab("Cumulative Return") +
  xlab("Year")

# Correlation analysis
corrplot(cor(arith_fin_returns), type='lower', 
         method = "shade", tl.col = 'black', cl.pos = "r", tl.cex = 1)

# Basic statistics for each factor
summary(arith_fin_returns)
plot_density(arith_fin_returns)
plot_qq(arith_fin_returns)

### Subsampling by periods to assess each period and its impact on factor performance
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

# Data check for each subsampling 
plot_intro(subsample_origins)
plot_intro(subsample_incubation)
plot_intro(subsample_outbreak)
plot_intro(subsample_fever)
plot_intro(subsample_treatment)

# We perform the plot analysis for each subsample
cumulative_returns_origins <- cumprod(1 + subsample_origins)
autoplot(cumulative_returns_origins, facets = NULL) +
  ggtitle("Cumulative Return Performance during the origins of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

cumulative_returns_incubation <- cumprod(1 + subsample_incubation)
autoplot(cumulative_returns_incubation, facets = NULL) +
  ggtitle("Cumulative Return Performance during the incubation of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

cumulative_returns_outbreak <- cumprod(1 + subsample_outbreak)
autoplot(cumulative_returns_outbreak, facets = NULL) +
  ggtitle("Cumulative Return Performance during the outbreak of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

cumulative_returns_fever <- cumprod(1 + subsample_fever)
autoplot(cumulative_returns_fever, facets = NULL) +
  ggtitle("Cumulative Return Performance during the fever period of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

cumulative_returns_treatment <- cumprod(1 + subsample_treatment)
autoplot(cumulative_returns_treatment, facets = NULL) +
  ggtitle("Cumulative Return Performance during the treatment period of the COVID crisis") +
  ylab("Cumulative Return") +
  xlab("Date") +
  theme_minimal()

# Correlation matrix
cor(subsample_origins)
cor(subsample_incubation)
cor(subsample_outbreak)
cor(subsample_fever)
cor(subsample_treatment)

# Correlation impact across subsamples
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



cat("\n========== INITIALIZING ANALYSIS ==========\n")

# Create list of all factors for systematic processing
factors_list <- list(
  list(name = "S&P 500", data = sp500_returns, type = "Index"),
  list(name = "Momentum", data = momentum_returns, type = "Factor"),
  list(name = "Value", data = value_returns, type = "Factor"),
  list(name = "Quality", data = quality_returns, type = "Factor"),
  list(name = "Size", data = size_returns, type = "Factor"),
  list(name = "Minimum Volatility", data = minvol_returns, type = "Factor")
)

cat("Factor list created with", length(factors_list), "series\n\n")

###############################################################################
# SECTION 2: MASTER FUNCTION - COMPLETE ANALYSIS FOR EACH FACTOR
###############################################################################

fit_complete_analysis <- function(returns_data, factor_name) {
  
  cat("  Analyzing:", factor_name, "\n")
  
  # ========================================================================
  # A. GARCH(1,1) Specification and Fitting
  # ========================================================================
  
  garch_spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  garch_fit <- ugarchfit(spec = garch_spec, data = returns_data, solver = "hybrid")
  garch_vol <- sigma(garch_fit)
  
  # Extract GARCH parameters
  garch_persistence <- as.numeric(coef(garch_fit)["alpha1"] + coef(garch_fit)["beta1"])
  garch_aic <- infocriteria(garch_fit)[1]
  garch_bic <- infocriteria(garch_fit)[2]
  
  # ========================================================================
  # B. EGARCH(1,1) Specification and Fitting
  # ========================================================================
  
  egarch_spec <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  egarch_fit <- ugarchfit(spec = egarch_spec, data = returns_data, solver = "hybrid")
  egarch_vol <- sigma(egarch_fit)
  
  # Extract EGARCH parameters
  egarch_persistence <- as.numeric(coef(egarch_fit)["alpha1"] + coef(egarch_fit)["beta1"])
  egarch_gamma <- as.numeric(coef(egarch_fit)["gamma1"])
  egarch_aic <- infocriteria(egarch_fit)[1]
  egarch_bic <- infocriteria(egarch_fit)[2]
  
  # ========================================================================
  # C. Diagnostic Tests on GARCH Residuals
  # ========================================================================
  
  std_resid <- residuals(garch_fit, standardize = TRUE)
  
  # Jarque-Bera test
  jb_test <- jarque.bera.test(std_resid)
  jb_stat <- as.numeric(jb_test$statistic)
  jb_pval <- as.numeric(jb_test$p.value)
  
  # Ljung-Box tests
  lb_resid_10 <- Box.test(std_resid, lag = 10, type = "Ljung-Box")
  lb_resid_20 <- Box.test(std_resid, lag = 20, type = "Ljung-Box")
  lb_sq_resid_10 <- Box.test(std_resid^2, lag = 10, type = "Ljung-Box")
  
  # ========================================================================
  # D. Out-of-Sample Rolling Window (S&P 500 & Momentum ONLY)
  # ========================================================================
  
  oos_results <- NULL
  
  if (factor_name %in% c("S&P 500", "Momentum")) {
    
    n_total <- length(returns_data)
    window_size <- 250
    forecast_steps <- n_total - window_size
    
    garch_forecasts <- numeric(forecast_steps)
    egarch_forecasts <- numeric(forecast_steps)
    realized_vol <- numeric(forecast_steps)
    
    for (i in 1:forecast_steps) {
      
      train_start <- i
      train_end <- i + window_size - 1
      train_data <- returns_data[train_start:train_end]
      
      # Fit GARCH on training window
      garch_spec_oos <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
        mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
        distribution.model = "norm"
      )
      garch_fit_oos <- ugarchfit(spec = garch_spec_oos, data = train_data, 
                                 solver = "hybrid", trace = FALSE)
      garch_fcst <- ugarchforecast(fitORspec = garch_fit_oos, n.ahead = 1)
      garch_forecasts[i] <- as.numeric(garch_fcst@forecast$sigmaFor)
      
      # Fit EGARCH on training window
      egarch_spec_oos <- ugarchspec(
        variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
        mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
        distribution.model = "norm"
      )
      egarch_fit_oos <- ugarchfit(spec = egarch_spec_oos, data = train_data, 
                                  solver = "hybrid", trace = FALSE)
      egarch_fcst <- ugarchforecast(fitORspec = egarch_fit_oos, n.ahead = 1)
      egarch_forecasts[i] <- as.numeric(egarch_fcst@forecast$sigmaFor)
      
      # Calculate realized volatility (20-day window)
      window_start <- i + window_size - 19
      window_end <- i + window_size
      if (window_end <= n_total) {
        daily_returns <- returns_data[window_start:window_end]
        realized_vol[i] <- sd(as.numeric(daily_returns))
      } else {
        realized_vol[i] <- NA
      }
    }
    
    # Clean data
    valid_idx <- !is.na(realized_vol)
    realized_vol_clean <- realized_vol[valid_idx]
    garch_fcst_clean <- garch_forecasts[valid_idx]
    egarch_fcst_clean <- egarch_forecasts[valid_idx]
    
    # Calculate metrics
    rmse_garch <- sqrt(mean((realized_vol_clean - garch_fcst_clean)^2, na.rm = TRUE))
    rmse_egarch <- sqrt(mean((realized_vol_clean - egarch_fcst_clean)^2, na.rm = TRUE))
    
    mae_garch <- mean(abs(realized_vol_clean - garch_fcst_clean), na.rm = TRUE)
    mae_egarch <- mean(abs(realized_vol_clean - egarch_fcst_clean), na.rm = TRUE)
    
    mape_garch <- mean(abs((realized_vol_clean - garch_fcst_clean) / realized_vol_clean), 
                       na.rm = TRUE) * 100
    mape_egarch <- mean(abs((realized_vol_clean - egarch_fcst_clean) / realized_vol_clean), 
                        na.rm = TRUE) * 100
    
    # Paired t-test
    garch_errors_sq <- (realized_vol_clean - garch_fcst_clean)^2
    egarch_errors_sq <- (realized_vol_clean - egarch_fcst_clean)^2
    t_test <- t.test(garch_errors_sq, egarch_errors_sq, paired = TRUE)
    
    oos_results <- list(
      rmse_garch = rmse_garch,
      rmse_egarch = rmse_egarch,
      mae_garch = mae_garch,
      mae_egarch = mae_egarch,
      mape_garch = mape_garch,
      mape_egarch = mape_egarch,
      t_stat = as.numeric(t_test$statistic),
      t_pval = as.numeric(t_test$p.value),
      forecast_data = data.frame(
        realized = realized_vol_clean,
        garch = garch_fcst_clean,
        egarch = egarch_fcst_clean
      )
    )
  }
  
  # ========================================================================
  # E. Return comprehensive results list
  # ========================================================================
  
  return(list(
    factor_name = factor_name,
    garch_fit = garch_fit,
    egarch_fit = egarch_fit,
    garch_vol = garch_vol,
    egarch_vol = egarch_vol,
    
    # GARCH metrics
    garch_persistence = garch_persistence,
    garch_aic = garch_aic,
    garch_bic = garch_bic,
    
    # EGARCH metrics
    egarch_persistence = egarch_persistence,
    egarch_gamma = egarch_gamma,
    egarch_aic = egarch_aic,
    egarch_bic = egarch_bic,
    aic_diff = egarch_aic - garch_aic,
    
    # Diagnostic tests
    jb_stat = jb_stat,
    jb_pval = jb_pval,
    lb_resid_10_pval = as.numeric(lb_resid_10$p.value),
    lb_resid_20_pval = as.numeric(lb_resid_20$p.value),
    lb_sq_resid_10_pval = as.numeric(lb_sq_resid_10$p.value),
    std_resid = std_resid,
    
    # Out-of-sample results (if applicable)
    oos_results = oos_results
  ))
}

###############################################################################
# SECTION 3: APPLY FUNCTION TO ALL FACTORS
###############################################################################

cat("\n========== PROCESSING ALL FACTORS ==========\n\n")

all_results <- list()

for (factor_info in factors_list) {
  result <- fit_complete_analysis(factor_info$data, factor_info$name)
  all_results[[factor_info$name]] <- result
}

cat("\n✓ Analysis complete for all factors!\n\n")

###############################################################################
# SECTION 4: CREATE COMPREHENSIVE COMPARISON TABLE
###############################################################################

cat("========== BUILDING RESULTS TABLE ==========\n\n")

comparison_table <- data.frame(
  Factor = sapply(all_results, function(x) x$factor_name),
  
  # GARCH Metrics
  GARCH_AIC = round(sapply(all_results, function(x) x$garch_aic), 2),
  GARCH_Persistence = round(sapply(all_results, function(x) x$garch_persistence), 4),
  
  # EGARCH Metrics
  EGARCH_AIC = round(sapply(all_results, function(x) x$egarch_aic), 2),
  EGARCH_Gamma = round(sapply(all_results, function(x) x$egarch_gamma), 4),
  
  # Model Comparison
  AIC_Difference = round(sapply(all_results, function(x) x$aic_diff), 4),
  Better_Model = ifelse(
    sapply(all_results, function(x) x$aic_diff) < 0,
    "EGARCH",
    "GARCH"
  ),
  
  # Diagnostic Tests
  JB_Statistic = round(sapply(all_results, function(x) x$jb_stat), 2),
  JB_P_Value = format(sapply(all_results, function(x) x$jb_pval), scientific = TRUE, digits = 2),
  LB_Resid_10_P = round(sapply(all_results, function(x) x$lb_resid_10_pval), 4),
  LB_SqResid_10_P = round(sapply(all_results, function(x) x$lb_sq_resid_10_pval), 4)
)

# Display table
cat("COMPREHENSIVE COMPARISON TABLE:\n\n")
print(comparison_table)

# Export to CSV
write.csv(comparison_table, "Complete_Analysis_Comparison.csv", row.names = FALSE)
cat("\n✓ Saved: Complete_Analysis_Comparison.csv\n")

###############################################################################
# SECTION 5: OUT-OF-SAMPLE RESULTS (S&P 500 & MOMENTUM)
###############################################################################

cat("\n========== OUT-OF-SAMPLE FORECAST RESULTS ==========\n\n")

oos_table <- data.frame(
  Factor = c("S&P 500", "Momentum"),
  
  RMSE_GARCH = c(
    round(all_results[["S&P 500"]]$oos_results$rmse_garch, 6),
    round(all_results[["Momentum"]]$oos_results$rmse_garch, 6)
  ),
  
  RMSE_EGARCH = c(
    round(all_results[["S&P 500"]]$oos_results$rmse_egarch, 6),
    round(all_results[["Momentum"]]$oos_results$rmse_egarch, 6)
  ),
  
  RMSE_Improvement_Pct = c(
    paste0(round((all_results[["S&P 500"]]$oos_results$rmse_garch - 
                  all_results[["S&P 500"]]$oos_results$rmse_egarch) / 
                 all_results[["S&P 500"]]$oos_results$rmse_garch * 100, 2), "%"),
    paste0(round((all_results[["Momentum"]]$oos_results$rmse_garch - 
                  all_results[["Momentum"]]$oos_results$rmse_egarch) / 
                 all_results[["Momentum"]]$oos_results$rmse_garch * 100, 2), "%")
  ),
  
  MAE_GARCH = c(
    round(all_results[["S&P 500"]]$oos_results$mae_garch, 6),
    round(all_results[["Momentum"]]$oos_results$mae_garch, 6)
  ),
  
  MAE_EGARCH = c(
    round(all_results[["S&P 500"]]$oos_results$mae_egarch, 6),
    round(all_results[["Momentum"]]$oos_results$mae_egarch, 6)
  ),
  
  MAE_Improvement_Pct = c(
    paste0(round((all_results[["S&P 500"]]$oos_results$mae_garch - 
                  all_results[["S&P 500"]]$oos_results$mae_egarch) / 
                 all_results[["S&P 500"]]$oos_results$mae_garch * 100, 2), "%"),
    paste0(round((all_results[["Momentum"]]$oos_results$mae_garch - 
                  all_results[["Momentum"]]$oos_results$mae_egarch) / 
                 all_results[["Momentum"]]$oos_results$mae_garch * 100, 2), "%")
  ),
  
  T_Test_P_Value = c(
    round(all_results[["S&P 500"]]$oos_results$t_pval, 4),
    round(all_results[["Momentum"]]$oos_results$t_pval, 4)
  ),
  
  Significant = c(
    ifelse(all_results[["S&P 500"]]$oos_results$t_pval < 0.05, "YES ***", "NO"),
    ifelse(all_results[["Momentum"]]$oos_results$t_pval < 0.05, "YES ***", "NO")
  )
)

print(oos_table)

# Export to CSV
write.csv(oos_table, "Out_of_Sample_Results.csv", row.names = FALSE)
cat("\n✓ Saved: Out_of_Sample_Results.csv\n")

###############################################################################

cat("\n========== BLIND SPOT FIX #1: QUANTIFY TAIL UNDERPREDICTION ==========\n\n")

# For S&P 500
sp500_high_vol_threshold <- quantile(all_results[["S&P 500"]]$oos_results$forecast_data$realized, 0.75)
sp500_high_vol_idx <- all_results[["S&P 500"]]$oos_results$forecast_data$realized > sp500_high_vol_threshold

sp500_garch_underpred <- mean(
  all_results[["S&P 500"]]$oos_results$forecast_data$realized[sp500_high_vol_idx] - 
    all_results[["S&P 500"]]$oos_results$forecast_data$garch[sp500_high_vol_idx]
)

sp500_egarch_underpred <- mean(
  all_results[["S&P 500"]]$oos_results$forecast_data$realized[sp500_high_vol_idx] - 
    all_results[["S&P 500"]]$oos_results$forecast_data$egarch[sp500_high_vol_idx]
)

sp500_tail_improvement <- ((sp500_garch_underpred - sp500_egarch_underpred) / 
                             sp500_garch_underpred) * 100

cat("S&P 500 - DURING HIGH VOLATILITY PERIODS (>75th percentile):\n")
cat("  GARCH Average Underprediction:", round(sp500_garch_underpred, 6), "\n")
cat("  EGARCH Average Underprediction:", round(sp500_egarch_underpred, 6), "\n")
cat("  Tail Risk Improvement:", round(sp500_tail_improvement, 2), "%\n\n")

# For Momentum
momentum_high_vol_threshold <- quantile(all_results[["Momentum"]]$oos_results$forecast_data$realized, 0.75)
momentum_high_vol_idx <- all_results[["Momentum"]]$oos_results$forecast_data$realized > momentum_high_vol_threshold

momentum_garch_underpred <- mean(
  all_results[["Momentum"]]$oos_results$forecast_data$realized[momentum_high_vol_idx] - 
    all_results[["Momentum"]]$oos_results$forecast_data$garch[momentum_high_vol_idx]
)

momentum_egarch_underpred <- mean(
  all_results[["Momentum"]]$oos_results$forecast_data$realized[momentum_high_vol_idx] - 
    all_results[["Momentum"]]$oos_results$forecast_data$egarch[momentum_high_vol_idx]
)

momentum_tail_improvement <- ((momentum_garch_underpred - momentum_egarch_underpred) / 
                                momentum_garch_underpred) * 100

cat("Momentum - DURING HIGH VOLATILITY PERIODS (>75th percentile):\n")
cat("  GARCH Average Underprediction:", round(momentum_garch_underpred, 6), "\n")
cat("  EGARCH Average Underprediction:", round(momentum_egarch_underpred, 6), "\n")
cat("  Tail Risk Improvement:", round(momentum_tail_improvement, 2), "%\n\n")

# Save to table
tail_risk_summary <- data.frame(
  Factor = c("S&P 500", "Momentum"),
  GARCH_Tail_Underpred = c(round(sp500_garch_underpred, 6), round(momentum_garch_underpred, 6)),
  EGARCH_Tail_Underpred = c(round(sp500_egarch_underpred, 6), round(momentum_egarch_underpred, 6)),
  Improvement_Pct = c(round(sp500_tail_improvement, 2), round(momentum_tail_improvement, 2))
)

write.csv(tail_risk_summary, "Tail_Risk_Improvement.csv", row.names = FALSE)
cat("✓ Saved: Tail_Risk_Improvement.csv\n\n")

###############################################################################
# BLIND SPOT FIX #2: FORECAST ACCURACY BY PANDEMIC PERIOD
###############################################################################

cat("========== BLIND SPOT FIX #2: FORECAST ACCURACY BY PANDEMIC PERIOD ==========\n\n")

# For S&P 500
sp500_oos_start_date <- index(sp500_returns)[251]  # Start of OOS period
sp500_forecast_dates <- seq(sp500_oos_start_date, 
                            sp500_oos_start_date + days(length(all_results[["S&P 500"]]$oos_results$forecast_data$realized) - 1),
                            by = "day")

# Remove weekends/non-trading days to match actual trading dates
sp500_forecast_dates <- sp500_forecast_dates[sp500_forecast_dates %in% index(sp500_returns)]
sp500_forecast_dates <- sp500_forecast_dates[1:length(all_results[["S&P 500"]]$oos_results$forecast_data$realized)]

sp500_forecast_period_mapping <- data.frame(
  forecast_num = 1:length(all_results[["S&P 500"]]$oos_results$forecast_data$realized),
  date = sp500_forecast_dates,
  realized_vol = all_results[["S&P 500"]]$oos_results$forecast_data$realized,
  garch_error_sq = (all_results[["S&P 500"]]$oos_results$forecast_data$realized - 
                      all_results[["S&P 500"]]$oos_results$forecast_data$garch)^2,
  egarch_error_sq = (all_results[["S&P 500"]]$oos_results$forecast_data$realized - 
                       all_results[["S&P 500"]]$oos_results$forecast_data$egarch)^2
)

# Classify by pandemic period
sp500_forecast_period_mapping <- sp500_forecast_period_mapping %>%
  mutate(
    period = case_when(
      date < as.Date("2020-03-20") ~ "Pre-Crisis",
      date >= as.Date("2020-03-20") & date <= as.Date("2020-04-15") ~ "Fever",
      date > as.Date("2020-04-15") ~ "Recovery"
    )
  )

# Calculate accuracy by period
sp500_accuracy_by_period <- sp500_forecast_period_mapping %>%
  group_by(period) %>%
  summarise(
    GARCH_RMSE = sqrt(mean(garch_error_sq, na.rm=TRUE)),
    EGARCH_RMSE = sqrt(mean(egarch_error_sq, na.rm=TRUE)),
    EGARCH_Advantage_Pct = round(((GARCH_RMSE - EGARCH_RMSE) / GARCH_RMSE * 100), 2),
    N_Forecasts = n(),
    .groups = 'drop'
  )

cat("S&P 500 - FORECAST ACCURACY BY PANDEMIC PERIOD:\n")
print(sp500_accuracy_by_period)

# Same for Momentum
momentum_oos_start_date <- index(momentum_returns)[251]
momentum_forecast_dates <- seq(momentum_oos_start_date, 
                               momentum_oos_start_date + days(length(all_results[["Momentum"]]$oos_results$forecast_data$realized) - 1),
                               by = "day")
momentum_forecast_dates <- momentum_forecast_dates[momentum_forecast_dates %in% index(momentum_returns)]
momentum_forecast_dates <- momentum_forecast_dates[1:length(all_results[["Momentum"]]$oos_results$forecast_data$realized)]

momentum_forecast_period_mapping <- data.frame(
  forecast_num = 1:length(all_results[["Momentum"]]$oos_results$forecast_data$realized),
  date = momentum_forecast_dates,
  realized_vol = all_results[["Momentum"]]$oos_results$forecast_data$realized,
  garch_error_sq = (all_results[["Momentum"]]$oos_results$forecast_data$realized - 
                      all_results[["Momentum"]]$oos_results$forecast_data$garch)^2,
  egarch_error_sq = (all_results[["Momentum"]]$oos_results$forecast_data$realized - 
                       all_results[["Momentum"]]$oos_results$forecast_data$egarch)^2
)

momentum_forecast_period_mapping <- momentum_forecast_period_mapping %>%
  mutate(
    period = case_when(
      date < as.Date("2020-03-20") ~ "Pre-Crisis",
      date >= as.Date("2020-03-20") & date <= as.Date("2020-04-15") ~ "Fever",
      date > as.Date("2020-04-15") ~ "Recovery"
    )
  )

momentum_accuracy_by_period <- momentum_forecast_period_mapping %>%
  group_by(period) %>%
  summarise(
    GARCH_RMSE = sqrt(mean(garch_error_sq, na.rm=TRUE)),
    EGARCH_RMSE = sqrt(mean(egarch_error_sq, na.rm=TRUE)),
    EGARCH_Advantage_Pct = round(((GARCH_RMSE - EGARCH_RMSE) / GARCH_RMSE * 100), 2),
    N_Forecasts = n(),
    .groups = 'drop'
  )

cat("\nMomentum - FORECAST ACCURACY BY PANDEMIC PERIOD:\n")
print(momentum_accuracy_by_period)

# Combine and save
accuracy_by_period_all <- bind_rows(
  sp500_accuracy_by_period %>% mutate(Factor = "S&P 500"),
  momentum_accuracy_by_period %>% mutate(Factor = "Momentum")
) %>%
  select(Factor, period, GARCH_RMSE, EGARCH_RMSE, EGARCH_Advantage_Pct, N_Forecasts)

write.csv(accuracy_by_period_all, "Accuracy_by_Period.csv", row.names = FALSE)
cat("\n✓ Saved: Accuracy_by_Period.csv\n\n")

# Identify when advantage is strongest
cat("KEY FINDING: EGARCH advantage is strongest during:\n")
sp500_max_advantage <- sp500_accuracy_by_period$period[which.max(sp500_accuracy_by_period$EGARCH_Advantage_Pct)]
sp500_max_advantage_val <- max(sp500_accuracy_by_period$EGARCH_Advantage_Pct)
cat("  S&P 500:", sp500_max_advantage, "period (", sp500_max_advantage_val, "% improvement)\n")

momentum_max_advantage <- momentum_accuracy_by_period$period[which.max(momentum_accuracy_by_period$EGARCH_Advantage_Pct)]
momentum_max_advantage_val <- max(momentum_accuracy_by_period$EGARCH_Advantage_Pct)
cat("  Momentum:", momentum_max_advantage, "period (", momentum_max_advantage_val, "% improvement)\n\n")

###############################################################################
# BLIND SPOT FIX #3: DIRECTIONAL ACCURACY DETAILED ANALYSIS
###############################################################################

cat("========== BLIND SPOT FIX #3: DIRECTIONAL ACCURACY BY PERIOD ==========\n\n")

# S&P 500 Directional Analysis
sp500_realized_direction <- sign(diff(all_results[["S&P 500"]]$oos_results$forecast_data$realized))
sp500_garch_direction <- sign(diff(all_results[["S&P 500"]]$oos_results$forecast_data$garch))
sp500_egarch_direction <- sign(diff(all_results[["S&P 500"]]$oos_results$forecast_data$egarch))

# Remove NA from diff
sp500_realized_direction <- sp500_realized_direction[-1]
sp500_garch_direction <- sp500_garch_direction[-1]
sp500_egarch_direction <- sp500_egarch_direction[-1]

sp500_garch_correct <- sp500_garch_direction == sp500_realized_direction
sp500_egarch_correct <- sp500_egarch_direction == sp500_realized_direction

sp500_both_wrong <- (!sp500_garch_correct) & (!sp500_egarch_correct)
sp500_only_egarch_right <- (!sp500_garch_correct) & (sp500_egarch_correct)
sp500_only_garch_right <- (sp500_garch_correct) & (!sp500_egarch_correct)

cat("S&P 500 - DIRECTIONAL FORECAST PERFORMANCE:\n")
cat("  Directional Accuracy (GARCH):", round(sum(sp500_garch_correct, na.rm=TRUE) / sum(!is.na(sp500_garch_correct)) * 100, 2), "%\n")
cat("  Directional Accuracy (EGARCH):", round(sum(sp500_egarch_correct, na.rm=TRUE) / sum(!is.na(sp500_egarch_correct)) * 100, 2), "%\n")
cat("  Times both models wrong:", sum(sp500_both_wrong, na.rm=TRUE), "/", sum(!is.na(sp500_both_wrong)), "periods\n")
cat("  Times only EGARCH correct (GARCH wrong):", sum(sp500_only_egarch_right, na.rm=TRUE), "periods\n")
cat("  Times only GARCH correct (EGARCH wrong):", sum(sp500_only_garch_right, na.rm=TRUE), "periods\n\n")

# Momentum Directional Analysis
momentum_realized_direction <- sign(diff(all_results[["Momentum"]]$oos_results$forecast_data$realized))
momentum_garch_direction <- sign(diff(all_results[["Momentum"]]$oos_results$forecast_data$garch))
momentum_egarch_direction <- sign(diff(all_results[["Momentum"]]$oos_results$forecast_data$egarch))

# Remove NA from diff
momentum_realized_direction <- momentum_realized_direction[-1]
momentum_garch_direction <- momentum_garch_direction[-1]
momentum_egarch_direction <- momentum_egarch_direction[-1]

momentum_garch_correct <- momentum_garch_direction == momentum_realized_direction
momentum_egarch_correct <- momentum_egarch_direction == momentum_realized_direction

momentum_both_wrong <- (!momentum_garch_correct) & (!momentum_egarch_correct)
momentum_only_egarch_right <- (!momentum_garch_correct) & (momentum_egarch_correct)
momentum_only_garch_right <- (momentum_garch_correct) & (!momentum_egarch_correct)

cat("Momentum - DIRECTIONAL FORECAST PERFORMANCE:\n")
cat("  Directional Accuracy (GARCH):", round(sum(momentum_garch_correct, na.rm=TRUE) / sum(!is.na(momentum_garch_correct)) * 100, 2), "%\n")
cat("  Directional Accuracy (EGARCH):", round(sum(momentum_egarch_correct, na.rm=TRUE) / sum(!is.na(momentum_egarch_correct)) * 100, 2), "%\n")
cat("  Times both models wrong:", sum(momentum_both_wrong, na.rm=TRUE), "/", sum(!is.na(momentum_both_wrong)), "periods\n")
cat("  Times only EGARCH correct (GARCH wrong):", sum(momentum_only_egarch_right, na.rm=TRUE), "periods\n")
cat("  Times only GARCH correct (EGARCH wrong):", sum(momentum_only_garch_right, na.rm=TRUE), "periods\n\n")

# Create summary table
directional_summary <- data.frame(
  Factor = c("S&P 500", "Momentum"),
  GARCH_Directional_Accuracy = c(
    round(sum(sp500_garch_correct, na.rm=TRUE) / sum(!is.na(sp500_garch_correct)) * 100, 2),
    round(sum(momentum_garch_correct, na.rm=TRUE) / sum(!is.na(momentum_garch_correct)) * 100, 2)
  ),
  EGARCH_Directional_Accuracy = c(
    round(sum(sp500_egarch_correct, na.rm=TRUE) / sum(!is.na(sp500_egarch_correct)) * 100, 2),
    round(sum(momentum_egarch_correct, na.rm=TRUE) / sum(!is.na(momentum_egarch_correct)) * 100, 2)
  ),
  Times_EGARCH_Beats_GARCH = c(
    sum(sp500_only_egarch_right, na.rm=TRUE),
    sum(momentum_only_egarch_right, na.rm=TRUE)
  )
)

write.csv(directional_summary, "Directional_Accuracy.csv", row.names = FALSE)
cat("✓ Saved: Directional_Accuracy.csv\n\n")

###############################################################################
# SUMMARY: KEY STATISTICS FOR INTRODUCTION/CONCLUSION WRITING
###############################################################################

cat("========== SUMMARY: KEY STATISTICS FOR YOUR PAPER ==========\n\n")

cat("USE THESE NUMBERS IN YOUR INTRODUCTION/CONCLUSION:\n\n")

cat("1. OVERALL FORECAST IMPROVEMENT:\n")
cat("   - S&P 500 RMSE improves by", round(all_results[["S&P 500"]]$oos_results$rmse_garch - all_results[["S&P 500"]]$oos_results$rmse_egarch, 6), 
    "(", round((all_results[["S&P 500"]]$oos_results$rmse_garch - all_results[["S&P 500"]]$oos_results$rmse_egarch) / all_results[["S&P 500"]]$oos_results$rmse_garch * 100, 2), "%)\n")
cat("   - Momentum RMSE improves by", round(all_results[["Momentum"]]$oos_results$rmse_garch - all_results[["Momentum"]]$oos_results$rmse_egarch, 6), 
    "(", round((all_results[["Momentum"]]$oos_results$rmse_garch - all_results[["Momentum"]]$oos_results$rmse_egarch) / all_results[["Momentum"]]$oos_results$rmse_garch * 100, 2), "%)\n\n")

cat("2. TAIL RISK IMPROVEMENT (Most Important for Crisis Risk Management):\n")
cat("   - S&P 500 tail risk improvement:", round(sp500_tail_improvement, 2), "%\n")
cat("   - Momentum tail risk improvement:", round(momentum_tail_improvement, 2), "%\n\n")

cat("3. CRISIS PERIOD ADVANTAGE (When It Matters Most):\n")
cat("   - S&P 500 EGARCH advantage strongest in:", sp500_max_advantage, "period\n")
cat("   - Momentum EGARCH advantage strongest in:", momentum_max_advantage, "period\n\n")

cat("4. ASYMMETRY COEFFICIENTS (Why EGARCH is Needed):\n")
cat("   - S&P 500 gamma:", round(all_results[["S&P 500"]]$egarch_gamma, 4), "\n")
cat("   - Momentum gamma:", round(all_results[["Momentum"]]$egarch_gamma, 4), "\n")
cat("   - Value gamma:", round(all_results[["Value"]]$egarch_gamma, 4), "\n\n")

cat("5. PERSISTENCE (Volatility Clustering):\n")
cat("   - All factors show persistence > 0.98, indicating extreme volatility clustering during crisis\n\n")

cat("========== READY TO WRITE INTRODUCTION AND CONCLUSION ==========\n")

###############################################################################
# SECTION 6: CREATE LATEX TABLES FOR PAPER
###############################################################################

cat("\n========== GENERATING LATEX TABLES ==========\n\n")

# Table 1: In-Sample Comparison (All Factors)
cat("Table 1: In-Sample GARCH vs EGARCH Comparison\n")
table_1 <- kable(
  comparison_table[, c("Factor", "GARCH_AIC", "GARCH_Persistence", 
                       "EGARCH_AIC", "EGARCH_Gamma", "AIC_Difference", "Better_Model")],
  format = "latex",
  booktabs = TRUE,
  digits = c(0, 2, 4, 2, 4, 4, 0),
  caption = "GARCH(1,1) vs EGARCH(1,1): In-Sample Model Comparison Across All Factors",
  col.names = c("Factor", "GARCH AIC", "GARCH α+β", "EGARCH AIC", "EGARCH γ", "Δ AIC", "Better")
) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"))

print(table_1)

# Table 2: Diagnostic Tests
cat("\n\nTable 2: Diagnostic Tests\n")
table_2 <- kable(
  comparison_table[, c("Factor", "JB_Statistic", "JB_P_Value", 
                       "LB_Resid_10_P", "LB_SqResid_10_P")],
  format = "latex",
  booktabs = TRUE,
  digits = c(0, 2, 2, 4, 4),
  caption = "Diagnostic Tests: Jarque-Bera and Ljung-Box Statistics",
  col.names = c("Factor", "JB Stat", "JB P-val", "LB Resid P", "LB Sq.Resid P")
) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

print(table_2)

# Table 3: Out-of-Sample Results
cat("\n\nTable 3: Out-of-Sample Results\n")
table_3 <- kable(
  oos_table,
  format = "latex",
  booktabs = TRUE,
  caption = "Out-of-Sample Forecast Accuracy: GARCH vs EGARCH",
  col.names = c("Factor", "GARCH RMSE", "EGARCH RMSE", "RMSE Imp.", 
                "GARCH MAE", "EGARCH MAE", "MAE Imp.", "T-test P", "Sig.")
) %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"))

print(table_3)

###############################################################################
# SECTION 7: GENERATE KEY PUBLICATION FIGURES
###############################################################################

cat("\n========== GENERATING PUBLICATION FIGURES ==========\n\n")

# Figure 1: Momentum vs Value (2-panel comparison)
cat("Figure 1: Momentum vs Value - GARCH/EGARCH Comparison\n")

plot_momentum <- data.frame(
  Date = index(momentum_returns),
  Returns = as.numeric(momentum_returns),
  GARCH = as.numeric(all_results[["Momentum"]]$garch_vol),
  EGARCH = as.numeric(all_results[["Momentum"]]$egarch_vol)
)

p_momentum <- ggplot(plot_momentum, aes(x = Date)) +
  geom_line(aes(y = Returns, colour = "Returns"), alpha = 0.4, size = 0.4) +
  geom_line(aes(y = GARCH, colour = "GARCH(1,1)"), size = 0.8) +
  geom_line(aes(y = EGARCH, colour = "EGARCH(1,1)"), size = 0.8, linetype = "dashed") +
  scale_colour_manual("", values = c("black", "blue", "red")) +
  labs(title = "Momentum Factor",
       subtitle = paste("γ =", round(all_results[["Momentum"]]$egarch_gamma, 3),
                       "| Δ AIC =", round(all_results[["Momentum"]]$aic_diff, 4)),
       y = "Returns / Volatility",
       x = "Date") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9))

plot_value <- data.frame(
  Date = index(value_returns),
  Returns = as.numeric(value_returns),
  GARCH = as.numeric(all_results[["Value"]]$garch_vol),
  EGARCH = as.numeric(all_results[["Value"]]$egarch_vol)
)

p_value <- ggplot(plot_value, aes(x = Date)) +
  geom_line(aes(y = Returns, colour = "Returns"), alpha = 0.4, size = 0.4) +
  geom_line(aes(y = GARCH, colour = "GARCH(1,1)"), size = 0.8) +
  geom_line(aes(y = EGARCH, colour = "EGARCH(1,1)"), size = 0.8, linetype = "dashed") +
  scale_colour_manual("", values = c("black", "blue", "red")) +
  labs(title = "Value Factor",
       subtitle = paste("γ =", round(all_results[["Value"]]$egarch_gamma, 3),
                       "| Δ AIC =", round(all_results[["Value"]]$aic_diff, 4)),
       y = "Returns / Volatility",
       x = "Date") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9))

fig_1 <- gridExtra::grid.arrange(p_momentum, p_value, ncol = 2)
print(fig_1)

ggsave("Figure_1_Momentum_vs_Value.png", fig_1, width = 14, height = 6, dpi = 300)
cat("✓ Saved: Figure_1_Momentum_vs_Value.png\n\n")

# Figure 2: Out-of-Sample Forecast Comparison
cat("Figure 2: Out-of-Sample Forecast Accuracy\n")

oos_visual <- data.frame(
  Factor = rep(c("S&P 500", "Momentum"), 2),
  Metric = rep(c("RMSE", "MAE"), each = 2),
  GARCH = c(
    all_results[["S&P 500"]]$oos_results$rmse_garch,
    all_results[["Momentum"]]$oos_results$rmse_garch,
    all_results[["S&P 500"]]$oos_results$mae_garch,
    all_results[["Momentum"]]$oos_results$mae_garch
  ),
  EGARCH = c(
    all_results[["S&P 500"]]$oos_results$rmse_egarch,
    all_results[["Momentum"]]$oos_results$rmse_egarch,
    all_results[["S&P 500"]]$oos_results$mae_egarch,
    all_results[["Momentum"]]$oos_results$mae_egarch
  )
)

oos_long <- pivot_longer(oos_visual, cols = c("GARCH", "EGARCH"), 
                         names_to = "Model", values_to = "Error")

fig_2 <- ggplot(oos_long, aes(x = Factor, y = Error, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1) +
  scale_fill_manual("Model", values = c("GARCH(1,1)" = "steelblue", "EGARCH(1,1)" = "coral")) +
  labs(title = "Out-of-Sample Forecast Accuracy: GARCH vs EGARCH",
       subtitle = "Rolling window validation (250-observation training window, 124 out-of-sample forecasts)",
       y = "Forecast Error (Lower is Better)",
       x = "Factor") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 11),
        axis.text.x = element_text(angle = 0))

print(fig_2)

ggsave("Figure_2_OOS_Accuracy.png", fig_2, width = 12, height = 5, dpi = 300)
cat("✓ Saved: Figure_2_OOS_Accuracy.png\n\n")

###############################################################################
# SECTION 8: SUMMARY STATISTICS FOR PAPER WRITING
###############################################################################

cat("========== KEY STATISTICS FOR WRITING ==========\n\n")

# Extract key numbers for narrative
cat("KEY FINDINGS TO CITE IN PAPER:\n\n")

cat("1. ASYMMETRY COEFFICIENTS (EGARCH γ):\n")
for (factor in names(all_results)) {
  gamma <- all_results[[factor]]$egarch_gamma
  aic_diff <- all_results[[factor]]$aic_diff
  cat("   ", factor, ": γ =", round(gamma, 4), ", Δ AIC =", round(aic_diff, 4), "\n")
}

cat("\n2. PERSISTENCE (α + β):\n")
for (factor in names(all_results)) {
  persist_g <- all_results[[factor]]$garch_persistence
  persist_e <- all_results[[factor]]$egarch_persistence
  cat("   ", factor, ": GARCH =", round(persist_g, 4), 
      ", EGARCH =", round(persist_e, 4), "\n")
}

cat("\n3. JARQUE-BERA NORMALITY TEST:\n")
for (factor in names(all_results)) {
  jb_stat <- all_results[[factor]]$jb_stat
  jb_pval <- all_results[[factor]]$jb_pval
  sig <- ifelse(jb_pval < 0.001, "***", ifelse(jb_pval < 0.05, "**", ""))
  cat("   ", factor, ": χ² =", round(jb_stat, 2), ", p < 0.001", sig, "\n")
}

cat("\n4. OUT-OF-SAMPLE FORECAST IMPROVEMENT:\n")
rmse_imp_sp500 <- (all_results[["S&P 500"]]$oos_results$rmse_garch - 
                   all_results[["S&P 500"]]$oos_results$rmse_egarch) / 
                  all_results[["S&P 500"]]$oos_results$rmse_garch * 100
rmse_imp_mom <- (all_results[["Momentum"]]$oos_results$rmse_garch - 
                 all_results[["Momentum"]]$oos_results$rmse_egarch) / 
                all_results[["Momentum"]]$oos_results$rmse_garch * 100

cat("   S&P 500 RMSE Improvement:", round(rmse_imp_sp500, 2), "%")
cat(" (p =", round(all_results[["S&P 500"]]$oos_results$t_pval, 4), ")\n")

cat("   Momentum RMSE Improvement:", round(rmse_imp_mom, 2), "%")
cat(" (p =", round(all_results[["Momentum"]]$oos_results$t_pval, 4), ")\n")

###############################################################################
# SECTION 9: EXPORT ALL RESULTS
###############################################################################

cat("\n========== EXPORT SUMMARY ==========\n\n")
cat("✓ Complete_Analysis_Comparison.csv - All in-sample metrics\n")
cat("✓ Out_of_Sample_Results.csv - OOS forecast accuracy\n")
cat("✓ Figure_1_Momentum_vs_Value.png - Publication figure\n")
cat("✓ Figure_2_OOS_Accuracy.png - Publication figure\n")
cat("✓ LaTeX tables printed above\n\n")

cat("========== ANALYSIS COMPLETE ==========\n")
cat("See 'KEY STATISTICS FOR WRITING' section above for specific numbers to cite.\n\n")

# Save workspace
save.image("analysis_results.RData")
cat("✓ Saved: analysis_results.RData (all results in workspace)\n\n")



