import yfinance as yf
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm

# (a) Download equity index data
us_equity_index = yf.download('^GSPC', start='2020-01-01', end='2021-12-31')  # S&P 500 as a proxy for the US market
euro_equity_index = yf.download('^STOXX50E', start='2020-01-01', end='2021-12-31')  # EURO STOXX 50 as a European index

# (b) Synchronize data to common date range
common_dates = us_equity_index.index.intersection(euro_equity_index.index)
us_equity_index = us_equity_index.loc[common_dates]
euro_equity_index = euro_equity_index.loc[common_dates]


# (c) Calculate returns for each series
us_returns = us_equity_index['Adj Close'].pct_change().dropna()
euro_returns = euro_equity_index['Adj Close'].pct_change().dropna()


# (d) Estimate regression of Euro index returns on US index returns
X = sm.add_constant(us_returns)
model = sm.OLS(euro_returns, X).fit()
print(model.summary())

# Create a scatter plot of EU vs. US equity returns
plt.scatter(us_returns, euro_returns, alpha=0.5)
plt.xlabel('US Equity Index Returns')
plt.ylabel('Euro Equity Index Returns')
plt.title('Scatter Plot of Returns (EU vs. US)')
plt.grid(True)
plt.show()



# Interpretation of the estimates:
# αˆ (Alpha hat) represents the intercept, which is the expected return of the European index when the US index has a return of 0.
# βˆ (Beta hat) represents the slope, which indicates the sensitivity of the European index's returns to changes in the US index's returns.




# (e) Download the risk-free rate for the US market (e.g., 10-year Treasury yield) and calculate risk premia
risk_free_rate = yf.download('^IRX', start='2020-01-01', end='2021-12-31')  # 13-week Treasury bill yield as a proxy for the risk-free rate

# Resample to daily frequency and forward-fill missing values
risk_free_rate = risk_free_rate.resample('D').ffill()

# Calculate the daily risk-free rate as a fraction of the annualized rate
risk_free_rate_daily = risk_free_rate['Adj Close'] / 100 / 252

# Align time series data to have the same index
us_returns_aligned, euro_returns_aligned = us_returns.align(euro_returns, join='inner')

# Align time series data to have the same index
us_returns_aligned, euro_returns_aligned = us_returns.align(euro_returns, join='inner')
common_dates = us_returns_aligned.index

# Align the risk-free rate data to the common dates
risk_free_rate_aligned = risk_free_rate.loc[common_dates]

# Calculate the daily risk-free rate as a fraction of the annualized rate
risk_free_rate_daily = risk_free_rate_aligned['Adj Close'] / 100 / 252

# Calculate excess equity returns using aligned data
us_excess_returns = us_returns_aligned - risk_free_rate_daily
euro_excess_returns = euro_returns_aligned - risk_free_rate_daily


# (f) Estimate regression of Euro risk premia on US risk premia
X_risk = sm.add_constant(us_excess_returns)
model_risk = sm.OLS(euro_excess_returns, X_risk).fit()
print(model_risk.summary())

# Create a scatter plot of EU vs. US excess equity returns
plt.scatter(us_excess_returns, euro_excess_returns, alpha=0.5)
plt.xlabel('US Excess Equity Returns')
plt.ylabel('Euro Excess Equity Returns')
plt.title('Scatter Plot of Excess Returns (EU vs. US)')
plt.grid(True)
plt.show()



# Interpretation of the estimates:
# αˆ (Alpha hat) represents the intercept, which is the expected excess return of the European index when the US index's excess return is 0.
# βˆ (Beta hat) represents the slope, which indicates the sensitivity of the European index's excess returns to changes in the US index's excess returns.

