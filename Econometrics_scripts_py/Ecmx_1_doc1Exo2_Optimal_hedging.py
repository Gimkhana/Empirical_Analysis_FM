import yfinance as yf
import matplotlib.pyplot as plt
# Now you can safely import the packages
import statsmodels.api as sm

# (a) Download equity index data
us_equity_index = yf.download('^GSPC', start='2020-01-01', end='2021-12-31')  # S&P 500 as a proxy for the US market
us_futures = yf.download('ES=F', start='2020-01-01', end='2021-12-31')  # US futures

# (b) Synchronize data to common date range
common_dates = us_equity_index.index.intersection(us_futures.index)
us_equity_index = us_equity_index.loc[common_dates]
us_futures = us_futures.loc[common_dates]


# (c) Calculate returns for each series
us_returns = us_equity_index['Adj Close'].pct_change().dropna()
us_futures_returns = us_futures['Adj Close'].pct_change().dropna()


# (d) Estimate regression of US Futures index returns on US index returns
X = sm.add_constant(us_futures_returns)
model = sm.OLS(us_returns, X).fit()
print(model.summary())

# Create a scatter plot of EU vs. US equity returns
plt.scatter(us_returns, us_futures_returns, alpha=0.5)
plt.xlabel('US Equity Index Returns')
plt.ylabel('US Futures Returns')
plt.title('Scatter Plot of Returns (EU vs. US)')
plt.grid(True)
plt.show()


# (e) Download the risk-free rate for the US market and calculate risk premia
risk_free_rate = yf.download('^IRX', start='2020-01-01', end='2021-12-31')  # 13-week Treasury bill

# Resample to daily frequency and forward-fill missing values
risk_free_rate = risk_free_rate.resample('D').ffill()

# Calculate the daily risk-free rate
risk_free_rate_daily = risk_free_rate['Adj Close'] / 100 / 252

# Align the risk-free rate data to the common dates of returns
common_dates = us_returns.index.intersection(us_futures_returns.index)
risk_free_rate_aligned = risk_free_rate_daily.reindex(common_dates, method='ffill')

# Calculate excess returns
us_excess_returns = us_returns - risk_free_rate_aligned
us_futures_excess_returns = us_futures_returns - risk_free_rate_aligned

# Estimate regression of US Futures risk premia on US risk premia
X = sm.add_constant(us_futures_excess_returns)  # Independent variable
y = us_excess_returns  # Dependent variable
model = sm.OLS(y, X).fit()
print(model.summary())

# Scatter plot
plt.scatter(us_futures_excess_returns, us_excess_returns, alpha=0.5)
plt.xlabel('US Futures Excess Returns')
plt.ylabel('US Excess Returns')
plt.title('Scatter Plot of Excess Returns')
plt.grid(True)
plt.show()
