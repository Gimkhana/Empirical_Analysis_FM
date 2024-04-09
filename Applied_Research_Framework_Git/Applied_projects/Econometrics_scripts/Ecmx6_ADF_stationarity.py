#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov  6 13:13:27 2023

@author: youssef
"""

import yfinance as yf
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import acf
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.tsa.stattools import adfuller



#2. Deterline ACF and autocorrelation

# Download data
sp500 = yf.download('^GSPC', start='2010-01-01', end='2023-01-01')
cac40 = yf.download('^FCHI', start='2010-01-01', end='2023-01-01')

# Calculate log returns
sp500['Log_Returns'] = np.log(sp500['Close']).diff()
cac40['Log_Returns'] = np.log(cac40['Close']).diff()

# Drop the NA values that arise from differencing
sp500 = sp500.dropna()
cac40 = cac40.dropna()

# Function to plot series and autocorrelation
def plot_series_and_acf(title, series, lags=50):
    # Plot time series
    plt.figure(figsize=(14, 7))
    plt.subplot(211)
    plt.plot(series)
    plt.title(f'{title} Time Series')
    
    # Plot ACF
    plt.subplot(212)
    plot_acf(series, lags=lags, alpha=0.05)
    plt.title(f'{title} Autocorrelation')
    
    plt.tight_layout()
    plt.show()

# Plot for S&P 500
plot_series_and_acf('S&P 500', sp500['Log_Returns'])

# Plot for cac40 
plot_series_and_acf('cac40 ', cac40['Log_Returns'])

# Determine stationarity based on autocorrelogram
# We are using the 95% confidence interval, so if the ACF plot crosses the upper or lower confidence bound, 
# it suggests non-stationarity

# Print out autocorrelation for lag 1 to 5 for S&P 500
print("S&P 500 Log Returns Autocorrelations (Lag 1 to 5):")
print(acf(sp500['Log_Returns'])[1:6])

# Print out autocorrelation for lag 1 to 5 for cac40  600
print("cac40 Log Returns Autocorrelations (Lag 1 to 5):")
print(acf(cac40['Log_Returns'])[1:6])




#3. Test of stationarity


# Function to run ADF test
def adf_test(series, title=''):
    print(f'ADF Test: {title}')
    result = adfuller(series.dropna(), autolag='AIC')  # dropna() is used to remove NaN entries
    labels = ['ADF test statistic', 'p-value', '# lags used', '# observations']
    out = pd.Series(result[0:4], index=labels)

    for key, val in result[4].items():
        out[f'critical value ({key})'] = val

    print(out.to_string())  
    if result[1] <= 0.05:
        print("Strong evidence against the null hypothesis, reject the null hypothesis. Data is stationary.")
    else:
        print("Weak evidence against the null hypothesis, fail to reject the null hypothesis. Data is non-stationary.")
    print("\n")

# Download data from Yahoo Finance
nasdaq = yf.download('^IXIC', start='2020-01-01', end='2023-01-01')
cac40 = yf.download('^FCHI', start='2020-01-01', end='2023-01-01')

# Calculate log returns
nasdaq['Log_Return'] = np.log(nasdaq['Adj Close']).diff()
cac40['Log_Return'] = np.log(cac40['Adj Close']).diff()

# Run ADF test on the original series
adf_test(nasdaq['Adj Close'], title='NASDAQ')
adf_test(cac40['Adj Close'], title='CAC 40')

# Run ADF test on the log returns
adf_test(nasdaq['Log_Return'], title='NASDAQ Log Returns')
adf_test(cac40['Log_Return'], title='CAC 40 Log Returns')



#ARIMA model

import numpy as np
import yfinance as yf
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import acf, pacf, adfuller
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.arima_process import ArmaProcess

# 1. Identify and estimate ARMA
# (a) Download and plot
ticker = 'AAPL'  # Apple Inc. as an example
data = yf.download(ticker, start="2020-01-01", end="2023-01-01")
data['Log_Return'] = np.log(data['Close']).diff().dropna()

plt.figure(figsize=(10, 4))
plt.plot(data['Log_Return'])
plt.title('Log Returns of ' + ticker)
plt.show()

# (b) ACF and PACF to identify possible ARMA(p,q) models
lags = 20  # Number of lags for ACF and PACF
plt.figure(figsize=(10, 5))
plt.subplot(121)
plt.stem(acf(data['Log_Return'].dropna(), nlags=lags))
plt.title('ACF of Log Returns')
plt.subplot(122)
plt.stem(pacf(data['Log_Return'].dropna(), nlags=lags))
plt.title('PACF of Log Returns')
plt.tight_layout()
plt.show()

# Based on the ACF and PACF, you can hypothesize several ARMA models and estimate them.
# Here, for simplicity, let's assume ARMA(1,1)
model = ARIMA(data['Log_Return'].dropna(), order=(1, 0, 1))
fitted_model = model.fit()
print(fitted_model.summary())

# 2. Simulate an ARIMA
# (a) Simulate AR(1) with φ1 = 0.8
np.random.seed(123)
ar1 = np.array([1, -0.8])  # NB: Python convention, should be 0.8. Negative sign because Python uses a different sign convention
ma1 = np.array([1])
AR_object1 = ArmaProcess(ar1, ma1).generate_sample(nsample=200)
plt.plot(AR_object1)
plt.title('Simulated AR(1) Process with φ1 = 0.8')
plt.show()

# (b) Simulate AR(1) with φ1 = -0.7
ar2 = np.array([1, 0.7])  # Python convention, should be -0.7. Positive sign due to sign convention
ma2 = np.array([1])
AR_object2 = ArmaProcess(ar2, ma2).generate_sample(nsample=200)
plt.plot(AR_object2)
plt.title('Simulated AR(1) Process with φ1 = -0.7')
plt.show()

# 3. Simulate then estimate an ARMA
# (a) For n=200, simulate an AR(1) with φ1 = 0.8
simulated_AR = ArmaProcess(ar1, ma1).generate_sample(nsample=200)
# (b) Estimate an AR(1)
simulated_model = ARIMA(simulated_AR, order=(1, 0, 0))
simulated_fitted_model = simulated_model.fit()
print('Estimated coefficient φ1 for simulated data:', simulated_fitted_model.arparams)

# 4. Estimate an AR(1) using OLS (ordinary least squares)
from statsmodels.regression.linear_model import OLS
from statsmodels.tools.tools import add_constant

# Add a constant to the data
Y = data['Log_Return'].dropna()
X = add_constant(data['Log_Return'].shift().dropna())
# Drop the first row with NaN due to shifting
Y = Y[1:]
X = X[1:]

# Fit the OLS model
ols_model = OLS(Y, X)
ols_results = ols_model.fit()
print(ols_results.summary())










