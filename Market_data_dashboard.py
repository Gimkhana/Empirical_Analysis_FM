#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov  6 20:40:34 2023

@author: youssef
"""

import pandas as pd
import yfinance as yf
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import skew, kurtosis


# Define assets by category
assets = {
    'Stock Indices': ["VTI", "EEM", "^GSPC", "^DJI","^GDAXI","^BVSP", "^IXIC", "^FTSE", "^FCHI", "^STOXX50E", "^N225", "^HSI", "^KS11", "^AXJO", "^GSPTSE", "^BSESN"],
    'Global Markets': ["EWA", "EWZ","EWC", "ASHR", "EWQ", "EWG","EWH","PIN", "EWI", "EWJ", "EWW", "EWP","EIS","EWU", "EFA", "IOO", "BKF", "CWI"], 
    'US Equity Sectors': ["IVW","IVE","RSP","DVY","XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLC", "XLU"],
    'US Government Bonds': ["IEF","TLT","AGG","BND","TIP","^IRX", "^FVX", "^TNX", "^TYX"],
    'Commodities': ["DBC","DBA","USO","UNG","GLD","SLV","GC=F", "MGC=F", "SI=F", "PL=F", "HG=F", "CL=F", "NG=F", "RB=F", "ZC=F", "BZ=F", "ZS=F", "ZC=F"],
    'Futures' : ["ES=F", "NQ=F", "RTY=F", "YM=F", "ZB=F", "ZT=F", "ZF=F", "ZN=F"],
    'FX': ["JPY=X", "GBPUSD=X", "AUDUSD=X", "NZDUSD=X","EURUSD=X", "EURJPY=X", "EURCAD=X","EURCHF=X", "GBPJPY=X", 
           "EURGBP=X", "GBPUSD=X", "USDSGD=X", "USDMXN=X", "EURAUD=X", "USDTRY=X"],
    'Volatility Index': ["^VIX","^OVX", "^VIX1D", "^VIX3M", "^EVZ", "^GVZ", "^OVX"]
}

# Fetch historical stock data from Yahoo Finance
start_date = "2023-01-01"
end_date = "2023-11-01"
data = {}
for category, tickers in assets.items():
    data[category] = {ticker: yf.download(ticker, start=start_date, end=end_date)['Adj Close'] for ticker in tickers}

def compute_rsi(data, window=14):
    """Compute the RSI (Relative Strength Index) of the data."""
    delta = data.diff()
    loss = delta.where(delta < 0, 0)
    gain = -delta.where(delta > 0, 0)
    avg_gain = gain.rolling(window=window, min_periods=1).mean()
    avg_loss = loss.rolling(window=window, min_periods=1).mean()
    rs = avg_gain / avg_loss
    return 100 - (100 / (1 + rs))

# Process and plot data for each category
for category, datasets in data.items():
    df = pd.DataFrame(datasets).dropna()
    daily_returns = df.pct_change().dropna()
    cumulative_returns = (1 + daily_returns).cumprod()
    
    # Plot Cumulative Return Performance
    cumulative_returns.plot(figsize=(12, 6), title=f"Cumulative Return Performance of {category}")
    plt.ylabel("Cumulative Return")
    plt.xlabel("Year")
    plt.legend(loc="upper left")
    plt.grid(True)
    plt.show()
    
    # Plot correlation matrix if more than one ticker in the category
    if len(datasets) > 1:
        corr_matrix = daily_returns.corr().round(1)
        sns.heatmap(corr_matrix, annot=True, cmap='coolwarm', vmin=-1, vmax=1)
        plt.title(f"Correlation Matrix of {category}")
        plt.show()
    
for category, datasets in data.items():
    df = pd.DataFrame(datasets).dropna()

    for ticker in datasets:
        
        # Plot Prices with Moving Averages
        plt.figure(figsize=(14, 7))
        df[ticker].plot(label='Price')
        df[ticker].rolling(window=50).mean().plot(label='50-period MA', linestyle='--')
        df[ticker].rolling(window=20).mean().plot(label='20-period MA', linestyle='-.')

        plt.title(f"Price and Moving Averages of {ticker}")
        plt.ylabel("Price")
        plt.xlabel("Date")
        plt.legend()
        plt.grid(True)
        plt.show()

        # Plot RSI
        rsi = compute_rsi(df[ticker])
        plt.figure(figsize=(14, 7))
        rsi.plot(label='RSI')
        plt.axhline(70, color='red', linestyle='--')
        plt.axhline(30, color='green', linestyle='--')
        plt.title(f"RSI of {ticker}")
        plt.ylabel("RSI")
        plt.xlabel("Date")
        plt.ylim(0, 100)
        plt.legend()
        plt.grid(True)
        plt.show()

    # Display Statistical Data for each asset in the category
    stats = pd.DataFrame(index=["Mean", "Standard Deviation", "Skewness", "Kurtosis"])
    daily_returns = df.pct_change().dropna()
    for ticker in datasets:
        stats[ticker] = [
            daily_returns[ticker].mean(),
            daily_returns[ticker].std(),
            skew(daily_returns[ticker]),
            kurtosis(daily_returns[ticker])
        ]
    print(f"Statistical Data for {category}:\n")
    print(stats)
    print("\n" + "-" * 50 + "\n")