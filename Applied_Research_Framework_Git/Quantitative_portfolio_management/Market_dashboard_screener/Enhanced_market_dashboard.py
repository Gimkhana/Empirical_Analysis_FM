#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 30 16:01:26 2023

@author: youssef
"""

import pandas as pd
import yfinance as yf
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import numpy as np

# Define assets by category (example subset)
assets = {
    'Stock Indices': ["VTI", "EEM", "^GSPC", "^DJI","^GDAXI","^BVSP", "^IXIC", "^FTSE", "^FCHI", "^STOXX50E", "^N225", "^HSI", "^KS11", "^AXJO", "^GSPTSE", "^BSESN"],
    'Global Markets': ["EWA", "EWZ","EWC", "ASHR", "EWQ", "EWG","EWH","PIN", "EWI", "EWJ", "EWW", "EWP","EIS","EWU", "EFA", "IOO", "BKF", "CWI"],
    'US Equity Sectors': ["IVW","IVE","RSP","DVY","XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLC", "XLU"]
}

# Fetch historical stock data from Yahoo Finance
start_date = "2023-01-01"
end_date = "2023-11-01"
data = {}
for category, tickers in assets.items():
    data[category] = {ticker: yf.download(ticker, start=start_date, end=end_date)['Adj Close'] for ticker in tickers}

# Function to compute MACD
def compute_macd(data, span1=12, span2=26, signal=9):
    exp1 = data.ewm(span=span1, adjust=False).mean()
    exp2 = data.ewm(span=span2, adjust=False).mean()
    macd = exp1 - exp2
    signal_line = macd.ewm(span=signal, adjust=False).mean()
    return macd, signal_line

# Function to compute Bollinger Bands
def compute_bollinger_bands(data, window=20, no_of_std=2):
    rolling_mean = data.rolling(window=window).mean()
    rolling_std = data.rolling(window=window).std()
    upper_band = rolling_mean + (rolling_std * no_of_std)
    lower_band = rolling_mean - (rolling_std * no_of_std)
    return upper_band, lower_band, rolling_mean

# Process and plot data for each category
for category, datasets in data.items():
    for ticker in datasets:
        fig = make_subplots(rows=2, cols=1, shared_xaxes=True, vertical_spacing=0.02, row_heights=[0.7, 0.3])

        # Price with Bollinger Bands
        upper_band, lower_band, rolling_mean = compute_bollinger_bands(datasets[ticker])
        fig.add_trace(go.Scatter(x=datasets[ticker].index, y=datasets[ticker], name='Price', line=dict(color='blue')), row=1, col=1)
        fig.add_trace(go.Scatter(x=datasets[ticker].index, y=upper_band, name='Upper Band', line=dict(color='red')), row=1, col=1)
        fig.add_trace(go.Scatter(x=datasets[ticker].index, y=lower_band, name='Lower Band', line=dict(color='green')), row=1, col=1)
        fig.add_trace(go.Scatter(x=datasets[ticker].index, y=rolling_mean, name='Moving Average', line=dict(color='purple', dash='dash')), row=1, col=1)

        # MACD
        macd, signal_line = compute_macd(datasets[ticker])
        fig.add_trace(go.Scatter(x=datasets[ticker].index, y=macd, name='MACD', line=dict(color='orange')), row=2, col=1)
        fig.add_trace(go.Scatter(x=datasets[ticker].index, y=signal_line, name='Signal Line', line=dict(color='black', dash='dot')), row=2, col=1)

        fig.update_layout(title_text=f"{ticker} Price, Bollinger Bands, and MACD", height=600)
        fig.show()

# Compute Sharpe Ratio (Assuming risk-free rate is 0 for simplicity)
for category, datasets in data.items():
    stats = pd.DataFrame(index=["Mean", "Standard Deviation", "Sharpe Ratio"])
    daily_returns = pd.DataFrame(datasets).pct_change().dropna()
    for ticker in datasets:
        stats[ticker] = [
            daily_returns[ticker].mean(),
            daily_returns[ticker].std(),
            daily_returns[ticker].mean() / daily_returns[ticker].std() if daily_returns[ticker].std() != 0 else np.nan
        ]
    print(f"\nStatistical Data for {category}:\n")
    print(stats)
    print("\n" + "-" * 50 + "\n")
