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

#Consolidated file to compute different metrics


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

# Generate insights
market_insights = get_insights(data)
for category, insight in market_insights.items():
    print(f"{category} Insights: {insight}")

def get_insights(datasets):
    insights = {}
    for category, data in datasets.items():
        daily_returns = pd.DataFrame(data).pct_change()
        avg_returns = daily_returns.mean().sort_values(ascending=False)
        high_volatility = daily_returns.std().sort_values(ascending=False)
        top_performers = avg_returns.head(3).index.tolist()
        most_volatile = high_volatility.head(3).index.tolist()
        insights[category] = {'Top Performers': top_performers, 'Most Volatile': most_volatile}
    return insights

##Data shortlisting

selected_category = input("Enter category separated by comma ('Stock Indices, Global Markets, US Equity Sectors, US Government Bonds, Commodities, Futures, FX, Volatility'): ").split(',')
selected_assets = input("Enter assets separated by comma (e.g., 'VTI,EEM'): ").split(',')

start_date = input("Enter start date (YYYY-MM-DD): ")
end_date = input("Enter end date (YYYY-MM-DD): ")

# Fetch data only for selected assets
selected_data = {ticker: yf.download(ticker, start=start_date, end=end_date)['Adj Close'] for ticker in selected_assets}

# Directory to save PDFs
import os

pdf_dir = 'pdf_figures'
os.makedirs(pdf_dir, exist_ok=True)

for ticker, ticker_data in selected_data.items():
    fig = make_subplots(rows=2, cols=1, shared_xaxes=True, vertical_spacing=0.02, row_heights=[0.7, 0.3])

    # Price with Bollinger Bands
    upper_band, lower_band, rolling_mean = compute_bollinger_bands(ticker_data)
    fig.add_trace(go.Scatter(x=ticker_data.index, y=ticker_data, name='Price', line=dict(color='blue')), row=1, col=1)
    fig.add_trace(go.Scatter(x=ticker_data.index, y=upper_band, name='Upper Band', line=dict(color='red')), row=1, col=1)
    fig.add_trace(go.Scatter(x=ticker_data.index, y=lower_band, name='Lower Band', line=dict(color='green')), row=1, col=1)
    fig.add_trace(go.Scatter(x=ticker_data.index, y=rolling_mean, name='Moving Average', line=dict(color='purple', dash='dash')), row=1, col=1)

    # MACD
    macd, signal_line = compute_macd(ticker_data)
    fig.add_trace(go.Scatter(x=ticker_data.index, y=macd, name='MACD', line=dict(color='orange')), row=2, col=1)
    fig.add_trace(go.Scatter(x=ticker_data.index, y=signal_line, name='Signal Line', line=dict(color='black', dash='dot')), row=2, col=1)

    fig.update_layout(title_text=f"{ticker} Price, Bollinger Bands, and MACD", height=600)
    fig.show()

    # Save the figure as a PDF
    fig_path = os.path.join(pdf_dir, f"{selected_category}_{ticker}.pdf")
    fig.write_image(fig_path)

#Save a excel copy of data
with pd.ExcelWriter('market_data.xlsx') as writer:
    for category, datasets in data.items():
        for ticker, df in datasets.items():
            df.to_excel(writer, sheet_name=f"{category}_{ticker}")

#Combining pdfs into a single one for the plots generated
import PyPDF2
import os

# Directory where individual PDFs are stored
pdf_dir = 'pdf_figures'

# Create a PdfWriter object
pdf_writer = PyPDF2.PdfWriter()

# Iterate through all PDF files in the directory and add them to the writer
for filename in os.listdir(pdf_dir):
    if filename.endswith('.pdf'):
        filepath = os.path.join(pdf_dir, filename)
        pdf_reader = PyPDF2.PdfReader(filepath)
        for page in pdf_reader.pages:
            pdf_writer.add_page(page)

# Write out the combined PDF
with open('combined_figures.pdf', 'wb') as out:
    pdf_writer.write(out)


import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

# Create a PDF file with adjusted price plots
with PdfPages('market_plots.pdf') as pdf:
    for category, datasets in data.items():
        for ticker in datasets:
            # Example of a simple plot - you would recreate your Plotly plots here
            plt.figure()
            plt.plot(datasets[ticker])
            plt.title(f"{ticker} Price")
            plt.xlabel("Date")
            plt.ylabel("Price")
            pdf.savefig()  # saves the current figure
            plt.close()

