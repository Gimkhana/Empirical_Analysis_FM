'''

#Update the code to accomodate for last year data

#Import data from yahoo finance
import yfinance as yf
# Specify the ticker symbol (e.g., Apple Inc. is AAPL)
ticker_symbol = "AAPL"

# Define the date range for the data
start_date = "2022-01-01"
end_date = "2022-01-12"

# Use yf.download() to fetch the data
df = yf.download(ticker_symbol, start=start_date, end=end_date)

import yfinance as yf

# Specify the ticker symbol (e.g., Apple Inc. is AAPL)
ticker_symbol = "AAPL"

# Define the date range for the data
start_date = "2022-01-01"
end_date = "2022-12-31"

# Use yf.download() to fetch the data
df = yf.download(ticker_symbol, start=start_date, end=end_date)

# Display the first few rows of the data
print(df.head())
'''





import yfinance as yf
import matplotlib.pyplot as plt

# Specify the ticker symbol (e.g., Apple Inc. is AAPL)
ticker_symbol = "BNP.PA"

# Define the date range for the data
start_date = "2022-01-01"
end_date = "2022-12-31"

# Use yf.download() to fetch the data
df = yf.download(ticker_symbol, start=start_date, end=end_date)

# Plotting the 'Close' price
plt.figure(figsize=(12, 6))  # Set the figure size (width, height)

# Extract the 'Close' column from the DataFrame and plot it as a line
plt.plot(df.index, df['Close'], label=ticker_symbol)

# Customize the plot
plt.title(f"{ticker_symbol} Stock Price")
plt.xlabel("Date")
plt.ylabel("Price (USD)")
plt.grid(True)
plt.legend()

# Show the plot
plt.show()




'''
#Update the code to accomodate for 10 year of data

import yfinance as yf
import matplotlib.pyplot as plt

# Specify the ticker symbol (e.g., Apple Inc. is AAPL)
ticker_symbol = "AAPL"

# Define the date range for the data (ten-year time frame)
start_date = "2013-01-01"
end_date = "2023-01-01"

# Use yf.download() to fetch the data
df = yf.download(ticker_symbol, start=start_date, end=end_date)

# Plotting the 'Close' price
plt.figure(figsize=(12, 6))  # Set the figure size (width, height)

# Extract the 'Close' column from the DataFrame and plot it as a line
plt.plot(df.index, df['Close'], label=ticker_symbol)

# Customize the plot
plt.title(f"{ticker_symbol} Stock Price (10-Year Time Frame)")
plt.xlabel("Date")
plt.ylabel("Price (USD)")
plt.grid(True)
plt.legend()

# Show the plot
plt.show()
'''


'''
#computation of returns

import yfinance as yf
import matplotlib.pyplot as plt

# Specify the ticker symbol (e.g., Apple Inc. is AAPL)
ticker_symbol = "AAPL"

# Define the date range for the data (ten-year time frame)
start_date = "2013-01-01"
end_date = "2023-01-01"

# Use yf.download() to fetch the data
df = yf.download(ticker_symbol, start=start_date, end=end_date)

# Calculate daily returns from adjusted closing prices
df['Daily_Return'] = df['Adj Close'].pct_change() * 100  # Calculate returns as a percentage

# Plotting the daily returns
plt.figure(figsize=(12, 6))  # Set the figure size (width, height)

# Extract the 'Daily_Return' column from the DataFrame and plot it as a line
plt.plot(df.index, df['Daily_Return'], label=f'{ticker_symbol} Daily Returns', color='b')

# Customize the plot
plt.title(f"{ticker_symbol} Daily Returns (10-Year Time Frame)")
plt.xlabel("Date")
plt.ylabel("Daily Returns (%)")
plt.grid(True)
plt.legend()

# Show the plot
plt.show()
'''


