import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt


#Data from the Refinitiv Eikon for russian loading patterns and other macro variables
file_path = "/Users/youssef/Library/CloudStorage/OneDrive-UniversiteEvryVald'Essonne/Google Drive/Mon Drive/Research/Models/Petroineos Trading Ltd/Models/Regression_Urals_loading_analysis_mod.xlsx"
Russian_data = pd.read_excel(file_path, sheet_name= "Expanded_regression", skiprows=2, usecols="B:H")

# Convert the 'Date' column to datetime and set it as the index
Russian_data['Date'] = pd.to_datetime(Russian_data['Date'])
Russian_data.set_index('Date', inplace=True)

# Drop the last six rows if they are empty or not needed
Russian_data = Russian_data[:-6]

# Drop any rows with NaN values
Russian_data.dropna(inplace=True)

# Compute the returns for all columns that represent asset prices
# Assuming 'Urals loading' is one of the columns and it represents asset prices, not the 'Date'
Russian_data_returns = Russian_data.pct_change().dropna()

# Define the dependent variable (e.g., 'Urals loading')
Y = Russian_data_returns['Urals loading']

# Define the independent variables
independent_factors = ['LCOc1', 'RSX']
X = sm.add_constant(Russian_data_returns[independent_factors])

# Check for any remaining NaN or infinite values in 'X' and 'Y'
if X.isnull().values.any() or np.isinf(X.values).any() or Y.isnull().any() or np.isinf(Y.values).any():
    raise ValueError("X or Y contains NaN or infinite values, which must be handled before regression.")

# Perform the regression
model = sm.OLS(Y, X).fit()
print(model.summary())

# Create scatter plots for each independent factor
for factor in independent_factors:
    plt.figure(figsize=(10, 6))
    plt.scatter(Russian_data_returns[factor], Y, alpha=0.5)
    plt.xlabel(factor)
    plt.ylabel('Urals loading Returns')
    plt.title(f'Scatter Plot of {factor} vs. Urals loading Returns')
    plt.grid(True)
    plt.show()
