# Step 1: Import necessary libraries
import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller

# Step 2: Read the dataset
# Replace 'path_to_Canada_mod_xlsx' with the actual path to your CSV file.
file_path = "...pathto/folder/Canada_mod.xlsx"
df = pd.read_excel(file_path)
print(df)

# Function to convert date to year and quarter format
def convert_date(x):
    year, decimal = str(x).split('.')
    quarter = str(int(float('0.' + decimal) * 4 + 1))
    return year + 'Q' + quarter

df['Date'] = df['Date'].apply(convert_date)

# Display the modified DataFrame
print(df.head())

# Step 3: Plot the variables
plt.figure(figsize=(10, 8))

# Subplot for productivity
plt.subplot(2, 2, 1)
plt.plot(df['prod'], label='Productivity')
plt.title('Productivity')
plt.legend()

# Subplot for employment
plt.subplot(2, 2, 2)
plt.plot(df['e'], label='Employment', color='orange')
plt.title('Employment')
plt.legend()

# Subplot for unemployment rate
plt.subplot(2, 2, 3)
plt.plot(df['U'], label='Unemployment Rate', color='green')
plt.title('Unemployment Rate')
plt.legend()

# Subplot for real wages
plt.subplot(2, 2, 4)
plt.plot(df['rw'], label='Real Wages', color='red')
plt.title('Real Wages')
plt.legend()

plt.tight_layout()
plt.show()

# Step 4: Run tests of stationarity
variables = ['prod', 'e', 'U', 'rw']
for var in variables:
    result = adfuller(df[var])
    print(f'ADF Statistic for {var}: {result[0]}')
    print(f'p-value for {var}: {result[1]}')
    if result[1] > 0.05:
        print(f"{var} is not stationary")
    else:
        print(f"{var} is stationary")
    print('-----------------------------------------------')

# Step 5: Transform the variables in differences and determine the degree of integration
df_diff = df[['e', 'prod', 'rw', 'U']].pct_change().dropna()

# Run ADF test again on the differenced data
for var in variables:
    result = adfuller(df_diff[var])
    print(f'ADF Statistic for differenced {var}: {result[0]}')
    print(f'p-value for differenced {var}: {result[1]}')
    if result[1] > 0.05:
        print(f"Differenced {var} is still not stationary, might need further differencing")
    else:
        print(f"Differenced {var} is stationary, it is integrated of order 1")
    print('-----------------------------------------------')
