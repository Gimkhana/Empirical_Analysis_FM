import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt

#(1) Load data from CSV file into the FF variable
# Extracted from https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
# U.S. Research Returns Data (Downloadable Files), changes in CRSP Data, Fama/French 3 Factors (CSV file)  
#NB: Ensure you use the right path direction to properly run the python script

file_path = insert path file in the system ("...path/user/folder/name_file.CSV")
FF = pd.read_csv(file_path)


#(2) Load data from CSV file into the Factors (Fama-French FF 3 factor model) variable
Factors_data = pd.read_csv("...path/user/folder/F-F_Research_Data_Factors.CSV", skiprows=3)

# Convert the first (index 0) column to a string
Factors_data.iloc[:, 0] = Factors_data.iloc[:, 0].astype(str)

# Format the date column to yyyy-mm-01
Factors_data.iloc[:, 0] = Factors_data.iloc[:, 0].str[:4] + '-' + Factors_data.iloc[:, 0].str[4:] + '-01'

# Keep rows up to index 1164
Factors_data = Factors_data.iloc[:1165]  # Note that indexing is zero-based, so 1165 corresponds to the 1164th row.

# Convert specified columns to numeric and then divide by 100
columns_to_convert = ['Mkt-RF', 'SMB', 'HML', 'RF']
Factors_data[columns_to_convert] = Factors_data[columns_to_convert].apply(pd.to_numeric, errors='coerce') / 100


# Check the updated DataFrame
print(Factors_data.head())
print(Factors_data.tail())


# (3) Computation of the Fidelity fund return

Fidelity_returns = FF['Adj Close'].pct_change().dropna()


# (4) shrink the databse to accomodate the time series available for the fidelity fund and the factors 


# Ensure Fidelity_returns has 59 observations NB:make sure data is cleaned (no na, missing values) and ordered chronogically 
Fidelity_returns = Fidelity_returns[-59:]

# Trim Factors_data to match the length of Fidelity_returns
Factors_data = Factors_data.iloc[-59:]


# Reset the index of Fidelity_returns and Factors_data to ensure alignment
Fidelity_returns.reset_index(drop=True, inplace=True)
Factors_data.reset_index(drop=True, inplace=True)

# Specify the dependent variable (Fidelity returns)
Y = Fidelity_returns

# Specify the independent variables (Factors that are assessed in the regression)
independent_factors = ['Mkt-RF', 'SMB', 'HML', 'RF']
X = sm.add_constant(Factors_data[independent_factors])

# Perform the regression
model = sm.OLS(Y, X).fit()
print(model.summary())

# Create a scatter plot of Factors vs. Fidelity returns
plt.scatter(Factors_data['Mkt-RF'], Fidelity_returns, alpha=0.5)
plt.xlabel('Market-RF')
plt.ylabel('Fidelity Returns')
plt.title('Scatter Plot of Market-RF vs. Fidelity Returns')
plt.grid(True)
plt.show()

