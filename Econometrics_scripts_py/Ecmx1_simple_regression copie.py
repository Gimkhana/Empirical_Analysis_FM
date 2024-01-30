import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt

# (a) Define constants
N = 50
a = 2
b = 3

# (b) Generate error term e
np.random.seed(0)  # For reproducibility
e = np.random.normal(0, 1, N)  # Simulate N random errors from a normal distribution

# (c) Generate random variable X
X = np.random.uniform(0, 10, N)  # Simulate N random values between 0 and 10

# (d) Create variable Y
Y = a + b * X + e

# (e) Perform linear regression
X_with_const = sm.add_constant(X)  # Add a constant term for the intercept
model = sm.OLS(Y, X_with_const).fit()  # Fit the linear regression model
print(model.summary())  # Display regression summary

plt.plot(X,Y,linestyle = 'none', marker = 'o')

# (f) Modify N and perform regression again
Ns = [100, 1000, 5000]  # List of new N values

for new_N in Ns:
    e = np.random.normal(0, 1, new_N)  # Generate new errors
    X = np.random.uniform(0, 10, new_N)  # Generate new X values
    Y = a + b * X + e  # Recreate Y
    
    X_with_const = sm.add_constant(X)
    model = sm.OLS(Y, X_with_const).fit()
    
    print(f"\nRegression results for N={new_N}:")
    print(model.summary())

# (g) Analyze the estimates αˆ and βˆ as N becomes larger
# As N becomes larger, the estimates αˆ and βˆ should converge to their true values a and b (2 and 3 in this case).
