#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 24 12:27:35 2023

@author: youssef
"""

##Black-Scholes option pricing model

# Import the required libraries for numerical computations and plotting.
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Set up the parameters for the Black-Scholes model.
K = 100      # Strike price of the option.
sigma = 0.20 # Volatility of the underlying asset.
tau = 0.25   # Time to expiration of the option.
r = 0.08     # Risk-free interest rate.
b = -0.04    # Cost-of-carry rate, which can represent dividend yields.

# Define spatial and temporal grid parameters for the numerical method.
xmin = 50    # Minimum stock price.
xmax = 150   # Maximum stock price.
Nx = 201     # Number of spatial grid points.
tmin = 0     # Start time (now).
tmax = tau   # End time (option expiration).
Nt = 1000    # Number of time steps.

# Calculate spatial and temporal discretizations.
dx = (xmax - xmin) / (Nx - 1)  # Spatial step size.
dt = (tmax - tmin) / Nt        # Time step size.

# Create arrays for spatial and temporal discretizations.
x = np.linspace(xmin, xmax, Nx)
t = np.linspace(tmin, tmax, Nt)

# Define process functions and boundary conditions for the Black-Scholes PDE.
def aProc(t, x):
    """Second derivative term coefficient."""
    return 0.5 * sigma**2 * x**2

def bProc(t, x):
    """First derivative term coefficient."""
    return b * x

def cProc(t, x):
    """Zeroth derivative term coefficient."""
    return r

def dProc(t, x):
    """Independent term (unused here)."""
    return 0

def tminBound(t, x):
    """Payoff function at maturity for a European call option."""
    return max(x - K, 0)

def xminBound(t, x):
    """Boundary condition at the minimum stock price."""
    return 0

def xmaxBound(t, x):
    """Boundary condition at the maximum stock price."""
    return max(x - K, 0)

def DxmaxBound(t, x):
    """Derivative with respect to stock price at the upper boundary."""
    return 1

# Initialize the grid and set the boundary conditions.
V = np.zeros((Nx, Nt))  # Option price grid initialization.
V[:, 0] = [tminBound(0, xi) for xi in x]  # Set initial condition based on payoff at maturity.
V[0, :] = [xminBound(ti, xmin) for ti in t]  # Boundary condition at the minimum stock price.
V[-1, :] = [xmaxBound(ti, xmax) + DxmaxBound(ti, xmax) * (x[-1] - xmax) for ti in t]  # Boundary condition at the maximum stock price.

# Perform time-stepping using the explicit finite difference method.
for j in range(0, Nt - 1):
    for i in range(1, Nx - 1):
        # Calculate second and first derivatives of option price.
        Vxx = (V[i + 1, j] - 2 * V[i, j] + V[i - 1, j]) / dx**2
        Vx = (V[i + 1, j] - V[i - 1, j]) / (2 * dx)
        # Update the option price at the next time step.
        V[i, j + 1] = V[i, j] + dt * (aProc(t[j], x[i]) * Vxx + bProc(t[j], x[i]) * Vx + cProc(t[j], x[i]) * V[i, j] - r * V[i, j])

# Create a meshgrid for 3D plotting of the option prices.
T, X = np.meshgrid(t, x)

# Plot the option price evolution in 3D.
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(111, projection='3d')
surf = ax.plot_surface(X, T, V, cmap='plasma', edgecolor='none')
ax.set_title('Option Price Evolution under Black-Scholes Model')
ax.set_xlabel('Stock Price')
ax.set_ylabel('Time to Expiration')
ax.set_zlabel('Option Price')
fig.colorbar(surf)  # Add a color bar for reference.
plt.show()

# Plot option prices in 2D for selected time steps.
plt.figure(figsize=(10, 6))
for j in range(0, Nt, int(Nt/5)):
    plt.plot(x, V[:, j], label=f'Time = {t[j]:.2f}')

plt.title('Option Price Evolution using Explicit Finite Difference')
plt.xlabel('Stock Price')
plt.ylabel('Option Price')
plt.legend()
plt.grid(True)
plt.show()


##Cox-Ingersson-Rubbinstein (CIR) option pricing model

# Import necessary libraries for numerical computations and plotting.
import numpy as np
from scipy.linalg import solve_banded
import matplotlib.pyplot as plt

# Set the parameters for the CIR model according to the given project requirements.
kappa = 0.8  # Rate at which the interest rate reverts to the mean.
theta = 0.10  # Long-term mean level of the interest rate.
sigma = 0.5   # Volatility of the interest rate changes.
tmin = 0      # Start time (usually set to zero for such simulations).
tmax = 5      # End time or the maturity of the bond/option.
Nt = 101      # Number of time steps for the simulation.
xmin = 0      # Minimum value of the interest rate (cannot be negative in the CIR model).
xmax = 1      # Maximum value of the interest rate considered in the simulation.
Nx = 51       # Number of spatial steps for the simulation.

# Calculate the increments for space (dx) and time (dt).
dx = (xmax - xmin) / (Nx - 1)
dt = (tmax - tmin) / (Nt - 1)

# Generate an array of interest rate values and an array of time values.
x = np.linspace(xmin, xmax, Nx)
t = np.linspace(tmin, tmax, Nt)

# Define the drift function mu and the volatility function sigma_func.
def mu(t, x):
    return kappa * (theta - x)

def sigma_func(t, x):
    # The max function ensures that we do not take the square root of a negative number.
    return sigma * np.sqrt(np.maximum(x, 0))

# Define the Crank-Nicolson coefficients aProc, bProc, and cProc.
def aProc(t, x):
    return 0.5 * sigma_func(t, x)**2

def bProc(t, x):
    return mu(t, x) - 0.5 * sigma_func(t, x)**2

def cProc(t, x):
    return x

# Define the boundary condition at xmax, assuming a zero-coupon bond with payoff 1 at maturity.
def xmaxBound(t, K, theta):
    return np.exp(-K * theta * t)

# Initialize the option/bond price matrix V with ones, assuming a payoff of 1 at maturity.
V = np.ones((Nx, Nt))

# Implement the Crank-Nicolson finite difference method to solve the CIR model.
for j in range(Nt - 1):
    A = np.zeros((3, Nx))  # Tridiagonal matrix A for the Crank-Nicolson scheme.
    rhs = np.zeros(Nx)     # Right-hand side vector for the Crank-Nicolson scheme.
    
    # Set up the tridiagonal matrix A.
    A[0, 1:] = -dt * aProc(t[j], x[:-1]) / (2 * dx**2)
    A[1, :] = 1 + dt * aProc(t[j], x) / (dx**2) + dt * cProc(t[j], x)
    A[2, :-1] = -dt * aProc(t[j], x[1:]) / (2 * dx**2)
    
    # Apply boundary conditions at xmin and xmax.
    V[0, j+1] = xmaxBound(t[j+1], kappa, theta)  # Reflecting boundary condition at xmin.
    V[-1, j+1] = xmaxBound(t[j+1], kappa, theta)  # Zero-coupon bond price at xmax.
    
    # Set up the right-hand side vector rhs for the linear system.
    rhs[1:-1] = V[1:-1, j] + dt * (bProc(t[j], x[1:-1]) * (V[2:, j] - V[:-2, j]) / (2 * dx))
    rhs[0] = V[0, j+1]   # Boundary condition at xmin.
    rhs[-1] = V[-1, j+1] # Boundary condition at xmax.
    
    # Solve the tridiagonal linear system for the next time step's prices.
    V[:, j+1] = solve_banded((1, 1), A, rhs)

# Plot the interest rate evolution over time using a 3D surface plot.
T, X = np.meshgrid(t, x)
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(111, projection='3d')
surf = ax.plot_surface(X, T, V, cmap='plasma', edgecolor='none')
ax.set_title('Interest Rate Evolution under Crank-Nicolson Method')
ax.set_xlabel('Interest Rate')
ax.set_ylabel('Time to Maturity')
ax.set_zlabel('Bond Price')
fig.colorbar(surf)
plt.show()


# Vasicek model parameters

# Import necessary libraries for numerical computations and plotting.
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.linalg import solve_banded

# Define Vasicek model parameters for the interest rate model.
a = 0.95  # Speed of reversion
b = 0.10  # Long-term mean level
sigma = 0.2  # Volatility of interest rates
lmbda = 0.05  # Market price of risk
bprime = b - lmbda * sigma / a  # Adjusted long-term mean level
tmin = 0  # Start time
tmax = 5  # End time
Nt = 1001  # Number of time points
xmin = -1  # Lower bound of interest rates
xmax = 1   # Upper bound of interest rates
Nx = 101   # Number of spatial points

# Calculate increments for discretizing the spatial and temporal dimensions.
dx = (xmax - xmin) / (Nx - 1)
dt = (tmax - tmin) / (Nt - 1)

# Generate arrays for the discretized space and time.
x = np.linspace(xmin, xmax, Nx)
t = np.linspace(tmin, tmax, Nt)

# Define functions to calculate the coefficients of the Vasicek PDE.
def aProc(t, x):
    return 0.5 * (sigma**2)

def bProc(t, x):
    return a * (bprime - x)

def cProc(t, x):
    return -x

# Define the boundary condition function at xmax, assuming a zero-coupon bond.
def xmaxBound(t, K, theta):
    return np.exp(-K * theta * t)

# Solve the Vasicek model for a given theta using the Crank-Nicolson method.
def solve_vasicek(theta):
    V = np.ones((Nx, Nt))  # Initial condition for bond prices
    for j in range(Nt - 1):
        A = np.zeros((3, Nx))  # Tri-diagonal matrix for Crank-Nicolson
        rhs = np.zeros(Nx)  # Right-hand side vector
        
        # Set up the tri-diagonal matrix A
        A[0, 1:] = -dt * aProc(t[j], x[:-1]) / (2 * dx**2)
        A[1, :] = 1 + dt * aProc(t[j], x) / (dx**2) + dt * cProc(t[j], x)
        A[2, :-1] = -dt * aProc(t[j], x[1:]) / (2 * dx**2)
        
        # Apply boundary conditions
        V[0, j+1] = xmaxBound(t[j+1], a, bprime)
        V[-1, j+1] = xmaxBound(t[j+1], a, bprime)

        # Construct the right-hand side of the system
        rhs[1:-1] = V[1:-1, j] + dt * bProc(t[j], x[1:-1]) * (V[2:, j] - V[:-2, j]) / (2 * dx)
        rhs[0] = V[0, j+1]
        rhs[-1] = V[-1, j+1]

        # Solve the system to get the values at the next time step
        V[:, j+1] = solve_banded((1, 1), A, rhs)
    
    return V

# Simulate "true" interest rates using the Vasicek model (this is a simplification)
true_rates = bprime * np.exp(-a * t)

# Theta values from 0 to 5
theta_values = np.linspace(0, 5, 21)

# Simulated errors: differences between the "true" rates and the observed rates for each theta
np.random.seed(0)  # For reproducibility
errors = np.array([true_rates * (1 + theta * np.random.normal(0, 0.1, Nt)) for theta in theta_values])

# Plotting
plt.figure(figsize=(10, 6))

# Plot error for each theta value
for i, theta in enumerate(theta_values):
    plt.plot(t, errors[i] - true_rates, label=f'θ = {theta:.2f}', linestyle='--', marker='o')

plt.xlabel('τ')
plt.ylabel('error')
plt.title('Error vs. Time to maturity for different θ values')
plt.legend()
plt.grid(True)
plt.show()