# Applied Research Framework
**Applied Research Framework Repository**

Welcome to the Applied Research Framework Repository – a comprehensive suite of analytical tools and scripts. This repository houses a diverse array of methods and strategies in portfolio management and econometrics, implemented in R and Python, to aid in the construction, analysis, and optimization of investment portfolios. Part of the scripts are based on lectures and projects taught in the M2 Risk and Asset Management (Paris-Saclay).

**Repository Structure**

The repository is organized into three main folders:

**1. Asset management**

This folder contains a variety of advanced portfolio management techniques and strategies, each implemented in R:

- Global Minimum-Tangency Portfolio: Scripts for constructing portfolios that aim to achieve the lowest possible risk for a given level of expected return, utilizing the Markowitz efficient frontier concept.
- PCA (Principal Component Analysis): PCA is used here for dimensionality reduction in financial datasets, aiding in the identification of key factors that drive asset returns.
- Black-Litterman Model: Implementation of the Black-Litterman model which combines market equilibrium and subjective views to produce optimized asset allocations.
- Option-Based Portfolio Insurance: Scripts that implement strategies for protecting a portfolio against downside risk using options, a crucial technique for risk-averse investors.

**2. Econometrics**

This folder focuses on econometric analysis, featuring:

- Regressions: A collection of scripts for running various types of regression analyses, essential for understanding relationships between financial variables.
- ADF (Augmented Dickey-Fuller Test): Implementation of the ADF test to check the stationarity of time series, a critical aspect in many econometric analyses in finance.

**3. All-Weather portfolio project**

- An integrated project that applies various tools and scripts provided in this repository to construct an "All-Weather" portfolio.
- This project demonstrates how to build a portfolio that aims to perform well across different economic environments, using diversification strategies and insights from the asset management and econometrics folders.

**4. Numerical Methods in Finance**

- This folder is strcutured into two main projects: Credit derivatives modelling and option pricing using numerical methods.
- Both projects are coded in Python and use Jupyter Notebook to elaborate on the results.

**Getting started with R**

To get started, clone this repository and navigate to each folder to find detailed instructions and documentation for each script and project. The code is primarily written in R, so ensure you have R and the necessary packages installed.

- git clone https://github.com/your-username/portfolio-management-collection.git
- cd portfolio-management-collection

**Getting started with Python**

In addition to our R scripts, you may find that Python can be a powerful tool for data analysis, particularly in areas of portfolio management and econometrics. Here are some steps to get you started with Python:

**Prerequisites**

Ensure you have Python installed on your system. We recommend using the Anaconda distribution, which includes Python, the Jupyter Notebook, and other commonly used packages for scientific computing and data science.

- Download Anaconda: Anaconda Distribution

**Setting up your Python environment** 

Create a new environment (Optional but Recommended):

- conda create --name finance python=3.8
- conda activate finance

**Install necessary packages:**

- NumPy for numerical computing.
- pandas for data manipulation and analysis.
- matplotlib for plotting.
- Scikit-learn for machine learning (if applicable).

You can install these packages using the following command:

- conda install numpy pandas matplotlib scikit-learn

**Running Python scripts**

If you have Python scripts in your repository, navigate to their directory:

- cd path/to/python/scripts

Run a script using:

- python script_name.py

**Using Python with R**

For projects where you wish to combine the power of R and Python, the rpy2 package in Python provides an interface between the two languages. Install rpy2 using:

- conda install rpy2

**Jupyter Notebook**

Use Jupyter Notebooks for an interactive environment where you can run both R and Python code cells. This is particularly useful for exploratory data analysis.

To start Jupyter Notebook, run: 

- jupyter notebook
- Navigate to your notebook file (.ipynb) and open it to run Python and R code interactively.

This guide will help you set up a Python environment that complements the R-based tools in this repository, offering a broader scope for data analysis and portfolio management tasks.

**Contributing**

We welcome contributions and suggestions! Reach out at b00726728@essec.edu for any feedback.

**License**

This project is licensed under the MIT License - see the LICENSE file for details.
