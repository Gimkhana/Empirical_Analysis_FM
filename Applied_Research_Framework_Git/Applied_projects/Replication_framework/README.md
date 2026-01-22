## Kalman Filtering & OLS-Based Atypical Performance Detection

Identifying fund alpha through dynamic factor models replicating Roncalli & Teiletche (2007) framework.

Compare fund returns against S&P 500 + 11 sector benchmarks using two estimation methods: 

- (1) OLS regression (static factor loadings) decomposes returns into beta exposure and alpha (permanent outperformance)
- (2) Kalman filter (dynamic loadings) detects time-varying exposures when funds shift strategy mid-period. 

Feed 11 sector indices into both models; OLS identifies persistent alpha; Kalman flags tactical allocation shifts. Visualize residuals to spot outlier dates which are considered days when fund diverged dramatically from model prediction -> signal manager skill or style drift.

Application: Portfolio managers screen 50+ funds daily. Which outperformed their factor model? OLS catches persistent alpha-generators (skill). Kalman catches momentum traders (style drift). Residual spikes > 2σ trigger deep dives.

Tech: NumPy, SciPy (Kalman implementation), statsmodels (OLS) | Data: Fund daily returns + 11 sector indices | Output: Alpha coefficient, residual plots, Kalman state trajectory