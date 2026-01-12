**Asymmetric Factor Volatility During COVID-19: Evidence for EGARCH over GARCH**
**Location**: [`Applied_Research_Framework_Git/Applied_research/Factor_investing_research`]

This paper documents a critical market inefficiency in factor-level volatility forecasting during systemic crises. 
Traditional symmetric GARCH(1,1) models, despite effectively capturing autocorrelation dynamics, systematically misspecify factor-level risk through two mechanisms: 
- (1) violation of normality assumptions creating extreme fat tails, and 
- (2) asymmetric shock responses

**Methodology** 
- Based on EDHEC Risk Institute paper (Hasaj, M. & Sherer, B., 2021; "Covid-19 and Smart-Beta: A Case Study on the Role of Sectors”. EDHEC-Risk Institute Working Paper., pp. 1-35.)
- We analyse the statistical returns of MSCI US equity factors during the Covid-19 period on 374 trading days
- We compare the results of volatility modeling using standard GARCH and EGARCH analysis.
- S&P500 as US equity benchmark
- Based on Pagano (2020) COVID-19 timeframe decomposition 
  
**Insights**:
Analysing five US equity factors (Size, Value, Quality, Momentum, Minimum Volatility) and the S&P 500 Index from November 2019 to December 2021, we found: 

- (1) all factors exhibit significant asymmetric volatility drifts (γ = 0.388-0.554, p < 0.05), with defensive factors showing stronger asymmetry than Value
- (2) GARCH systematically underpredicts tail volatility by 40-260 basis points during crisis peaks
- (3) EGARCH reduces this misspecification by 20% when forecasting the Momentum factor and show overconfidence in prediction when assessing market aggregate (S&P 500) with statistically significant out-of-sample improvements (p < 0.001)
- Defensive factors do not escape asymmetric responses challenges conventional portfolio diversification wisdom
- The findings presented in this paper suggest practitioners consider alternative models to capture volatility such as EGARCH compared to standard Gaussian GARCH model

**Technologies**: R, RStudio, rugarch 
**Methods**: Econometrics (GARCH modelling) 
**Data**: MSCI factor indices (2019-2021, Refinitiv Eikon)

📄 [Published Paper (SSRN)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4677442) | 100+ downloads
