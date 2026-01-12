### CPPI vs. OBPI Portfolio Insurance Strategies
**Location**: [`Applied_Research_Framework_Git/Applied_research/CPPI_strategy_research`]

Monte Carlo simulation comparing Constant Proportion Portfolio Insurance (CPPI) and Option-Based Portfolio Insurance (OBPI) under Black-Scholes and Lévy jump-diffusion models.

**Insights**:
- The US 10-year Treasury bill yield averaged across the period analysed had a level close to 2.15% and a volatility in terms of yield level of 0.07%.
- The S&P 500 index stayed on average at a level close to 2742.69 and returned on average 11.22% with a volatility of 17.83%.
- During the same time frame, there was a -0.023 correlation coeﬃcientbetween the performance asset and the risk-free asset. This shows that having both assets in an investment portfolio could help diversify it.
- Lévy process shows times of abrupt increases or declines in portfolio value in contrast to the Black-Scholes model. These spikes may result from the model’s capacity to account for both minor and major moves, which aren’t always predicted by historical volatility or price trends.
- In contrast, these jumps are not taken into consideration by the Black-Scholes model. Its reliance on a lognormal price distribution undervalues the likelihood of signiﬁcant price ﬂuctuations, which may lead to a smoother curve but also understate risk.

**Technologies**: R, Monte Carlo simulation  
**Methods**: CPPI, OBPI, Black-Scholes, Lévy processes, GARCH volatility forecasting  
**Data**: S&P 500, US 10Y Treasury (2013-2023) extracted from Refinitiv Eikon

📄 [Published Paper](https://www.researchgate.net/publication/379753726_Comparative_analysis_of_CPPI_and_OBPI_portfolio_insurance_strategies_under_different_modelling_techniques_Black-Scholes_versus_Levy_jump_diffusion_process)
