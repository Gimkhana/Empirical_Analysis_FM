### **Empirical Analysis of Financial Markets: Factor Investing, Portfolio Management, and Quantitative Methods**

[![MIT License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Python](https://img.shields.io/badge/Python-3776AB?logo=python&logoColor=white)](https://www.python.org/)

#### **About this GitHub repository**

This repository documents quantitative research and applied projects in portfolio management, factor investing, and investment strategies. It combines academic rigor with institutional and academic insights.

- For practitioners: Reproducible frameworks for portfolio construction and investment strategies.
- For researchers: Well-documented methodologies with clear citations and data sources.
- This repository is also well suited for anyone looking to delve into data science and quantitative techniques applied to finance.

--- 

#### **About me**

Driven finance postgraduate with 20+ months of experience across institutional asset management (Amundi), commodity trading (PetroIneos), central banking (Bank Al-Maghrib), and client solutions analytics (FactSet). I hope you can find value in the research I share.

In a nutshell:

- Master's degree in Risk & Asset Management (M2 GRA) @ Paris-Saclay
- Master's degree in Energy Trade and Finance @ Bayes (formerly CASS) Business School, London
- Graduate in Business Studies (Finance concentration) @ ESSEC Business School
- 20+ months of experience across capital markets @ Central Bank of Morocco (BAM), commodity trading and market analysis @ PetroIneos Trading London, risk management @ Amundi AM, and client solutions analytics @ FactSet London

**Contact**
Email: youssef.louraoui@essec.edu
[LinkedIn](https://www.linkedin.com/in/youssef-louraoui/) | [SSRN](https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=4240493) | [ResearchGate](https://www.researchgate.net/profile/Youssef-Louraoui-2)

---

**Structure of this Github**

You will find the main repository structured into four sub-folders:

- Applied Projects: Covers different smaller projects on
  - Crude oil modelling (PetroIneos Trading independent project)
  - Machine learning forecasting (part of the Machine Learning module @ Paris-Saclay, instructor A. Bousabaa)
  - Portfolio management projects (Portfolio Management module @ Paris-Saclay, instructor J. Caicedo)

- Applied Research: Covers all the content shared for each paper
  - All-Weather portfolio
  - Factor investing during COVID-19
  - CPPI vs. OBPI strategy research
  - MONIA transition (presentation slides and dataset)
  - IPO underpricing project

- Quantitative Methods (part of the Quantitative Techniques in Finance @ Paris-Saclay, instructor A. Bousabaa; co-authored with Y. Bancé): Project that elaborates on
  - Option pricing
  - CDS calibration via Monte Carlo simulation

- Quantitative Portfolio Management: Project covering portfolio management techniques (part of the Asset Management module @ Paris-Saclay, instructor P. Clauss)

**Research Focus**: Factor investing, Portfolio Management, Investment Strategies

--- 

## 📂 Featured Projects

**1. Asymmetric Factor Volatility During COVID-19: Evidence for EGARCH over GARCH**
**Location**: [`Applied_Research_Framework_Git/Applied_research/Factor_investing_research`]

This paper documents a critical market inefficiency in factor-level volatility forecasting during systemic crises. 
Traditional symmetric GARCH(1,1) models, despite effectively capturing autocorrelation dynamics, systematically misspecify factor-level risk through two mechanisms: 
- (1) violation of normality assumptions creating extreme fat tails, and 
- (2) asymmetric shock responses

**Methodology** 
- Based on EDHEC Risk Institute paper (Hasaj, M. & Sherer, B., 2021; "Covid-19 and Smart-Beta: A Case Study on theRole of Sectors”. EDHEC-Risk Institute Working Paper., pp. 1-35.)
- We compare the performance of the funds to the VIX level during the Covid-19 period on 374 trading days.
- MSCI factors funds as a benchmark
- Based on Pagano (2020) COVID-19 timeframe decomposition 
  
**Insights**:
Analysing five US equity factors (Size, Value, Quality, Momentum, Minimum Volatility) and the S&P 500 Index from November 2019 to December 2021, we found: 

- (1) all factors exhibit significant asymmetric volatility drifts (γ = 0.388-0.554, p < 0.05), with defensive factors showing stronger asymmetry than Value
- (2) GARCH systematically underpredicts tail volatility by 40-260 basis points during crisis peaks
- (3) EGARCH reduces this misspecification by 20% when forecasting the Momentum factor and show overconfidence in prediction when assessing market aggregate (S&P 500) with statistically significant out-of-sample improvements (p < 0.001)
- The finding that defensive factors do not escape asymmetric responses challenges conventional portfolio diversification wisdom
- The findings presented in this paper suggest practitioners consider alternative models to capture volatility such as EGARCH compared to standard Gaussian GARCH model

**Technologies**: R, RStudio, rugarch 
**Methods**: Econometrics (GARCH modelling) 
**Data**: MSCI factor indices (2019-2021, Refinitiv Eikon)

📄 [Published Paper (SSRN)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4677442) | 100+ downloads

--- 

### 2. All Weather Portfolio Optimization
**Location**: [`Applied_Research_Framework_Git/Applied_research/All_weather_portfolio_research`]

Comparative analysis of portfolio construction strategies: Global Minimum Variance, Principal Component Analysis, Tangency Portfolio, and Black-Litterman optimization applied to multi-asset ETF portfolios (part of the Asset Management module @ Paris-Saclay, instructor P. Clauss)

**Insights**:
- Black-Litterman incorporating market views from BlackRock, Goldman Sachs, JPMorgan achieved highest Sharpe ratio (0.82)
- GMV allocation: 88.91% bonds, demonstrating risk-minimization trade-offs
- COVID-19 period analysis revealed correlation breakdown across asset classes

**Technologies**: R, tydiverse, quantmod, corrplot, DataExplorer, ggplot2  
**Methods**: Markowitz optimization, PCA, Black-Litterman,
**Data**: VTI, EMGF, IEF, DBC, GLD (2019-2023, Refinitiv Eikon)

---

### 3. CPPI vs. OBPI Portfolio Insurance Strategies
**Location**: [`Applied_Research_Framework_Git/Applied_research/CPPI_strategy_research`]

Monte Carlo simulation comparing Constant Proportion Portfolio Insurance (CPPI) and Option-Based Portfolio Insurance (OBPI) under Black-Scholes and Lévy jump-diffusion models (part of the Financial Insurance module @ Paris-Saclay, instructor C. Benezet)

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

---

### 4. Slides for MONIA Forward Rate Curve Construction
**Location**: [`Applied_Research_Framework_Git/Applied_research/Money_market_research_BAM`]

Development of forward rate model for Moroccan interbank market (MONIA) to support Bank Al-Maghrib's LIBOR transition framework (slides presented for undergraduate research project).

**Insights**:
- Construction of the forward yield curve using alternative reference rates has not seen the light of day at no central bank until now (as of July 2021).
- Paradigm shift in methodology: from a declarative-based rate to an eﬀective-based rate.
- Moroccan market limited in terms of maturity.
- Limiting assumption: the rate is diﬃcult to model for maturities of more than one year.
- The 12-month rate explodes despite its risk-free nature.
- Impact of Covid-19 on results.
- Research work at a germinal stage.

**Technologies**: Microsoft Excel 
**Methods**: Compounding in arrears, forward rate calculations, sensitivity analysis  
**Data**: MONIA repo rates (2020-2021, Bank Al-Maghrib)

📄 [Slides](https://www.researchgate.net/publication/376477514_Construction_of_a_forward_rate_model_for_the_Moroccan_interbank_market) (Co-authored with A. Feral, PhD & A. Rafiki, Bank Al-Maghrib)

---

## Technical Skills

**Programming**: RStudio, Python, LaTeX  
**Finance**: Bloomberg Terminal, Refinitiv Eikon, FactSet
**Finance**: Market Research, Academic Research, Market Analysis

---

## Publications & Research

📄 [View all publications on SSRN](https://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=4240493)
📄 [View all publications on Research Gate](https://www.researchgate.net/profile/Youssef-Louraoui-2)

---

## Getting Started

### Prerequisites
- **R** (≥4.0): Download from [CRAN](https://cran.r-project.org/)
- **Python** (≥3.8): Download from [Python.org](https://www.python.org/)
- **RStudio** (recommended): Download from [RStudio](https://posit.co/products/open-source/rstudio/)
  
### Installation

Clone the repository
git clone https://github.com/Gimkhana/Empirical_Analysis_FM.git
cd Empirical_Analysis_FM

---

### Usage

Each project folder contains:
- **Code notebooks** for replicable analysis with comments
- **Data/** with sample datasets or instructions to obtain data
- **Output/** generated figures, tables, and results

---

## Contributions

Contributions are welcome! Whether you want to:
- **Improve existing models** (add robustness checks, alternative specifications)
- **Add new projects**
- **Fix bugs or enhance documentation**

---

## License

This project is licensed under the **MIT License** - see the [LICENSE] file for details.
You are free to use, modify, and distribute this code for academic, personal, or commercial purposes with attribution.

---

## Acknowledgments

- Special mention to **P.Clauss** for introducing me to Github and leveraging it as a portfolio document for showcasing work and also for his unwaring support during the Asset Management module @ Paris-Saclay
- **ESSEC Business School**, **Bayes (formerly CASS) Business School** and **Université Paris-Saclay** faculty for research guidance
- **Bank Al-Maghrib** for MONIA data access and collaboration
  
---

*Last updated: November 2025*
