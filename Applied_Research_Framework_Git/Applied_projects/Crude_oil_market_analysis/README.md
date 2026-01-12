### ML forecasting: An application to Urals loading 
**Location**: [`Applied_Research_Framework_Git/Applied_projects/Crude_oil_market_analysis`]

The project aim to answer the following question: can we build a generalizable ML model that works across normal AND crisis periods, using only economically independent variables?

**Insights**:
- Wide gap between the models tested (0.521 → 1.371 mbbl/d deviations in MAE) MLP outperforms LSTM by 25%. 
- By removing multicollinear features, MLP now beats LSTM by 58%
- MLP (3 layers) outperforms LSTM despite simpler architecture, because it trains on independent, interpretable features.

**Dataset**: 
- 234 weekly observations (2017-2021)
- Target: Urals crude loading volumes (mb/d)
- Training: 164 observations (70%)
- Testing: 70 observations (30%)
- Normalization: Z-score (+/− 3 σ threshold)
- Source: We tested two dfferrent set of features in the ML model using Reﬁnitiv Eikon, Petrologistics datasets

**Technologies**: Jupyter Notebook (Python script) 
**Methods**: ML techniques