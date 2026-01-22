## Fixed Income Market Dashboard

Real-time monitoring system for multi-asset fixed income markets: replicating institutional risk framework.

Six asset classes (FX, 2Y yields, 10Y yields, money market, equities/commodities, CDS spreads) ingested from Excel, normalized, and visualized with rolling statistics. Dashboard computes four views: (1) Reindexed levels (base 100, outliers clipped at 1st/99th percentiles) for cross-asset comparison, (2) Rolling ranges (min/max over sliding window) to identify mean-reversion targets, (3) Distributions (histogram + mean/median) for tail risk assessment, (4) Spreads (10Y-2Y tenor curves) for yield curve positioning. Automated with refinitiv Excel add-in for daily ingestion (replicated the framework from Amundi as portfolio risk vigilance tool, now generalized for any institutional asset class).

Application: Risk managers scan six charts in 60 seconds. Spot anomalies: Is EUR/USD at 6-month extreme? Is the 2Y spread compressing dangerously? Is CDS spiking (credit stress signal)? Rolling statistics remove subjective bias -> mean/percentile bands replace gut calls. 

Skill: pandas, NumPy, Matplotlib, Seaborn | Input: Excel multisheet timeseries | Output: Presentable dashboards