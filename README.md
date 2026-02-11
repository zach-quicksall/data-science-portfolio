# Data Science Portfolio

This repository contains selected data science projects focused on real-world modeling problems, with an emphasis on **decision-making under uncertainty**, **systems dynamics**, and **applied machine learning**.

The projects span domains including healthcare, marketplaces, fraud detection, marketing, natural language processing, and genomics, and are designed to reflect how data science is practiced in production and research-adjacent settings: with attention to data quality, modeling assumptions, interpretability, and operational tradeoffs.

---

## Projects

### Equity Pairs Selection (Russell 2000)

**Directory:** `equity_pair_selection`

A research-focused pipeline for identifying robust equity pairs suitable for statistical arbitrage, using constituents of the Russell 2000. This project emphasizes **pair selection and validation**, rather than backtest optimization or execution, with a strong focus on **out-of-sample stability** and **economic coherence**.

**Key themes:**  
`time-series` · `statistical arbitrage` · `financial markets` · `mean reversion` · `out-of-sample validation`

---

### Equity Pairs Trading (Russell 2000)

**Directory:** `equity_pair_trading`

Building upon the content of `equity_pair_selection`, this simulation provides out-of-sample performance metrics for a simple z-score based trading algorithm.

*Key themes:**  
`time-series` · `statistical arbitrage` · `financial markets` · `mean reversion` · `out-of-sample validation` · `portfolio construction`

---

### ECG Beat Classification  
**Directory:** `ecg_beat_classification`

Supervised classification of ECG heartbeats using time-series signal data.  
This project focuses on feature engineering for biomedical signals, handling class imbalance, and evaluating models using clinically relevant performance metrics rather than overall accuracy alone.

**Key themes:**  
`time-series` · `machine learning` · `healthcare data` · `class imbalance` · `model evaluation`

---

### Fraud Detection Modeling  
**Directory:** `fraud_detection_modeling`

An end-to-end fraud detection pipeline designed for highly imbalanced data.  
The analysis emphasizes PR-AUC optimization, threshold selection, review-rate tradeoffs, and decision metrics aligned with real-world fraud operations.

**Key themes:**  
`imbalanced classification` · `XGBoost` · `decision thresholds` · `precision–recall` · `operational metrics`

---

### Housing Market Dynamics  
**Directory:** `housing_market_dynamics`

Exploratory modeling of housing market behavior with attention to temporal structure, supply dynamics, and price evolution.  
The project examines how market indicators converge and diverge over time and highlights structural patterns relevant to economic interpretation.

**Key themes:**  
`econometrics` · `market dynamics` · `time series` · `exploratory modeling`

---

### Marketing Campaign Effectiveness  
**Directory:** `marketing_campaign_effectiveness`

Analysis of marketing campaign performance and customer conversion behavior.  
The project focuses on diminishing returns, customer fatigue, and tradeoffs between campaign intensity and expected conversion outcomes, with an emphasis on decision-relevant metrics.

**Key themes:**  
`customer analytics` · `causal reasoning` · `experimentation` · `marketing optimization`

---

### Scientific Abstract Topic Modeling  
**Directory:** `scientific_abstract_topic_modeling`

Unsupervised topic modeling and semantic analysis of scientific paper abstracts.  
Applies modern NLP techniques to uncover latent thematic structure in large text corpora and explores multiple embedding, clustering, and visualization approaches.

**Key themes:**  
`NLP` · `topic modeling` · `embeddings` · `clustering` · `t-SNE` · `UMAP` · `dimensionality reduction`

---

### Urban Congestion Dynamics  
**Directory:** `urban_congestion_dynamics`

Modeling congestion and breakdown regimes in an urban transportation marketplace.  
Includes regime detection, simulation-based intervention strategies, and analysis of system stability under supply-side changes, with an emphasis on operational decision-making.

**Key themes:**  
`systems modeling` · `simulation` · `congestion dynamics` · `empirical Bayes` · `urban mobility`

---

### Cross-Tissue Differential Gene Expression  
**Directory:** `tissue_differential_expression`

This project investigates differential gene expression patterns across human tissues using RNA-seq data, with an emphasis on robust normalization and cross-tissue comparability. Expression values are normalized using Conditional Quantile Normalization (CQN) to account for gene length, GC content, and library size effects prior to differential expression analysis.

The analysis focuses on identifying tissue-driven expression differences, evaluating the impact of normalization choices, and producing interpretable visual summaries of differential expression results across multiple tissues.

**Key themes:**  
`gene expression` · `differential expression` · `normalization` · `CQN` · `RNA-seq` · `EDA`

---

### Cross-Tissue Weighted Gene Co-expression Network Analysis  
**Directory:** `tissue_wgcna`

This project applies Weighted Gene Co-expression Network Analysis (WGCNA) to CQN-normalized RNA-seq data to identify modules of co-expressed genes across tissues. Rather than focusing on individual genes, the analysis emphasizes module-level structure, eigengene summaries, and relationships between co-expression patterns and biological covariates such as tissue, age, and sex.

The workflow includes network construction, module detection, eigengene-based characterization, module–trait association analysis, and diagnostic assessment of potential technical batch effects. Results are presented using interpretable module-level visualizations rather than dense gene-level networks.

**Key themes:**  
`gene expression` · `network analysis` · `WGCNA` · `RNA-seq` · `gene modules` · `module eigengenes`

---
