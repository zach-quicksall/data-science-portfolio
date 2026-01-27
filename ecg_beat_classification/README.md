# Real-time arrhythmia detection under asymmetric clinical risk and alert-fatigue constraints

## Executive Summary

This project explores beat-level ECG classification in the context of continuous cardiac monitoring, where machine learning models must operate under asymmetric clinical risk, alarm fatigue constraints, and strict reliability requirements.

Using the MIT-BIH Arrhythmia Database (sourced from PhysioNet), I develop and evaluate a beat-level classification model while explicitly addressing the limitations of accuracy-driven evaluation, the risks of dataset bias, and the challenges of deploying ECG models in real-world clinical or medical-device settings.

Rather than optimizing for headline performance metrics, this project focuses on decision-aware evaluation, failure modes, and governance considerations required for safe deployment.

## Problem Context and Clinical Motivation

Continuous ECG monitoring systems support early detection of potentially life-threatening arrhythmias. However:

- False negatives may delay treatment and cause patient harm
- False positives contribute to alarm fatigue, clinician distrust, and system abandonment
- Real-world ECG signals are noisy, heterogeneous, and subject to drift over time

This creates a high-stakes decision environment where model performance must be evaluated relative to operational constraints, not just classification accuracy.

Goal: Design and evaluate a beat-level ECG classification system that acknowledges clinical risk, operational tradeoffs, and deployment realities.

## Dataset

MIT-BIH Arrhythmia Database

- Annotated ECG recordings with beat-level labels
- Widely used benchmark in academic ECG research
- Known limitations:
    - Small patient population
    - Controlled recording conditions
    - Label distributions that differ from modern monitoring environments

Key takeaway: Performance on MIT-BIH should be treated as an upper bound, not an estimate of real-world performance.

## Modeling Approach (High-Level)

- Beat segmentation using annotated R-peaks
- Supervised beat-level classification
- Standard preprocessing (normalization, filtering)
- Train/validation split at the patient level to avoid leakage

The modeling approach is intentionally kept simple and transparent, as the focus of this project is evaluation, risk, and system behavior, not architectural novelty.

## Evaluation Strategy: Beyond Accuracy

Traditional metrics (accuracy, macro-F1) are insufficient in clinical monitoring contexts. This project emphasizes:

- Class-conditional recall, especially for clinically significant arrhythmias
- Sensitivity at fixed false-positive rates
- Precision at constrained alert volumes
- Calibration analysis (reliability curves)

A model with excellent overall accuracy may still be unusable if it:

- Misses rare but dangerous arrhythmias
- Produces excessive false alarms
- Is poorly calibrated and overconfident

## Interpretation of Results

Class-conditional evaluation shows strong performance on the majority class and progressively lower recall for rarer beat types, despite consistently high precision. This indicates a conservative classifier that prioritizes avoiding false positives at the expense of missing minority classes.

One-vs-rest sensitivity at fixed false positive rates further highlights this tradeoff. Under strict false-alarm constraints (≤0.1% FPR), recall for rarer classes drops below 60–70%, while acceptable recall is only achieved by relaxing false-positive tolerance.
When evaluated as an alerting system, the model demonstrates near-perfect precision for the highest-confidence alerts, but captures only a small fraction of abnormal beats under tight alert budgets. Increasing alert volume improves recall but introduces diminishing returns relative to false-alarm cost.

Overall, these results suggest the model is well suited for high-precision triage or prioritization, but insufficient as a standalone diagnostic system. Safe deployment would require temporal aggregation, alert suppression logic, and human-in-the-loop review to balance sensitivity and alarm fatigue.

