# 🦑 Stock Assessment via Predictive Modeling (Squid CPUE)🦑🌊📊📈


This project uses machine learning to model and predict CPUE (Catch Per Unit Effort) of squid across various environmental and spatial conditions.

## Goals
- Predict CPUE from water temperature, depth, spatial coordinates, and time
- Evaluate model performance over multiple years
- Save predictions for future comparison with observed catch

## Workflow
1. Load and clean historical squid data
2. Train models (e.g., linear regression, XGBoost)
3. Validate predictions (focus: 2016–2020)
4. Visualize trends (CPUE vs year, location)

## Tools
- Python (pandas, sklearn, xgboost, seaborn)
- Jupyter Notebooks or VSCode
- Data exported from QGIS or remote sensing tools (optional)