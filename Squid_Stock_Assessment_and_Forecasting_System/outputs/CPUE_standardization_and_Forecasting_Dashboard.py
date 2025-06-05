# cpue_dashboard.py

import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt

# Load your actual and predicted values (replace with your actual data)
y_test = pd.Series([...])  # actual
linear_preds = [...]
gamma_preds = [...]
tweedie_preds = [...]

# Dropdown for model selection
model = st.selectbox("Choose a model", ["LinearGAM", "GammaGAM", "TweedieGLM"])

# Get predictions based on choice
if model == "LinearGAM":
    preds = linear_preds
elif model == "GammaGAM":
    preds = gamma_preds
else:
    preds = tweedie_preds

# Plotting
st.title("Actual vs Predicted CPUE")
fig, ax = plt.subplots()
ax.plot(y_test.values, label="Actual", marker='o')
ax.plot(preds, label=f"Predicted ({model})", marker='x')
ax.set_xlabel("Test Sample")
ax.set_ylabel("CPUE")
ax.legend()
st.pyplot(fig)