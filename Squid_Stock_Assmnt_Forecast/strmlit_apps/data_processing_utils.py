import streamlit as st
import pandas as pd
from PIL import Image
import plotly.express as px
import plotly.graph_objects as go
from utils.data_utils import (
    load_model_data,
    get_model_colors,
    load_prediction_data,
    load_residual_data,
    load_monthly_cpue,
    load_observed_vs_standardized
)
from utils.plots_utils import plot_predictions  # wherever you put it

# ---------------------- Page Configuration ----------------------
st.set_page_config(page_title="CPUE Model Evaluation Dashboard", layout="wide")


# Load prediction data
pred_dict, color_dict = load_prediction_data()

# Plot the predictions
plot_predictions(pred_dict, color_dict)


# ---------------------- Custom CSS ----------------------
st.markdown("""
    <style>
    /* --- Sidebar background with coral image + navy transparent overlay --- */
    [data-testid="stSidebar"] > div:first-child {
        position: relative;
        background-image: url("https://thumbs.dreamstime.com/b/underwater-seascape-ocean-coral-reef-deep-sea-bottom-swimming-under-water-marine-corals-background-vector-seaweed-algae-354608779.jpg");
        background-repeat: no-repeat;
        background-size: cover;
        background-position: center;
        min-height: 100vh;
        color: #E1EAF2;
    }
    [data-testid="stSidebar"] > div:first-child::before {
        content: "";
        position: absolute;
        top: 0; left: 0; right: 0; bottom: 0;
        background-color: rgba(0, 31, 63, 0.6); /* navy transparent */
        z-index: 0;
    }
    [data-testid="stSidebar"] > div:first-child > * {
        position: relative;
        z-index: 1;
    }

    /* --- Main app background with deep sea image + navy transparent overlay --- */
    .stApp {
        position: relative;
        background-image: url("https://images.unsplash.com/photo-1530951980629-fbeef86f69a1?q=80&w=2768&auto=format&fit=crop&ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D");
        background-repeat: no-repeat;
        background-size: cover;
        background-position: center;
        min-height: 100vh;
        color: #E1EAF2;
    }
    .stApp::before {
        content: "";
        position: absolute;
        top: 0; left: 0; right: 0; bottom: 0;
        background-color: rgba(10, 47, 68, 0.7); /* navy transparent */
        z-index: 0;
    }
    .stApp > * {
        position: relative;
        z-index: 1;
    }

    /* --- Top bar background color navy --- */
    header, .css-nahz7x {
        background-color: #001f3f !important;
    }
    </style>
""", unsafe_allow_html=True)

# ---------------------- Sidebar Navigation ----------------------
st.sidebar.title("🎣 CPUE Dashboard Navigation")
page = st.sidebar.radio("Sections", [
    "Overview",
    "Model Comparison",
    "Evaluation Metrics",
    "Residual Analysis",
    "Predictions"
])

# ---------------------- Data Loaders ----------------------
model_summary, cv_results, test_results = load_model_data()

# ---------------------- Page Content ----------------------
if page == "Overview":
    st.title("🎣 CPUE Standardization – Model Summary")
    st.markdown("""
    **Objective:**
    Standardize Monthly CPUE using environmental and spatiotemporal covariates to remove external influences, enabling fair comparison across years for trend analysis and stock assessment.

    **Approaches Explored:**
    - Log-transformed Generalized Additive Models (GAMs)
    - Raw CPUE using Gamma and Tweedie distributions
    - Cross-validation and residual diagnostics
    """)

elif page == "Model Comparison":
    st.header("🔍 Model Comparison Table")
    if not model_summary.empty:
        st.dataframe(model_summary)
    else:
        st.warning("Model summary data not found.")

elif page == "Evaluation Metrics":
    col1, col2 = st.columns(2)
    with col1:
        st.subheader("⚙️ Cross-Validation Results")
        if not cv_results.empty:
            st.dataframe(cv_results)
        else:
            st.warning("Cross-validation data not found.")
    with col2:
        st.subheader("🧾 Test Set Evaluation")
        if not test_results.empty:
            st.dataframe(test_results)
        else:
            st.warning("Test results data not found.")

elif page == "Residual Analysis":
    st.header("📉 Residual Plots")
    residual_data = load_residual_data()

    for model, (preds, residuals) in residual_data.items():
        with st.expander(f"Residuals for {model}"):
            fig = go.Figure()
            fig.add_trace(go.Scatter(x=preds, y=residuals,
                                     mode='markers',
                                     marker=dict(color='steelblue'),
                                     name='Residuals'))
            fig.add_hline(y=0, line=dict(color='red', dash='dash'))
            fig.update_layout(title=f"Residuals: {model}",
                              xaxis_title="Predicted",
                              yaxis_title="Residuals",
                              template="plotly_white")
            st.plotly_chart(fig, use_container_width=True)

elif page == "Predictions":
    st.header("📊 Predictions & Standardization")

    st.subheader("Interactive Monthly CPUE")
    monthly_cpue = load_monthly_cpue()
    fig_monthly = px.line(
        monthly_cpue,
        x="Month",
        y="Monthly_CPUE",
        color="Year",
        markers=True,
        title="Monthly Standardized CPUE (kg/day) by Year",
        labels={"Monthly_CPUE": "CPUE (kg/day)", "Month": "Month"}
    )
    fig_monthly.update_layout(
        legend_title="Year",
        xaxis=dict(tickmode='linear', tick0=1, dtick=1),
        hovermode='x unified',
        width=900,
        height=600
    )
    st.plotly_chart(fig_monthly, use_container_width=True)

    st.subheader("Observed vs Standardized CPUE")
    merged, colors = load_observed_vs_standardized()
    fig_obs_std = go.Figure()
    for name in colors:
        fig_obs_std.add_trace(
            go.Scatter(x=merged["Year"], y=merged[name],
                       mode="lines+markers", name=name,
                       line=dict(color=colors[name])))
    fig_obs_std.update_layout(
        title="Observed vs Standardized CPUE per Year",
        xaxis_title="Year",
        yaxis_title="CPUE",
        hovermode="x unified",
        width=1000,
        height=600
    )
    st.plotly_chart(fig_obs_std, use_container_width=True)

    st.subheader("Actual vs Predicted CPUE")
    pred_data, pred_colors = load_prediction_data()
    fig_preds = go.Figure()
    for label, (x, y, style, color) in pred_data.items():
        fig_preds.add_trace(go.Scatter(x=x, y=y, mode='lines+markers', name=label,
                                       marker=dict(symbol=style),
                                       line=dict(color=pred_colors[label])))
    fig_preds.update_layout(
        title="Actual vs Predicted CPUE (Interactive)",
        xaxis_title="Test Sample Index",
        yaxis_title="CPUE",
        hovermode="x unified",
        legend_title="Legend",
        template="plotly_white"
    )
    st.plotly_chart(fig_preds, use_container_width=True)
