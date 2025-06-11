import pandas as pd
import numpy as np
import streamlit as st

def load_model_data():
    try:
        model_summary = pd.read_csv("../results/summaries/model_summary.csv")
        cv_results = pd.read_csv("../results/summaries/cv_results.csv")
        test_results = pd.read_csv("../results/summaries/eval_results.csv")
    except FileNotFoundError:
        model_summary = pd.DataFrame()
        cv_results = pd.DataFrame()
        test_results = pd.DataFrame()
    return model_summary, cv_results, test_results

def get_model_colors():
    return {
        "Actual": "#1f77b4",
        "Predicted (LinearGAM_log_CPUE_plus_c)": "#ff7f0e",
        "Predicted (LinearGAM_log_CPUE_plus_1)": "#2ca02c",
        "Predicted (GammaGAM_CPUE)": "#d62728",
        "Predicted (TweedieGLM_CPUE)": "#9467bd",
    }

def load_monthly_cpue():
    try:
        return pd.read_csv("../results/summaries/monthly_cpue.csv")
    except FileNotFoundError:
        return pd.DataFrame(columns=["Month", "Year", "Monthly_CPUE"])

def load_observed_vs_standardized():
    try:
        merged = pd.read_csv("../results/summaries/observed_vs_standardized.csv")
        colors = {
            "Observed": "#1f77b4",
            "Standardized_log_cpueC": "#ff7f0e",
	    "Standardized_log_cpue1": "#2ca02c",
	    "Standardized_CPUE_GammaGAM":"#d62728",
	    "Standardized_CPUE_tweedieglm":"#9467bd"
        }
        return merged, colors
    except FileNotFoundError:
        return pd.DataFrame(), {}

def load_residual_data():
    try:
        data = np.load("../results/summaries/residuals_data_clean.npz", allow_pickle=True)
        return data["residual_dict"].item()
    except Exception as e:
        print("Error loading cleaned residuals_data:", e)
        return {}

def load_prediction_data():
    try:
        data = np.load("../results/summaries/prediction_data_clean.npz", allow_pickle=True)
        return data["pred_dict"].item(), data["color_dict"].item()
    except Exception as e:
        print("Error loading cleaned prediction_data:", e)
        return {}, {}
