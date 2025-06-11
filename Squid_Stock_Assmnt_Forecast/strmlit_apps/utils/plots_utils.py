import plotly.graph_objs as go
import streamlit as st

def plot_predictions(pred_dict, color_dict, title="Model Predictions vs Actual"):
    fig = go.Figure()

    for label, (x, y) in pred_dict.items():
        # Convert x and y to lists if they are not already
        if hasattr(x, 'tolist'):
            x = x.tolist()
        if hasattr(y, 'tolist'):
            y = y.tolist()

        color = color_dict.get(label, "#000000")  # default to black if not found
        mode = "lines" if "Predicted" in label else "markers"

        fig.add_trace(go.Scatter(
            x=x,
            y=y,
            mode=mode,
            name=label,
            line=dict(color=color) if mode == "lines" else None,
            marker=dict(color=color) if mode == "markers" else None
        ))

    fig.update_layout(
        title=title,
        xaxis_title="X",
        yaxis_title="CPUE",
        template="plotly_white"
    )

    st.plotly_chart(fig)
