## 🧠 Data Mining 🧬📊🧪

### Overview 🎯  
This stage involved a range of exploratory and statistical techniques to extract meaningful patterns, generate hypotheses, and uncover ecological or biological insights from pollutant concentration data. Analyses were performed across various biological and environmental variables, focusing on identifying how factors like gender, size, year, and tissue type influence pollutant distributions in squid.

  
This folder contains datasets, R scripts for: 
- **Interactively preprocessing data for analysis**
- **Size-Based Correlation Analysis**  
- **Gender Temporal Comparison**  
- **Tissue-Wise Temporal Regression Modeling**  
- **Multivariate & Machine Learning Approaches**  
- **Visualization Outputs**
    * Icons to represent the source of each pollutant that are incorprated into the        graphs

### 🎓 Skills & Techniques Demonstrated  
- **🔍 Biological Pattern Detection**  
  Learned how to use correlation analysis to test hypotheses about pollutant accumulation across biological size classes and how imputation choices affect results.  
  * Spearman correlation tests were run to examine whether squid size (categorized       by mantle length) influenced pollutant levels, analyzing both filtered and           imputed datasets to assess sensitivity to BLOD/BLOQ values.

- **📊 Comparative Analysis**  
  Applied statistical tests to evaluate pollutant differences by gender and uncover biologically relevant temporal trends.  
  * Tests included Kruskal-Wallis, t-tests, ANOVA, Shapiro-Wilks,and Mann-Whitney        methods across and within years.

- **📅 Temporal Modeling**  
  Used linear regression models to assess how variables such as gender, month, latitude, distance to land, and maturity influenced pollutant levels across years and tissues.  
  * Plots were generated to visualize trends and changing relationships over time.

- **🧮 Multivariate Analysis & Classification**  
  Explored patterns and interactions in the data using PCA, RDA, correlation plots, PLS, k-means, and decision trees.  
   * PCA visualized metals and organic datasets.  
   * RDA examined pollutant interactions per tissue.  
   * Correlation plots and PLS investigated compound synergy.  
   * K-means and decision trees classified tissue types by compound profiles.

- **📈 Communicating Results Visually**  
  Strengthened data storytelling by producing reproducible, publication-ready plots that clearly summarized key insights across analyses.
