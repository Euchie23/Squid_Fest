## 🧪 Temporal Variations in Chemical Concentrations 📈🧬📊

### Overview 🎯  
This stage focused on investigating how chemical pollutant concentrations in squid tissues vary over time. It addresses one of the core hypotheses of the thesis: **do temporal patterns reveal significant shifts in exposure or accumulation?** The analysis integrates statistical testing, biological relevance, and environmental context to assess annual concentration differences across multiple tissues and elements.

This folder contains datasets, plotting icons, and R scripts used to:  
- **Run temporal statistical comparisons across years**  
- **Visualize concentration trends per tissue and compound**  
- **Assess exceedance against recommended safety thresholds**  
- **Interactively preprocess data with user-defined filtering and imputation settings**

---

### 🎓 Skills & Techniques Demonstrated  

- **🧼 Flexible Data Preparation**  
  Developed an interactive preprocessing script that allowed users to choose how to handle BLOD and BLOQ values before analysis.  
  * Options included keeping or removing LOQ values, or imputing them as ⅓ LOQ, offering flexibility in sensitivity analysis.

- **🧪 Statistical Testing for Temporal Trends**  
  Applied appropriate statistical tests to evaluate year-on-year pollutant variations in each tissue.  
  * Used Shapiro-Wilks to test normality and chose between ANOVA or Kruskal-Wallis based on distribution, followed by Tukey or Dunn post hoc comparisons.

- **📊 Visualizing Annual Concentration Changes**  
  Designed multi-bar plots to visualize pollutant trends across years for each tissue.  
  * Bar plots grouped tissues on the x-axis, with bars representing different years and y-axis showing mean concentration levels.

- **⚠️ Risk Benchmarking with Toxicological Thresholds**  
  Incorporated toxicological reference data into visualizations to flag exceedances.  
  * Used a curated dataframe of recommended maximum concentrations and oral reference doses (RfDs) from published studies to overlay risk thresholds on plots.

- **📈 Enhancing Interpretability Through Icons and Annotation**  
  Integrated pollutant source icons into graphs to improve visual storytelling and reinforce the link between contaminants and ecological/industrial origins.

