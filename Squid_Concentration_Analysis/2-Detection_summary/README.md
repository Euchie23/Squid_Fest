## 📊 Detection Summary 📁🧪🦑 <br><br>

### Overview 🎯 <br>
This section focuses on summarizing the **detection behavior** of trace metals and organic compounds across squid tissues and sampling years. These analyses offer crucial insights into the **distribution and quality** of the concentration data before applying statistical models. The aim was to determine which elements or compounds were reliably detected, how many values were classified as outliers, and how often concentrations fell **below the limits of detection (BLOD)** or **quantification (BLOQ)**.

The folder includes Rscripts used for: 
- **Interactively preprocessed data**: Provides data is both numerical and categorical datasets to help with summary
- **Detection_Summary_Tables**: Summary statistics for each compound and tissue, including detection counts  
- **BLOD_BLOQ_Frequency**: Frequencies of samples falling below detection (BLOD) or quantification (BLOQ) limits  
- **Outlier_Analysis**: Identification of extreme values outside expected concentration ranges
    - [See Detection Summary Tables for Trace Metals](../2-Detection_summary/Detection_summary_plots_and_tables/Trace_metals/Detection_Summary_View.pdf)
    - [See Detection Summary Tables for Organic Compounds](../2-Detection_summary/Detection_summary_plots_and_tables/Organic_compounds/Detection_Summary_View.pdf) 
- **Visualization**: Barplots showing detection summary per compound, tissue, and sampling year
    - Trace Metals
      - [See Barplots for detection summary per metal](../2-Detection_summary/Detection_summary_plots_and_tables/Trace_metals/Concentration_Detection_Summary_using_pollutants.png) 
      - [See Barplots for detection summary per tissue](../2-Detection_summary/Detection_summary_plots_and_tables/Trace_metals/Concentration_Detection_Summary_using_tissues.png)
    - Organic Compounds
       - [See Barplots for detection summary per compound](../2-Detection_summary/Detection_summary_plots_and_tables/Organic_compounds/Concentration_Detection_Summary_using_pollutants.png)
       - [See Barplots for detection summary per tissue](../2-Detection_summary/Detection_summary_plots_and_tables/Organic_compounds/Concentration_Detection_Summary_using_pollutants.png)

<br>

### 🎓 Skills & Techniques Demonstrated <br>

**🧼 Flexible Data Preparation**  
Developed an interactive preprocessing script that allowed users to choose how to handle BLOD and BLOQ values before analysis.  
Options included keeping or removing LOQ values, or imputing them by multiplying LOQ values by a user-inputed muliplier (e.g 1/3 or 1/2), offering flexibility in sensitivity analysis. also a dataset with categorial values as well as numerical for a detection report.


**📈 Developing Quantitative Insight into Detection Behavior**  
Through systematic tabulation and visualization of detection statistics, I developed an understanding of how consistently each pollutant was measured across the dataset. This helped identify compounds or tissues with insufficient data coverage — an insight that directly influenced which variables could be used in downstream models. <br>

**🔍 Automated Detection Summary Generation in R**  
Using R's `dplyr` and `tidyr`, I wrote scripts that programmatically computed detection counts, BLOD/BLOQ flags, and outliers across tissues and years. These scripts reduced manual work and ensured the workflow could be repeated when new data was introduced. <br>

**📉 Understanding and Interpreting BLOD/BLOQ Patterns**  
One of the critical lessons involved correctly interpreting BLOD and BLOQ frequencies. I learned how values below these thresholds affect analytical integrity and how to classify such results for environmental toxicology studies. This was first explored while preprocessing LC-MS and ICP-MS outputs and reinforced during the creation of these summary tables. <br>

**📊 Visual Communication of Data Coverage**  
By plotting detection profiles as barplots, I made the coverage and gaps in the dataset more interpretable. These visualizations helped collaborators and reviewers quickly grasp where measurements were robust and where caution was warranted. <br>

**🧠 Recognizing Dataset Strengths and Limitations Early**  
This step acted as a diagnostic phase in the data pipeline. It helped prevent later surprises during modeling by revealing low-information variables, unbalanced distributions, and inconsistencies in tissue representation. <br>

**Note:** The detection summary serves as a **critical QA/QC checkpoint** between raw data transformation and statistical modeling. It allowed me to validate assumptions, filter unreliable data, and build confidence in subsequent analyses.
