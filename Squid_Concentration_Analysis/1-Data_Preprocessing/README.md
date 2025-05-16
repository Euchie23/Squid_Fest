🧪 Data Preprocessing 🧼📊🦑


Overview 🎯
The goal of this stage was to transform raw instrumental outputs into clean, biologically contextualized datasets. These processed files were then used for downstream statistical modelling, ecological interpretation, and temporal analysis.

This folder contains all the raw and processed data used for the Squid_Concentration_Analysis project's chemical analysis pipeline, including:
&nbsp;&nbsp;&nbsp;&nbsp;* Raw output files from ICP-MS (Inductively Coupled Plasma Mass Spectrometry) for trace metals <br>
&nbsp;&nbsp;&nbsp;&nbsp;* Raw output files from LC-MS (Liquid Chromatography–Mass Spectrometry) <br>
&nbsp;&nbsp;&nbsp;&nbsp;* R scripts used to clean, process, and structure the data <br>
&nbsp;&nbsp;&nbsp;&nbsp;* Cleaned and merged raw datasets with biological and environmental data <br>
&nbsp;&nbsp;&nbsp;&nbsp;* Calculated standard concentrations (LOD and LOQ) for sample concentration calculation <br>	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[View Data Processing Workflow PDF](../Appendix/Data_processing_workflow.pdf)

📂 Folder Contents
	--Raw_data : Contains raw CSV outputs from ICP-MS and LC-MS instruments
	--Output data: Contains cleaned, unit-converted datasets, with flagged LOD/LOQ values
		--Final_Results_For_Analysis
		--Datasets/Cleaned_Data
	--R scripts: For data processing
	--Metadata: sample biological and environmental variables merged to processed data
		--Squid_Catch_Data

🎓 Skills & Techniques Demonstrated
🔬 Translating Lab Outputs into Meaningful Data
Converted raw LC-MS and ICP-MS readings into dry-weight concentrations by applying calibration logic and unit conversions based on scientific standards.

🧹 Data Cleaning and Preprocessing in R
Used dplyr, stringr, and tidyr in R to automate preprocessing steps — from flagging LOD/LOQ values to exporting tidy datasets in a reproducible pipeline.

🧠 Handling Imperfect, Real-World Data
Standardized inconsistently labeled datasets and managed missing or blank entries, preparing data for clean statistical analysis across multiple years.

🧬 Joining Scientific, Biological & Environmental Data
Merged processed trace metal concentrations with squid-specific biological data and environmental covariates, aligning lab results with ecological context.

🧪 Validating Measurements Against Detection Limits
Programmatically calculated and applied LOD/LOQ thresholds to classify data reliability, ensuring measurement integrity across all elements and samples.

🔄 Creating Reproducible, Documented Workflows
Structured the entire preprocessing phase as R scripts with clear logic, outputs, and checkpoints — allowing anyone to rerun or audit the full pipeline.
