import pandas as pd

# Load squid CPUE data
squid_df = pd.read_csv("../data/squid_cpue.csv")

# Load interpolated (or raw) chlorophyll data
chl_df = pd.read_csv("../data/chlorophyll_swatlantic_2000_2022_interpolated.csv")

# Make sure column names match
squid_df = squid_df.rename(columns={"Year1": "Year"})
chl_df = chl_df.rename(columns={"year": "Year","month": "Month" })


# Merge on Year and Month — keep all squid data, fill missing chl_a with NaN
merged = pd.merge(squid_df, chl_df, on=["Year", "Month"], how="left")

# Reorder: insert chlor_a between Depth and SqCatch_Kg
cols = list(merged.columns)
chl_index = cols.index("Depth") + 1
cols.insert(chl_index, cols.pop(cols.index("chlor_a")))
merged = merged[cols]

# add unit of measurement to chlor_A
merged = merged.rename(columns={"chlor_a": "Chlor_a_mg_m3"})

# Save to new CSV
merged.to_csv("../data/merged_squid_cpue_with_chlorophyll.csv", index=False)

print("✅ Merged file created: squid_cpue_with_chlorophyll.csv (missing chlor_a = NaN)")