import pandas as pd
import numpy as np

# Load the merged squid+chlorophyll dataset
merged_df = pd.read_csv("../data/merged_squid_cpue_with_chlorophyll.csv")

# Load SSH summary and merge
ssh_df = pd.read_csv("../data/SSH_summary.csv")
merged_df = pd.merge(merged_df, ssh_df, on=["Year", "Month"], how="left")

# Move SSH column to the right of WaterTemp
if "WaterTemp" in merged_df.columns:
    cols = list(merged_df.columns)
    ssh_idx = cols.index("SSH")
    temp_idx = cols.index("WaterTemp") + 1
    cols.insert(temp_idx, cols.pop(ssh_idx))
    merged_df = merged_df[cols]
else:
    print("⚠ 'WaterTemp' column not found. SSH will be added at the end.")

# Custom WaterTemp imputation by Month using random values between prev/next month means
for month in range(1, 13):
    month_mask = (merged_df["Month"] == month)
    missing_mask = month_mask & (merged_df["WaterTemp"].isna())

    if missing_mask.any():
        # Get previous and next months (with wrapping)
        prev_month = 12 if month == 1 else month - 1
        next_month = 1 if month == 12 else month + 1

        prev_mean = merged_df.loc[merged_df["Month"] == prev_month, "WaterTemp"].mean()
        next_mean = merged_df.loc[merged_df["Month"] == next_month, "WaterTemp"].mean()

        # Fallback if one of the months has no data
        if pd.isna(prev_mean): prev_mean = next_mean
        if pd.isna(next_mean): next_mean = prev_mean

        # Generate random values between prev_mean and next_mean
        num_missing = missing_mask.sum()
        random_vals = np.random.uniform(low=min(prev_mean, next_mean),
                                        high=max(prev_mean, next_mean),
                                        size=num_missing)

        # Fill the missing WaterTemp values
        merged_df.loc[missing_mask, "WaterTemp"] = random_vals

# Save final dataset
output_path = "../data/Final_dataset.csv"
merged_df.to_csv(output_path, index=False)

print(f"✅ Final dataset saved with SSH added and WaterTemp imputed by month logic: {output_path}")


