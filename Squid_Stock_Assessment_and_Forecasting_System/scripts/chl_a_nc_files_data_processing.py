import xarray as xr
import pandas as pd
import os
import numpy as np

# Folder with your downloaded .nc files
data_folder = "../data/chlor_a_nc_files"

# Coordinates setup
primary_coords = {
    "lat_name": "lat",
    "lon_name": "lon",
    "lat_slice": None,  # will set later based on latitude ordering
    "lon_slice": slice(-60.2175, -58.1383),
}

backup_coords = {
    "lat_name": "lat",
    "lon_name": "lon",
    "lat_slice": slice(-49.3014, -47.0933),
    "lon_slice": slice(-58.2483, -57.6389),
}

def get_mean_chla(ds, lat_slice, lon_slice, lat_name="lat", lon_name="lon"):
    """
    Extract mean chlorophyll-a value for given region.
    Returns None if data is missing or invalid.
    """
    region = ds.sel({lat_name: lat_slice, lon_name: lon_slice})
    chl = region["chlor_a"].where(region["chlor_a"] > 0)  # Remove invalid/fill values
    mean_val = chl.mean().values
    if np.isnan(mean_val):
        return None
    return float(mean_val)

results = []

# Load all data first for temporal interpolation step
all_data = []

for file in sorted(os.listdir(data_folder)):
    if file.endswith(".nc"):
        file_path = os.path.join(data_folder, file)
        ds = xr.open_dataset(file_path, engine="netcdf4")

        lat_vals = ds[primary_coords["lat_name"]].values
        # Set lat slice based on lat order (ascending/descending)
        if lat_vals[0] > lat_vals[-1]:  # descending
            primary_coords["lat_slice"] = slice(-49.5453, -50.5153)
        else:
            primary_coords["lat_slice"] = slice(-50.5153, -49.5453)

        parts = file.split(".")[1]
        year = int(parts[:4])
        month = int(parts[4:6])

        # Try primary region first
        mean_chl = get_mean_chla(ds, primary_coords["lat_slice"], primary_coords["lon_slice"],
                                 primary_coords["lat_name"], primary_coords["lon_name"])

        # If no data found, try backup region
        if mean_chl is None:
            mean_chl = get_mean_chla(ds, backup_coords["lat_slice"], backup_coords["lon_slice"],
                                     backup_coords["lat_name"], backup_coords["lon_name"])

        all_data.append({"year": year, "month": month, "chlor_a": mean_chl})

# Convert to DataFrame for temporal interpolation
df = pd.DataFrame(all_data).sort_values(["year", "month"]).reset_index(drop=True)

# Temporal interpolation for missing chlor_a values
for i, row in df.iterrows():
    if pd.isna(row["chlor_a"]):
        prev_idx = i - 1
        next_idx = i + 1

        # Find previous valid chlor_a
        while prev_idx >= 0 and pd.isna(df.loc[prev_idx, "chlor_a"]):
            prev_idx -= 1

        # Find next valid chlor_a
        while next_idx < len(df) and pd.isna(df.loc[next_idx, "chlor_a"]):
            next_idx += 1

        if prev_idx >= 0 and next_idx < len(df):
            # Linear interpolation: average of neighbors
            prev_val = df.loc[prev_idx, "chlor_a"]
            next_val = df.loc[next_idx, "chlor_a"]
            df.at[i, "chlor_a"] = (prev_val + next_val) / 2
        elif prev_idx >= 0:
            df.at[i, "chlor_a"] = df.loc[prev_idx, "chlor_a"]
        elif next_idx < len(df):
            df.at[i, "chlor_a"] = df.loc[next_idx, "chlor_a"]
        else:
            df.at[i, "chlor_a"] = np.nan  # no data to interpolate

# Save results to CSV
df.to_csv("../data/chlorophyll_swatlantic_2000_2022_interpolated.csv", index=False)

print("CSV created with interpolation: chlorophyll_swatlantic_2000_2022_interpolated.csv")
