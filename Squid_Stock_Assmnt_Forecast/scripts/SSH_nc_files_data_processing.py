import os
import netCDF4 as nc
import numpy as np
import csv
import re

# Path to the folder with NetCDF files
folder_path = '../data/SSH_nc_files'

# Output CSV file
output_csv = '../data/SSH_summary.csv'

# Regex to extract Year and Month from filename
pattern = re.compile(r'(\d{4})[-_]?m?(\d{2})')

# Known variable name options
ssh_vars = ['SSH', 'adt', 'sla', 'sea_surface_height_anomaly']

# Collect data rows
data_rows = []

for filename in os.listdir(folder_path):
    if filename.endswith(".nc"):
        match = pattern.search(filename)
        if match:
            year, month = match.groups()
            filepath = os.path.join(folder_path, filename)

            try:
                ds = nc.Dataset(filepath)
                ssh_data = None

                for var in ssh_vars:
                    if var in ds.variables:
                        ssh_data = ds.variables[var][:]
                        break

                if ssh_data is None:
                    raise ValueError("No recognized SSH variable found in file.")

                ssh_mean = np.mean(ssh_data[:])
                data_rows.append([year, month, round(float(ssh_mean), 4)])
                ds.close()
            except Exception as e:
                print(f"❌ Error reading {filename}: {e}")

# Write to CSV
with open(output_csv, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['Year', 'Month', 'SSH'])
    writer.writerows(sorted(data_rows))

print(f"✅ SSH data saved to {output_csv}")

