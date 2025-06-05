import requests
import os

# Path to your text file with links
links_file = '../data/NASA_nc_links.txt'

# Folder to save the downloaded files
download_folder = "../data/nc_files"
os.makedirs(download_folder, exist_ok=True)

# Read URLs from the file
with open(links_file, "r") as f:
    urls = [line.strip() for line in f if line.strip()]

# Use .netrc authentication for NASA Earthdata
session = requests.Session()
session.auth = requests.auth.HTTPBasicAuth('', '')  # .netrc handles credentials

for url in urls:
    filename = url.split("/")[-1]
    filepath = os.path.join(download_folder, filename)

    print(f"Downloading {filename}...")

    response = session.get(url)

    if "html" in response.headers.get("Content-Type", ""):
        print(f"⚠️ Got HTML instead of .nc file for {filename}. Check login or link.")
        continue

    with open(filepath, "wb") as f:
        f.write(response.content)
        print(f"✅ Saved: {filename}")
