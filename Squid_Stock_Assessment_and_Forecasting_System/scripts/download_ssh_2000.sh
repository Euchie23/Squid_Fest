#!/bin/bash

GREP_OPTIONS=''

cookiejar=$(mktemp cookies.XXXXXXXXXX)
netrc=$(mktemp netrc.XXXXXXXXXX)
chmod 0600 "$cookiejar" "$netrc"
function finish {
  rm -rf "$cookiejar" "$netrc"
}

trap finish EXIT

prompt_credentials() {
    echo "Enter your Earthdata Login or other provider supplied credentials"
    read -p "Username (euchie23): " username
    username=${username:-euchie23}
    read -s -p "Password: " password
    echo "machine urs.earthdata.nasa.gov login $username password $password" >> $netrc
    echo
}

exit_with_error() {
    echo
    echo "Unable to Retrieve Data"
    echo
    echo $1
    echo
    exit 1
}

detect_app_approval() {
    approved=`curl -s -b "$cookiejar" -c "$cookiejar" -L --max-redirs 5 --netrc-file "$netrc" https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2017-12_ECCO_V4r4_latlon_0p50deg.nc -w '\n%{http_code}' | tail  -1`
    if [ "$approved" -ne "200" ] && [ "$approved" -ne "301" ] && [ "$approved" -ne "302" ]; then
        exit_with_error "Please ensure that you have authorized the remote application by visiting the link below "
    fi
}

setup_auth_curl() {
    status=$(curl -s -z "$(date)" -w '\n%{http_code}' https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2017-12_ECCO_V4r4_latlon_0p50deg.nc | tail -1)
    if [[ "$status" -ne "200" && "$status" -ne "304" ]]; then
        detect_app_approval
    fi
}

setup_auth_wget() {
    touch ~/.netrc
    chmod 0600 ~/.netrc
    credentials=$(grep 'machine urs.earthdata.nasa.gov' ~/.netrc)
    if [ -z "$credentials" ]; then
        cat "$netrc" >> ~/.netrc
    fi
}

fetch_urls() {
  if command -v curl >/dev/null 2>&1; then
      setup_auth_curl
      while read -r line; do
        filename="${line##*/}"
        stripped_query_params="${filename%%\?*}"
        target="../data/SSH_nc_files/$stripped_query_params"

        # Check if file exists and is bigger than 1KB (adjust threshold if needed)
        if [ -f "$target" ] && [ $(stat -f%z "$target") -gt 1024 ]; then
            echo "File $target exists and seems valid, skipping download."
        else
            echo "Downloading $target ..."
            curl -f -b "$cookiejar" -c "$cookiejar" -L --netrc-file "$netrc" -g -o "$target" -- "$line" || exit_with_error "Failed to download $line"
        fi
      done;
  elif command -v wget >/dev/null 2>&1; then
      echo
      echo "WARNING: Can't find curl, using wget instead."
      echo "WARNING: Script may not correctly identify Earthdata Login integrations."
      echo
      setup_auth_wget
      while read -r line; do
        filename="${line##*/}"
        stripped_query_params="${filename%%\?*}"
        target="../data/SSH_nc_files/$stripped_query_params"

        if [ -f "$target" ] && [ $(stat -f%z "$target") -gt 1024 ]; then
            echo "File $target exists and seems valid, skipping download."
        else
            echo "Downloading $target ..."
            wget --load-cookies "$cookiejar" --save-cookies "$cookiejar" --output-document "$target" --keep-session-cookies -- "$line" || exit_with_error "Failed to download $line"
        fi
      done;
  else
      exit_with_error "Error: Could not find a command-line downloader.  Please install curl or wget"
  fi
}

prompt_credentials


fetch_urls <<'EOF'
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-01_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-02_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-03_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-04_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-05_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-06_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-07_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-08_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-09_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-10_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-11_ECCO_V4r4_latlon_0p50deg.nc"
"https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/ECCO_L4_SSH_05DEG_MONTHLY_V4R4/SEA_SURFACE_HEIGHT_mon_mean_2000-12_ECCO_V4r4_latlon_0p50deg.nc"

EOF
