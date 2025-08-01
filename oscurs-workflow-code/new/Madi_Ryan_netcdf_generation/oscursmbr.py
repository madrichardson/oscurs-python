"""
OSCURS Python Reimplementation
==============================
Last Updated: 7/24/25

This script generates gridded, interpolated surface pressure fields for the
OSCURS (Ocean Surface CURrent Simulator) model, replicating the original
Fortran-based workflow.

It pulls 6-hourly pressure data from the FNMOC dataset via NOAA's ERDDAP
server, interpolates it onto a custom lat/lon grid, extracts 00Z slices,
and fills any missing data via temporal interpolation. The final output
is written to a compressed NetCDF4 file for model input or analysis.

----------
Inputs
----------
- YYYYMM : A date string (e.g., '201603') passed as a command-line argument.
- `lola92180.txt` : A text file defining the 92 x 180 target lat/lon grid
  (matching OSCURS formatting).

----------
Outputs
----------
- `erdlasFnPres6_YYYYMM.nc` : A temporary NetCDF file downloaded from ERDDAP
  for the given month.
- `oscurstest_YYYYMM.nc` : Final processed NetCDF4 file containing the
  interpolated 00Z pressure fields on the OSCURS grid.

----------
Workflow
----------
1. **Read Grid**:
   - Load a static lat/lon grid from `lola92180.txt`.

2. **Download Data**:
   - Open FNMOC 6-hour pressure field dataset from ERDDAP using `xarray`
     + OPeNDAP.
   - Subset based on the specified month.

3. **Convert Time Axis**:
   - Convert ERDDAP's "seconds since 1970" to Fortran-compatible
     "hours since 1948".

4. **Spatial Interpolation**:
   - Use bilinear interpolation to map pressure data to the target OSCURS grid.

5. **Temporal Subsetting**:
   - Extract 00Z (midnight UTC) slices from the interpolated grid.

6. **Fill Missing Time Steps**:
   - Use linear interpolation across time if any 00Z slices are missing.

7. **Save Output**:
   - Write interpolated fields and metadata to a compressed CF-1.6-compliant
     NetCDF file.

----------
Dependencies
----------
- Python 3.8+
- NumPy, Pandas, Xarray, NetCDF4
- Custom helper functions in `oscursUtilmbr.py`

----------
Assumptions
----------
- The pressure data is on a 1°x1° grid (latitude: -89.5 to 89.5, longitude:
  0.5 to 359.5).
- 6-hourly time steps are ordered: 00Z, 06Z, 12Z, 18Z.
- Interpolation is only performed within the data bounds (no extrapolation).
- Missing 00Z values are filled via weighted average of surrounding valid time
  steps.
"""

# Import libraries
import os
import numpy as np
import pandas as pd
import urllib.request
import xarray as xr
import netCDF4
from datetime import datetime, timedelta, timezone
from netCDF4 import num2date, date2num, Dataset
import math
from oscursUtilmbr import *  # Import helper functions
import csv
import calendar
import sys

# Read the input date (format: YYYYMM) from command line
input_date = sys.argv[1]

# Build Lat/Lon Grid
# Read fixed lat/lon reference points from a text file
results = []
with open("lola92180.txt") as inputfile:
    for row in csv.reader(inputfile):
        results.append(row)

# Initialize target lat/lon grids (92 rows, 180 columns)
lat_grid = np.zeros((92, 180), dtype="float32")
lon_grid = np.zeros((92, 180), dtype="float32")

# Reorganize 1D rows from the file into 2D grid arrays
count = 0
countj = 0
latjunk = []
lonjunk = []
for line in results:
    junk = str.split(line[0])
    lonjunk.append((float(junk[0]) - 360) * -1)  # Convert from 0-360 to -180-180
    latjunk.append(float(junk[1]))
    if count == 91:
        lat_grid[:, countj] = np.array(latjunk)
        lon_grid[:, countj] = np.array(lonjunk)
        count = -1
        countj = countj + 1
        latjunk = []
        lonjunk = []
    count = count + 1

# Set Time Range and Retrieve Pressure Data
# Extract year/month and compute start/end of the month
start_year = input_date[0:4]
start_month = input_date[4:6]
start_day = "01"
mon_range = calendar.monthrange(int(start_year), int(start_month))
end_day = str(mon_range[1]).rjust(2, "0")

# Build start and end date strings in ISO 8601 format
start_string = f"{start_year}-{start_month}-{start_day}T00:00:00Z"
end_string = f"{start_year}-{start_month}-{end_day}T18:00:00Z"

# Open dataset via ERDDAP OPeNDAP using xarray
print("Opening dataset via OPeNDAP...")
ds = xr.open_dataset(
    'https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnPres6',
    decode_times=True
)
print("Time dtype:", ds.time.dtype)
print("First time value:", ds.time.values[0])

# Subset data for selected time range and full lat/lon range
print(f"Subsetting data from {start_string} to {end_string}...")
subset = ds.sel(
    time=slice(
        np.datetime64(start_string.replace("Z", "")),
        np.datetime64(end_string.replace("Z", ""))
    ),
    latitude=slice(-89.5, 89.5),
    longitude=slice(0.5, 359.5)
)

# Save subset locally for downstream processing
subset_file = f'erdlasFnPres6_{input_date}.nc'
print(f"Saving to {subset_file}...")
subset.to_netcdf(subset_file)


# Load Pressure and Time Arrays from NetCDF
# Open the saved NetCDF file
ncfile = netCDF4.Dataset(subset_file)
ncv = ncfile.variables
pmsl = ncv['p_msl'][:, :, :]  # Shape: [time, lat, lon]

# Extract and convert time values to match Fortran: "hours since 1948-01-01"
time = ncv["time"][:]  # seconds since 1970-01-01T00:00:00Z
time_conv = np.zeros(len(pmsl) // 4, dtype="float32")
count = 0
for i in range(len(pmsl) // 4):
    a = num2date(time[count], "seconds since 1970-01-01T00:00:00Z")
    b = date2num(a, "hours since 1948-01-01 00:00:00")
    time_conv[i] = b
    count = count + 4

# Interpolate Spatial Grid
# Allocate arrays for full and 00Z-only pressure fields
ntime = len(pmsl)
pressure_full = np.full((ntime, 92, 180), np.nan, dtype=np.float32)
pressure_converted = np.full((ntime // 4, 92, 180), np.nan, dtype=np.float32)

# Interpolate each 6-hour slice onto the custom grid
for i in range(ntime):
    interp = interpolate_grid(pmsl[i, :, :], lat_grid, lon_grid)
    pressure_full[i] = interp
    if i % 4 == 0:
        pressure_converted[i // 4] = interp

# Identify and Fill Missing Time Steps
# Identify missing values in pressure_converted (00Z time slices)
mylist_nan = list(set(np.where(np.isnan(pressure_converted))[0]))
mylist_full = list(set(np.where(np.isnan(pressure_full) == False)[0]))

print(f"Detected NaNs to interpolate: {len(mylist_nan)}")
print("Indices:", mylist_nan[:10])  # Show first 10, if any


# Convert times to "hours since 1948-01-01 00:00:00" for weighted 
# time interpolation
time_hours = np.array([
    date2num(num2date(t, "seconds since 1970-01-01T00:00:00Z"), "hours since 1948-01-01 00:00:00")
    for t in time
])

# Fill missing 00Z time slices using weighted interpolation between
# closet valid frames
for i in mylist_nan:
    idx = get_time_index(i)
    target_time = round_time(time_hours[idx])

    if i == 0:
        print(f"Skipping first-of-month index {i}")
        continue

    # Find closest valid time slices
    before = max((t for t in mylist_full if t < idx), default=None)
    after = min((t for t in mylist_full if t > idx), default=None)

    if before is None or after is None:
        print(f"Missing neighbors for index {i}, skipping")
        continue

    t_before = round_time(time_hours[before])
    t_after = round_time(time_hours[after])

    total = t_after - t_before
    if total == 0:
        print(f"Degenerate time window at index {i}, skipping")
        continue

    w1 = (t_after - target_time) / total
    w2 = (target_time - t_before) / total

    interp_prev = pressure_full[before, :, :]
    interp_after = pressure_full[after, :, :]
    interpolated = w1 * interp_prev + w2 * interp_after

    pressure_full[idx, :, :] = interpolated
    pressure_converted[i, :, :] = interpolated


# Save Output as NetCDF
# Write the interpolated 00Z pressure field to NetCDF4
outfile = f"oscurstest_{input_date}.nc"
ncfile = Dataset(outfile, "w", format="NETCDF4")

# Define dimensions
ncfile.createDimension("time", None)
ncfile.createDimension("y", 92)
ncfile.createDimension("x", 180)

# Define variables
time = ncfile.createVariable("time", "f8", ("time",))
y = ncfile.createVariable("y", "f4", ("y",))
x = ncfile.createVariable("x", "f4", ("x",))
latitude = ncfile.createVariable("lat", "f4", ("y", "x"))
longitude = ncfile.createVariable("lon", "f4", ("y", "x"))
pmsl = ncfile.createVariable(
    "pmsl", "f4", ("time", "y", "x"),
    zlib=True, complevel=4, fill_value=-9999.0
)

# Assign variable metadata
time.units = "hours since 1948-01-01 00:00:00"
time.long_name = "reference time"
y.long_name = "y-index"
x.long_name = "x-index"
latitude.units = "degrees_north"
latitude.long_name = "latitude"
longitude.units = "degrees_east"
longitude.long_name = "longitude"
pmsl.units = "Mb"
pmsl.long_name = "Pressure reduced to MSL"

# Assign global attributes
ncfile.title = "Interpolated from FNMOC 6-hour pressure fields"
ncfile.Conventions = "CF-1.6"
now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")
ncfile.history = f"Created on {now} UTC"
ncfile.source = "Python OSCURS reimplementation of Fortran"

# Write data to file
time[:] = time_conv
x[:] = np.arange(1, 181, dtype="f4")
y[:] = np.arange(1, 93, dtype="f4")
latitude[:, :] = lat_grid.astype("f4")
longitude[:, :] = lon_grid.astype("f4")
pmsl[:, :, :] = pressure_converted.astype("f4")

# Close the file
ncfile.close()
print(f"Wrote NetCDF4 file: {outfile}")
