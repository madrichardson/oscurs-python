# Step 3: Read pressure data & interpolate to OSCURS grid
# -------------------------------------------------------
# In Fortran, this is handled by "bilin" (bilinear interpolation).
# We'll replicate with SciPy for convenience.

import xarray as xr
from scipy.interpolate import RegularGridInterpolator

# Example NetCDF file with mean sea-level pressure (pmsl)
# Replace with your actual file (e.g., "oscurs_pres_200005.nc")
nc_file = "oscurs_pres_example.nc"

# Open dataset
ds = xr.open_dataset(nc_file)

# Inspect variables
print(ds)

# Assume dataset has variables: "p_msl", "latitude", "longitude", "time"
pmsl = ds["p_msl"]  # shape: (time, lat, lon)
lat_src = ds["latitude"].values
lon_src = ds["longitude"].values

# Ensure longitude is -180..180 like OSCURS
lon_src = np.where(lon_src > 180, lon_src - 360, lon_src)

# Create bilinear interpolator for a single time slice
def interpolate_to_grid(field, lat_src, lon_src, lat_grid, lon_grid):
    """
    Bilinear interpolation from source (lat_src, lon_src) grid
    to OSCURS (lat_grid, lon_grid).
    """
    interp_func = RegularGridInterpolator(
        (lat_src, lon_src),
        field,
        bounds_error=False,
        fill_value=np.nan
    )
    pts = np.array([lat_grid.ravel(), lon_grid.ravel()]).T
    result = interp_func(pts)
    return result.reshape(lat_grid.shape)

# Test on the first time step
field0 = pmsl.isel(time=0).values
pmsl_interp = interpolate_to_grid(field0, lat_src, lon_src, lat_grid, lon_grid)

print("Interpolated field shape:", pmsl_interp.shape)

# Quick plot
import matplotlib.pyplot as plt

plt.figure(figsize=(8,4))
plt.pcolormesh(lon_grid, lat_grid, pmsl_interp, cmap="coolwarm")
plt.colorbar(label="pmsl (Pa)")
plt.title("Interpolated PMSL onto OSCURS grid (first timestep)")
plt.show()
