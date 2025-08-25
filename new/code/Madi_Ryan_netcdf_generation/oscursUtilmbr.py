"""
OSCURS Utility Functions
========================
Last Updated: 7/24/25

This module provides helper functions used in the OSCURS (Ocean Surface 
CURrent Simulator) Python reimplementation. It includes tools for
constructing time strings, generating ERDDAP data access URLs,
performing bilinear spatial interpolation, converting time units,
and interpolating pressure data to a custom lat/lon grid.

These utilities are designed to work with 6-hourly pressure fields retrieved
from the FNMOC dataset hosted on NOAA's ERDDAP server, and replicate
the core behavior of the original Fortran-based OSCURS model.

----------
Included Functions
----------

1. `starting_date(year, month, day, hour)`
   - Returns an ISO 8601 UTC timestamp string for ERDDAP queries.

2. `ending_date(year, month, day, hour)`
   - Returns a formatted end date string for ERDDAP time slicing.

3. `erddap_link_creator(start_date, end_date, latitude, longitude)`
   - Generates an ERDDAP OPeNDAP link for downloading a 2x2° spatial subset.

4. `points_creator(lat, lon, pmsl)`
   - Identifies the four surrounding pressure grid values needed for bilinear
     interpolation.

5. `bilinear_interpolation(x, y, points)`
   - Performs bilinear interpolation within a grid-aligned rectangle.

6. `interpolate_grid(pmsl, lat_grid, lon_grid) 
   - Interpolates an entire 2D pressure field onto a custom grid using 
     bilinear interpolation.

7. `round_time(val, digits=6)`
   - Rounds a floating-point time value to a specified precision to mitigate
     floating-point drift.

8. `get_time_index(i)`
   - Maps a 00Z index to the full 6-hourly array index (i.e., multiplies by 4).

----------
Assumptions
----------
- Pressure grids are 1°x1°, spanning latitude -89.5 to 89.5 and longitude 0.5
  to 359.5.
- Time is represented either as "seconds since 1970-01-01" or
  "hours since 1948-01-01".
- Latitude and longitude are center-aligned (e.g., 34.5, 35.5).
- Interpolation is local (no extrapolation).
- Functions gracefully handle out-of-bound or invalid inputs with `ValueError`.

----------
Dependencies
----------
- Python 3.8+
- NumPy
- Math (standard library)
"""

# Import libraries
import os
import math
import numpy as np
import xarray as xr
from datetime import datetime, timezone


def bilinear_interpolation(x, y, points):
    """
    Estimate the value of a function at a target point (x, y) using
    bilinear interpolation.

    This function computes a bilinear interpolation over a rectangular grid
    cell, using four known corner points and their associated scalar values.
    The interpolation is performed over a rectangle defined by two unique
    x-values and two unique y-values, assuming axis-aligned, evenly spaced
    corners.

    Parameters
    ----------
    x : float
        The x-coordinate of the target location where the interpolated value
        is desired.
    y : float
        The y-coordinate of the target location where the interpolated value
        is desired.
    points : list of tuple[float, float, float]
        A list of exactly four tuples, where each tuple is of the form
        (x_i, y_i, value_i), corresponding to the known value at that
        corner of the rectangle.

        The four points must:
        - Define a proper axis-aligned rectangle (e.g., not skewed or
        triangular)
        - Have two distinct x-values and two distinct y-values
        - Be ordered arbitrarily; the function internally sorts them to assign
        corners

    Returns
    -------
    float
        The interpolated value at location (x, y) using bilinear interpolation.

    Raises
    ------
    ValueError
        If fewer or more than four points are provided.
        If the four points do not define a valid rectangle
        (e.g., repeated x/y coordinates).
        If the target (x, y) lies outside the bounding rectangle
        defined by the points.
    """
    # Sort input points by x, then y, to ensure a consistent order
    points = sorted(points, key=lambda p: (p[0], p[1]))

    # Unpack the sorted points into corner coordinates and values
    try:
        (x1, y1, q11), (_x1, y2, q12), (x2, _y1, q21), (_x2, _y2, q22) = points
    except ValueError as e:
        raise ValueError(f"Expected 4 points, got {len(points)}: {points}")

    # Validate the geometry of the rectangle
    if x1 != _x1 or x2 != _x2 or y1 != _y1 or y2 != _y2:
        raise ValueError(f"Points do not form a proper rectangle: {points}")

    # Ensure (x, y) lies within the rectangle defined by the 4 corners
    if not (x1 <= x <= x2) or not (y1 <= y <= y2):
        raise ValueError(
            f"(x={x}, y={y}) not in rectangle " f"x:[{x1},{x2}], y:[{y1},{y2}]"
        )

    # Apply the bilinear interpolation formula
    return (
        q11 * (x2 - x) * (y2 - y)
        + q21 * (x - x1) * (y2 - y)
        + q12 * (x2 - x) * (y - y1)
        + q22 * (x - x1) * (y - y1)
    ) / ((x2 - x1) * (y2 - y1) + 0.0)


def starting_date(year=2001, month=1, day=17, hour=12):
    """
    Construct an ISO 8601 formatted UTC timestamp string for use in ERDDAP
    queries.

    This function builds a complete timestamp string
    (e.g., "2001-01-17T12:00:00Z") given a specific date
    and hour. It is primarily used for defining time slices in
    ERDDAP query URLs that retrieve 6-hourly data.

    Parameters
    ----------
    year : int, optional
        The calendar year for the timestamp. Default is 2001.
    month : int, optional
        The calendar month (1-12). Default is 1 (January).
    day : int, optional
        The day of the month. Default is 17.
    hour : int, optional
        The hour of the day in 24-hour format. Must be one of [0, 6, 12, 18].
        Default is 12.

    Returns
    -------
    start_date: str
        A string representing the formatted datetime in UTC,
        formatted as: 'YYYY-MM-DDTHH:00:00Z', for use in ERDDAP query URLs.
    """
    # Convert year to string
    stryear = str(year)

    # Convert month to string and pad to 2 digits
    strmonth = str(month)
    strmonth = strmonth.rjust(2, "0")

    # Convert day to string and pad to 2 digits
    strday = str(day)
    strday = strday.rjust(2, "0")

    # Convert hour to string and pad to 2 digits
    strhour = str(hour)
    strhour = strhour.rjust(2, "0")

    # Concatenate components into ISO 8601 UTC timestamp
    start_date = (
        stryear + "-" + strmonth + "-" + strday + "T" + strhour + ":00:00Z"
    )
    return start_date


def ending_date(year, month, day=17, hour=12):
    """
    Construct an ISO 8601 formatted UTC timestamp string to define an end date
    for ERDDAP queries.

    This function creates a string formatted as 'YYYY-MM-DDTHH:00:00Z' to be
    used as an ERDDAP query end date. The formatting matches the requirements
    for accessing subdaily datasets (e.g., 6-hourly pressure fields). It
    ensures consistent formatting by zero-padding the components and assumes
    that time is expressed in UTC.

    Parameters
    ----------
    year : int
        The calendar year of the end date (e.g., 2025).
    month : int
        The calendar month (1-12) of the end date.
    day : int, optional
        The day of the month for the end date. Default is 17.
    hour : int, optional
        The hour of the day in 24-hour format. Must be one of [0, 6, 12, 18].
        Default is 12.

    Returns
    -------
    end_date: str
        A string representing the UTC end datetime, formatted as:
        'YYYY-MM-DDTHH:00:00Z', for use in ERDDAP query URLs.
    """
    # Convert year to string
    stryear = str(year)

    # Convert month to string and pad with zero
    strmonth = str(month)
    strmonth = strmonth.rjust(2, "0")

    # Convert day to string and pad to two digits
    strday = str(day)
    strday = strday.rjust(2, "0")

    # Convert hour to string and pad to two digits
    strhour = str(hour)
    strhour = strhour.rjust(2, "0")

    # Format into ISO 8601 UTC datetime string expected by ERDDAP
    end_date = (
        stryear + "-" + strmonth + "-" + strday + "T" + strhour + ":00:00Z"
    )

    return end_date


def erddap_link_creator(start_date, end_date, latitude, longitude):
    """
    Generate a complete ERDDAP OPeNDAP link for downloading gridded data
    over a small bounding box around a target (lat, lon) and time range.

    This function constructs an ERDDAP data request URL for extracting a subset
    of the FNMOC 6-hourly pressure dataset using a start date, end date, and a
    center point (latitude, longitude). The latitude and longitude inputs are
    expanded to their nearest grid-aligned bounding box (1° spacing assumed).

    Parameters
    ----------
    start_date : str
        ISO 8601 formatted string (e.g., "2025-03-01T00:00:00Z"), usually
        created using `starting_date()`.
    end_date : str
        ISO 8601 formatted string (e.g., "2025-03-02T18:00:00Z"), usually
        created using `ending_date()`.
    latitude : float
        Target latitude for the query. The function will extract the
        surrounding 1° cell.
    longitude : float
        Target longitude for the query. The function will extract the
        surrounding 1° cell.

    Returns
    -------
    link: str
        A fully formatted ERDDAP URL string that can be used to download a
        subset of data.
    """

    # Round latitude to its surrounding grid box (e.g., 34.2 → 34.5 to 35.5)
    latitude5 = latitude + 0.5
    lat_ceil = math.ceil(latitude5) - 0.5  # Upper edge of the 1° grid cell
    lat_floor = math.floor(latitude5) - 0.5  # Lower edge

    # Same logic for longitude
    longitude5 = longitude + 0.5
    lon_ceil = math.ceil(longitude5) - 0.5
    lon_floor = math.floor(longitude5) - 0.5

    # Convert values to strings for URL formatting
    strlonC = str(lon_ceil)
    strlonF = str(lon_floor)
    strlatC = str(lat_ceil)
    strlatF = str(lat_floor)

    # Construct ERDDAP URL for extracting pressure (pmsl) subset
    link = (
        "http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnWPr.nc?pmsl[("
        + start_date
        + "):1:("
        + end_date
        + ")][("
        + strlatF
        + "):1:("
        + strlatC
        + ")][("
        + strlonF
        + "):1:("
        + strlonC
        + ")]"
    )

    return link


def points_creator(lat, lon, pmsl):
    """
    Identify the four surrounding grid points (corners of a 1x1 degree cell)
    needed for bilinear interpolation at a given target latitude and longitude.

    This function is designed to match the spatial resolution and coordinate
    system of the ERDDAP pressure dataset, which uses a 1° x 1° global grid:
      - latitude: from -89.5 to 89.5 in 1.0° increments (180 rows)
      - longitude: from 0.5 to 359.5 in 1.0° increments (360 columns)

    The function returns four tuples, each representing a corner point:
        (lat_corner, lon_corner, value at that corner)

    Parameters
    ----------
    lat : float
        Target latitude (e.g., 23.7) for interpolation.
    lon : float
        Target longitude (e.g., 132.2) for interpolation.
    pmsl : 2D array-like
        Pressure field on a regular grid [lat_index, lon_index], typically
        from NetCDF.

    Returns
    -------
    points: list of tuple
        A list of four (lat, lon, value) tuples representing the
        corners of the grid cell that encloses the (lat, lon) point. The
        order is:
        - upper right
        - lower right
        - upper left
        - lower left

    Raises
    ------
    ValueError
        If the requested (lat, lon) point lies outside the supported
        grid bounds or if the indices go out of bounds in the pressure array.
    """
    # Define coordinate origin of the pressure grid
    lat_start = -89.5
    lon_start = 0.5

    # Define coordinate origin of the pressure grid
    lat = min(max(lat, lat_start + 1e-6), 89.5 - 1e-6)
    lon = min(max(lon, lon_start + 1e-6), 359.5 - 1e-6)

    # Calculate the lat/lon values of the grid cell bounding box
    lat_floor = math.floor(lat - 0.5) + 0.5
    lat_ceil = lat_floor + 1.0
    lon_floor = math.floor(lon - 0.5) + 0.5
    lon_ceil = lon_floor + 1.0

    # Convert lat/lon values to array indices by offsetting from grid origin
    lat_idx_floor = int(round(lat_floor - lat_start))
    lat_idx_ceil = int(round(lat_ceil - lat_start))
    lon_idx_floor = int(round(lon_floor - lon_start))
    lon_idx_ceil = int(round(lon_ceil - lon_start))

    try:
        # Extract the 4 corner pressure values from the array using
        # computed indices
        points = [
            (lat_ceil, lon_ceil, pmsl[lat_idx_ceil, lon_idx_ceil]),
            (lat_floor, lon_ceil, pmsl[lat_idx_floor, lon_idx_ceil]),
            (lat_ceil, lon_floor, pmsl[lat_idx_ceil, lon_idx_floor]),
            (lat_floor, lon_floor, pmsl[lat_idx_floor, lon_idx_floor]),
        ]
    except IndexError as e:
        # Raise a clear error if indexing failed (e.g., lat/lon outside bounds)
        raise ValueError(f"Index error for lat={lat}, lon={lon}: {e}")

    return points


def interpolate_grid(pmsl, lat_grid, lon_grid):
    """
    Interpolate a source pressure field onto a target lat/lon grid using
    bilinear interpolation.

    This function loops through each target grid point defined by `lat_grid`
    and `lon_grid` and performs bilinear interpolation using the source
    field `pmsl`. It uses the `points_creator()` function to extract the
    four neighboring grid cell values and `bilinear_interpolation()` to
    compute the interpolated result.

    Parameters
    ----------
    pmsl : 2D numpy.ndarray
        Source pressure field on a regular 1° global grid. Shape: [lat, lon]
        (typically 180 x 360).
    lat_grid : 2D numpy.ndarray
        Target latitude grid of shape [92, 180], where each element specifies
        the latitude to interpolate at.
    lon_grid : 2D numpy.ndarray
        Target longitude grid of shape [92, 180], where each element specifies
        the longitude to interpolate at.

    Returns
    -------
    interpolated : 2D numpy.ndarray
        The interpolated pressure field of shape [92, 180], matching the shape
        of `lat_grid` and `lon_grid`. Points outside valid bounds are filled
        with NaN.

    Raises
    ------
    ValueError
        Raised internally for invalid interpolation points (e.g., outside of
        the source domain). These points are caught and logged, and NaN is
        inserted into the output.
    """
    # Extract the number of rows and columns in the target grid
    nrows, ncols = lat_grid.shape

    # Preallocate output array for interpolated values
    interpolated = np.empty((nrows, ncols), dtype=np.float32)

    # Loop over every grid cell in the target domain
    for i in range(nrows):
        for j in range(ncols):
            lat_i = lat_grid[i, j]
            lon_i = lon_grid[i, j]
            try:
                # Get the four surrounding points from the source field
                points = points_creator(lat_i, lon_i, pmsl)

                # Apply bilinear interpolation using those four points
                interpolated[i, j] = bilinear_interpolation(
                    lat_i, lon_i, points
                )
            except ValueError as e:
                # Handle any out-of-bounds or invalid cells gracefully
                print(
                    f"Interpolation failed at ({i},{j}) lat={lat_i}, lon={lon_i}: {e}"
                )
                interpolated[i, j] = np.nan

    return interpolated


def round_time(val, digits=6):
    """
    Round a floating-point time value to a fixed number of decimal places.

    Parameters
    ----------
    val : float
        The numeric value to be rounded (typically a time in hours).
    digits : int, optional
        Number of decimal places to round to. Default is 6, which is
        generally sufficient for resolving small floating-point drift
        in hourly time coordinates.

    Returns
    -------
    float
        The rounded value with reduced precision drift.
    """
    # Use built-in round function to control floating-point precision drift
    return round(val, digits)


def get_time_index(i):
    """
    Convert a 6-hourly time slice index to the corresponding index in a
    finer-resolution time array.

    This function is used when pressure data are stored at 6-hour intervals
    (e.g., 00Z, 06Z, 12Z, 18Z), and a coarser index (e.g., for 00Z only)
    needs to be mapped back to the corresponding index in the full-resolution
    time array (which includes 4 time steps per day).

    Parameters
    ----------
    i : int
        The index into the daily 00Z-only pressure array
        (e.g., `pressure_converted`).
        For example, `i = 0` corresponds to the first 00Z timestep, `i = 1`
        to the second, etc.

    Returns
    -------
    int
        The corresponding index into the full 6-hourly pressure array.
        Calculated as `i * 4` assuming 4 timesteps per day.
    """
    # Multiply by 4 to access the 00Z time slice within the 6-hourly array
    return i * 4


def open_minimal(path):
    """
    Open a NetCDF file and return only the Fortran-style OSCURS variables
    needed for appending into yearly files.

    This function ensures consistency with the legacy Fortran outputs by
    stripping out auxiliary fields (e.g., latitude, longitude) and retaining
    only the essential variables:
      - 'time' (dimension of hours since 1948-01-01)
      - 'pmsl' (pressure reduced to mean sea level)

    Parameters
    ----------
    path : str
        Path to the NetCDF file to open (daily or monthly OSCURS output).

    Returns
    -------
    xarray.Dataset
        A dataset containing only 'time' and 'pmsl', with dimensions
        (time, y, x). The structure matches the Fortran-generated yearly
        files used by OSCURS.

    Raises
    ------
    ValueError
        If neither 'time' nor 'time_series' is present.
        If 'pmsl' is missing from the dataset.
    """
    ds = xr.open_dataset(path, chunks={"time": 31})

    # Handle alternate naming from some NetCDFs
    if "time" not in ds and "time_series" in ds:
        ds = ds.rename({"time_series": "time"})

    if "time" not in ds:
        raise ValueError(f"{path}: no 'time' or 'time_series' variable found")
    if "pmsl" not in ds:
        raise ValueError(f"{path}: no 'pmsl' variable found")

    # Ensure only Fortran-style variables are kept
    return ds[["time", "pmsl"]]


def upsert_daily_into_year(ds_year, ds_daily, log_file, threshold=0.01):
    """
    Merge a daily OSCURS pressure dataset into a yearly dataset.

    This function appends daily-resolution pressure fields into an existing
    yearly file while ensuring continuity and consistency:
      - Non-overlapping time steps from the daily dataset are appended.
      - Overlapping time steps are compared, and yearly values are replaced
        only if the maximum absolute difference exceeds the threshold.
      - Any corrections applied are logged for traceability.

    Parameters
    ----------
    ds_year : xarray.Dataset
        The yearly OSCURS dataset, containing 'time' and 'pmsl'
        (dimensions: time, y, x).
    ds_daily : xarray.Dataset
        The daily OSCURS dataset to merge into the yearly dataset.
    log_file : str
        Path to the log file where correction details will be recorded.
    threshold : float, optional
        Maximum allowed absolute difference (in millibars) before a
        replacement is made. Default is 0.01 mb.

    Returns
    -------
    xarray.Dataset
        The updated yearly dataset, containing:
          - Original data with corrections applied.
          - All new non-overlapping daily slices appended.
          - Sorted and deduplicated time dimension.
    """
    # Ensure both datasets are sorted along the time axis
    ds_daily = ds_daily.sortby("time")
    ds_year = ds_year.sortby("time")

    # Extract raw time arrays for convenience
    y_times = ds_year.time.values
    d_times = ds_daily.time.values

    # Build a boolean mask: True where yearly times overlap with daily times
    overlap_mask = np.isin(y_times, d_times)

    # Keep only the non-overlapping portion of the yearly dataset
    # If there are overlaps, drop them from ds_year; otherwise keep ds_year unchanged
    ds_year_wo = (
        ds_year.sel(time=~xr.DataArray(overlap_mask, dims=("time",)))
        if overlap_mask.any()
        else ds_year
    )

    # If there are overlapping time steps between yearly and daily
    if overlap_mask.any():
        # Extract the overlapping slices from yearly
        y_overlap = ds_year.sel(
            time=xr.DataArray(y_times[overlap_mask], dims=("time",))
        )
        # Extract the overlapping slices from daily (aligned by time)
        d_overlap = ds_daily.sel(time=y_overlap.time)

        # Loop through each overlapping time step
        for i, t in enumerate(y_overlap.time.values):
            # Old pressure values from yearly
            old_vals = y_overlap["pmsl"].isel(time=i).values
            # New pressure values from daily
            new_vals = d_overlap["pmsl"].isel(time=i).values
            # Compute maximum absolute difference between the two
            diff = np.nanmax(np.abs(new_vals - old_vals))
            # If the difference exceeds threshold, replace yearly values with daily
            if diff > threshold:
                y_overlap["pmsl"][i] = new_vals
                # Log the correction for traceability
                with open(log_file, "a") as log:
                    log.write(
                        f"    Correction @ {str(t)} | max Δ {diff:.6f} mb\n"
                    )

        # Take all daily times not in yearly (non-overlapping new data)
        d_nonoverlap = ds_daily.sel(time=~np.isin(d_times, y_times))

        # Concatenate:
        # - yearly without overlaps
        # - corrected overlap slices
        # - new non-overlapping daily slices
        combined = xr.concat([ds_year_wo, y_overlap, d_nonoverlap], dim="time")

    # If there are no overlaps at all, just concatenate yearly + daily directly
    else:
        combined = xr.concat([ds_year, ds_daily], dim="time")

    # Return the merged dataset with times sorted and duplicates removed
    return combined.sortby("time").drop_duplicates("time")


def upsert_monthly_into_year(ds_year, ds_monthly, log_file):
    """
    Merge a monthly OSCURS pressure dataset into a yearly dataset, ensuring
    that monthly values always take precedence.

    This function is designed for situations where the yearly file may contain
    older or less accurate values that should be corrected by higher-fidelity
    monthly reprocessing. Any overlapping time steps in the yearly dataset
    are replaced with the corresponding values from the monthly dataset,
    and these replacements are logged for traceability. Non-overlapping
    monthly records are appended so that the output contains a complete,
    deduplicated time series.

    Parameters
    ----------
    ds_year : xarray.Dataset
        Yearly dataset containing `time` and `pmsl` variables. This serves as
        the base dataset into which monthly values will be merged.
    ds_monthly : xarray.Dataset
        Monthly dataset containing `time` and `pmsl` variables. Considered
        authoritative and used to overwrite or extend the yearly dataset.
    log_file : str
        Path to a text log file where overwrite operations are recorded,
        with one entry per corrected timestamp.

    Returns
    -------
    xarray.Dataset
        A combined dataset containing the original yearly records with
        monthly corrections applied and any missing monthly records appended.
        The result is sorted by time and deduplicated to ensure a clean
        continuous record.

    Raises
    ------
    ValueError
        If either dataset does not contain the required `time` or `pmsl` variables.
    IOError
        If the log file cannot be opened for appending during overwrite logging.
    """

    # Ensure both datasets are sorted by time for consistent merging
    ds_monthly = ds_monthly.sortby("time")
    ds_year = ds_year.sortby("time")

    # Extract time values from yearly and monthly datasets
    y_times = ds_year.time.values
    m_times = ds_monthly.time.values

    # Boolean mask marking where yearly times overlap with monthly times
    overlap_mask = np.isin(y_times, m_times)

    # Exclude overlapping times from yearly dataset (if any)
    ds_year_wo = (
        ds_year.sel(time=~xr.DataArray(overlap_mask, dims=("time",)))
        if overlap_mask.any()
        else ds_year
    )

    if overlap_mask.any():
        # Subset yearly and monthly datasets to overlapping times
        y_overlap = ds_year.sel(
            time=xr.DataArray(y_times[overlap_mask], dims=("time",))
        )
        m_overlap = ds_monthly.sel(time=y_overlap.time)

        # Replace overlapping yearly values with authoritative monthly values
        for i, t in enumerate(y_overlap.time.values):
            y_overlap["pmsl"][i] = m_overlap["pmsl"].isel(time=i).values
            with open(log_file, "a") as log:
                log.write(f"    Overwrote @ {str(t)}\n")

        # Identify monthly records not already in yearly
        m_nonoverlap = ds_monthly.sel(time=~np.isin(m_times, y_times))

        # Concatenate yearly without overlaps, updated overlaps, and non-overlap monthly
        combined = xr.concat([ds_year_wo, y_overlap, m_nonoverlap], dim="time")
    else:
        # If no overlaps exist, concatenate directly
        combined = xr.concat([ds_year, ds_monthly], dim="time")

    # Return dataset sorted by time with duplicate timestamps removed
    return combined.sortby("time").drop_duplicates("time")
