# OSCURS Python Modernization

OSCURS is an ocean surface currents model historically powered by daily 6-hour FNMOC mean sea level pressure (MSLP) interpolated to an equal-area OSCURS grid and then run via legacy Fortran and a PHP web front end.  
This repo documents and delivers a **Python-based** replacement that reproduces the Fortran results while modernizing data handling, storage, automation, and the user interface.

---

## Background
- **Legacy site:** <https://oceanview.pfeg.noaa.gov/oscurs/>  
- **Use case:** Daily pressure → interpolation to OSCURS equal-area grid → OSCURS trajectory model → downloadable tracks via web UI.

---

## Old Process (For Reference)

### Data Workflow
1. **FNMOC** sftp’s  360° x 180° SLP every 6 hours to `oceanwatch`.  
   - Fleet Numerical Meteorology and Oceanography Center runs numerical models to generate 6-hourly mean sea level pressure and other parameters (GRIB format).
   - Forecasts are used for the Navy fleet but not generally stored by FNMOC. ERD has stored and used these outputs for decades to generate the Bakun Upwelling Index.
2. **Cron job on oceanwatch** converts GRIB→NetCDF.  
   - Daily: generates `monthly_pressure.nc` containing current month's data (~7am; may have missing data).  
   - Monthly: appends to yearly NetCDF, calculates Ekman transports & upwelling index, captures missing data; serves files on ERDDAP.
3. **Daily cron** on oceanwatch calculates 6-hr upwelling index/transports, displayed on web page; sends to `oceanview`.
4. On **oceanview** (~07:45am):
   - Interpolate FNMOC SLP (180×360, 1°) → OSCURS equal-area grid (180×92).
   - Extract 00:00 UTC slice each day; save to `this_month_oscurs.nc`; append to `oscurs_pressure.nc` (huge).
   - Ideally: split `oscurs_pressure.nc` into yearly/decade files.

**Sources:**
- Input:
  - [ERDDAP](https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnTransMon.graph?P_msl%5B(202507-09T06:00:00Z)%5D%5B(-90.0):(90.0)%5D%5B(0.0):(359.0)%5D,v%5B(2025-07-09T06:00:00Z)%5D%5B(-90.0):(90.0)%5D%5B(0.0):(359.0)%5D)  
  - [THREDDS](https://oceanview.pfeg.noaa.gov/thredds/dodsC/Model/FNMOC/current_month/this_month_transport.nc.html)  
- Interpolated output:
  - [ERDDAP](https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnOscurs.html)  
  - [THREDDS](https://oceanview.pfeg.noaa.gov/thredds/dodsC/oscurs_model/oscurs_pressure.nc.html)

**Legacy Fortran code:**
- Daily: `old/code/daily_netcdf_generation/{pres_interp_daily.f, pres_append_daily.f}`
- Monthly: `old/code/monthly_netcdf_generation/{pres_interp_monthly.f, pres_append.f}`
- Early Python attempt: `old/code/Ryan_netcdf_generation` (close, but not exact).

---

## OSCURS Model Workflow (Old)
1. Interpolated data above is input to the OSCURS model.
2. Model code in `old/code/model_code`; parameters in `oscurs_web_page`.
3. Web page (Dojo + PHP) executes model with user inputs.
4. Can run via wget, e.g.:

Ex:
```bash
wget -O test.txt "https://oceanview.pfeg.noaa.gov/oscurs/runOscurs9.php?cl=1&latdeg=45&latmin=0&londeg=175&lonmin=0&year=2011&mon=NOV&day=4&nnnn=20&factor=1&angle=0&ddfac=1&outfile=filename1.csv"
```
This will give you a 20 day track starting at 45N, 175W, Nov 4 2011, where the output will be written to the file on your computer called test.txt in your current directory.

---

## New Process (Python Modernization)

### Overview
The legacy OSCURS pipeline relied on Fortran scripts and large monolithic NetCDF files.  
The new workflow replaces these with Python-based processes that:
- Reproduce the Fortran results with high numerical parity.
- Reduce file size and improve manageability.
- Prepare for a more flexible model execution setup.

### Steps
1. **Develop Python code** to interpolate 1-degree mean sea level pressure (MSLP) to the OSCURS 180×92 grid.  
   - Status: Complete; validated against Fortran outputs.
2. **Reconfigure the OSCURS pressure file** into chunks (e.g., yearly) so it is smaller and easier to manage.
3. **Operationalize daily interpolation process** to add daily interpolated data to the yearly file.
   - At set intervals (monthly, yearly), check for and integrate corrections to the data.
4. **Develop Python code** to run OSCURS, producing results as close as possible to the Fortran code.
5. **Decide where the model code will reside** (e.g., local, server, hosted repository).
6. **Determine if a RESTful service** similar to the current PHP code should be provided for remote execution.
7. **Identify other needed steps** such as testing and validation procedures.
8. **Develop a user interface** for running the model.  
   - Decide where the UI will reside and how users will access it.

### Key Improvements Over the Old Process
- Yearly NetCDF files instead of a single large monolithic file.
- Use of Python for interpolation and model execution instead of Fortran.
- Flexibility to adapt the workflow for new interfaces or hosting environments.
- Structured approach to incorporating corrected data at regular intervals.

---
