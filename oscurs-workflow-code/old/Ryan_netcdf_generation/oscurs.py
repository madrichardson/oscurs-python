import os
import numpy as np
import urllib
import urllib2
import netCDF4
from datetime import datetime, timedelta
from netCDF4 import num2date, date2num, Dataset
import matplotlib.pyplot as plt
import math
from oscursUtil import *    #verify this works
import csv
import calendar
import sys


input_date   = sys.argv[1]  #YYYYMM, don't use strings in command line


#Set up Lat/Lon grid
results = []
with open('lola92180.txt') as inputfile:
    for row in csv.reader(inputfile):
        results.append(row)
lat_grid  = np.zeros((92,180),dtype = 'float64')
lon_grid  = np.zeros((92,180),dtype = 'float64')
count  = 0
countj = 0
latjunk = []
lonjunk = []
for line in results:
  junk    = str.split(line[0])
  lonjunk.append((float(junk[0])-360)*-1)
  latjunk.append(float(junk[1]))
  if count == 91:
    lat_grid[:,countj] = np.array(latjunk)
    lon_grid[:,countj] = np.array(lonjunk)
    count   = -1
    countj  = countj+1
    latjunk = []
    lonjunk = []
  count   = count + 1

#Get data using input date
start_year   = input_date[0:4]
start_month  = input_date[4:6]
start_day    = '01'
mon_range    = calendar.monthrange (int(start_year),int(start_month))
end_day      = str(mon_range[1]).rjust(2,'0')
start_string = start_year + '-' + start_month + '-' + start_day
end_string   = start_year + '-' + start_month + '-' + end_day
url = 'http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnPres6.nc?p_msl[(' + start_string + '):1:('\
+ end_string + 'T18:00:00Z)]''[(-89.5):1:(89.5)][(0.5):1:(359.5)]'
file = 'erdlasFnPres6'
urllib.urlretrieve(url, file)
ncfile = netCDF4.Dataset(file)
ncv = ncfile.variables
pmsl = ncv['p_msl'][:,:,:]
time = ncv['time'][:]  #seconds since 1970-01-01T00:00:00Z
time_conv = np.zeros(len(pmsl)/4,dtype = 'float64')
count = 0
for i in range(len(pmsl)/4):
  a = num2date(time[count],'seconds since 1970-01-01T00:00:00Z')
  b = date2num(a,'hours since 1948-1-1')
  time_conv[i] = b 
  count = count + 4

#Create interpolated pressure array
pressure_converted = np.zeros((len(pmsl)/4,92,180))
count = 0
for i in range(len(pmsl)/4):
  a = interpolate_grid(pmsl[i+count,:,:],lat_grid,lon_grid)
  pressure_converted[i,:,:] = a
  count = count + 3
pressure_full = np.zeros((len(pmsl),92,180))
for i in range(len(pmsl)):
  a = interpolate_grid(pmsl[i,:,:],lat_grid,lon_grid)
  pressure_full[i,:,:] = a

#Find nan values, and interpolate through time
mylist_fullnan = list(set(np.where(np.isnan(pressure_full) == True)[0]))
mylist_full    = list(set(np.where(np.isnan(pressure_full) == False)[0]))
mylist_nan     = list(set(np.where(np.isnan(pressure_converted) == True)[0]))
mylist         = list(set(np.where(np.isnan(pressure_converted) == False)[0]))
if len(mylist_nan) == 0:
  print 'There is no missing data at 00:00:00 time stamps'
  pass
else:
  for i in mylist_nan:
    if i == 0:
      if int(start_month) == 1:
        mon_range    = calendar.monthrange (int(start_year)-1,12)
        end_day      = str(mon_range[1]).rjust(2,'0')
        start_string = str(int(start_year)-1).rjust(4,'0') + '-' + '12' + '-' + start_day
        end_string   = str(int(start_year)-1).rjust(4,'0') + '-' + '12' + '-' + end_day
      else:
        mon_range    = calendar.monthrange (int(start_year),int(start_month)-1)
        end_day      = str(mon_range[1]).rjust(2,'0')
        start_string = start_year + '-' + str(int(start_month)-1).rjust(2,'0') + '-' + start_day
        end_string   = start_year + '-' + str(int(start_month)-1).rjust(2,'0') + '-' + end_day
      url = 'http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnPres6.nc?p_msl[(' + start_string + '):1:('\
    + end_string + 'T18:00:00Z)]''[(-89.5):1:(89.5)][(0.5):1:(359.5)]'
      file = 'erdlasFnPres6'
      urllib.urlretrieve(url, file)
      ncfile_p = netCDF4.Dataset(file)
      ncv_p = ncfile_p.variables
      pmsl_p = ncv_p['p_msl'][:,:,:]
      pressure_full_prev = np.zeros((len(pmsl_p),92,180))
      for j in range(len(pmsl_p)):
        a = interpolate_grid(pmsl_p[j,:,:],lat_grid,lon_grid)
        pressure_full_prev[j,:,:] = a
      if len(list(set(np.where(np.isnan(pressure_full_prev) == True)[0]))) == 0:
        interp_prev = pressure_full_prev[-1]
        index_p_dist = 1
      else:
        mylist_full_prev = list(set(np.where(np.isnan(pressure_full_prev) == False)[0]))
        index_p_dist = len(pmsl_p) - mylist_full_prev[-1] + 1
        interp_prev = pressure_full_prev[mylist_full_prev[-1],:,:]
      index_a_dist = mylist_full[0]
      interp_after = pressure_full[mylist_full[0],:,:]
      index_total  = index_a_dist + index_p_dist  
      pressure_full[i,:,:] = (index_a_dist/index_total)*insterp_prev + (index_p_dist/index_total)*interp_after
      pressure_converted[i,:,:] = (index_a_dist/index_total)*insterp_prev + (index_p_dist/index_total)*interp_after
    elif i in range(1,len(pressure_converted)-1):
      data_p_index = np.where(np.array(mylist_full)<(i*4))[0][-1]
      data_p       = mylist_full[data_p_index]
      index_p_dist = (i*4) - data_p
      interp_prev  = pressure_full[data_p,:,:]
      data_a_index = np.where(np.array(mylist_full)>(i*4))[0][0]
      data_a       = mylist_full[data_a_index]
      index_a_dist = data_a - (i*4)
      interp_after = pressure_full[data_a,:,:]
      index_total  = index_a_dist + index_p_dist
      interp_prev  = np.array(interp_prev)
      interp_after = np.array(interp_after)
      prev_perc    = (float(index_a_dist)/index_total)*interp_prev
      aftr_perc    = (float(index_p_dist)/index_total)*interp_after
      pressure_full[i*4,:,:] = np.add(prev_perc,aftr_perc)
      pressure_converted[i,:,:] = np.add(prev_perc,aftr_perc)
    else:
      checker = []
      for j in range(i*4,(i*4)+4):
        if j in mylist_full:
          checker.append('yes')
        else:
          checker.append('no')
      if 'yes' in checker:
        data_p_index = np.where(np.array(mylist_full)<(i*4))[0][-1]
        data_p       = mylist_full[data_p_index]
        index_p_dist = (i*4) - data_p
        interp_prev  = pressure_full[data_p,:,:]
        data_a_index = np.where(np.array(mylist_full)>(i*4))[0][0]
        data_a       = mylist_full[data_a_index]
        index_a_dist = data_a -(i*4)
        interp_after = pressure_full[data_a,:,:]
        index_total = index_a_dist + index_p_dist
        interp_prev  = np.array(interp_prev)
        interp_after = np.array(interp_after)
        prev_perc    = (float(index_a_dist)/index_total)*interp_prev
        aftr_perc    = (float(index_p_dist)/index_total)*interp_after
        pressure_full[i*4,:,:] = np.add(prev_perc,aftr_perc)
        pressure_converted[i,:,:] = np.add(prev_perc,aftr_perc)
      else:
        print 'Data not available to interpolate last data point'


#Create NC file
ncfile  = Dataset('oscurstest_' + input_date + '.nc', 'w')
#Create Dimensions
latdim  = ncfile.createDimension('y', 92)
londim  = ncfile.createDimension('x', 180)
timedim  = ncfile.createDimension('time', None)
#Create Variables
y            = ncfile.createVariable('y','f4',('y'))
x            = ncfile.createVariable('x','f4',('x'))
latitude     = ncfile.createVariable('lat','f4',('y','x'))
longitude    = ncfile.createVariable('lon','f4',('y','x'))
time         = ncfile.createVariable('time','f4',('time'))
pmsl         = ncfile.createVariable('pmsl','f4',('time','y','x'),fill_value=-9999.0)
#Create Attributes
#time attributes
time.long_name     = 'reference time'
time.units         = 'hours since 1948-1-1'    
#Y attributes
y.long_name        = 'y-index'
#X Attributes
x.long_name        = 'x-index'
#latitude Attributes
latitude.long_name = 'latitude'
latitude.units     = 'degrees_north'
#longitude Attributes
longitude.long_name = 'longitude'
longitude.units     = 'degrees_east'
#pressure Attributes
pmsl.long_name     = 'Pressure reduced to MSL'
pmsl.units         = 'Mb'
#Global Attributes
ncfile.title       = 'Interpolated from FNMOC 6-hr pressure\000\000\000+'
ncfile.Conventions = 'COARDS'
#Fill variables
time[:]        = time_conv
x[:]           = range(1,181)
y[:]           = range(1,93)
latitude[:,:]  = lat_grid[:,:]
longitude[:,:] = lon_grid[:,:]
pmsl[:,:,:]    = pressure_converted[:,:,:]
ncfile.close()

print 'all clear'