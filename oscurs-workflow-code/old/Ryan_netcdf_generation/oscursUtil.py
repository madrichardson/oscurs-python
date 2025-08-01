def bilinear_interpolation(x, y, points):
  '''Interpolate (x,y) from values associated with four points.

  The four points are a list of four triplets:  (x, y, value).
  The four points can be in any order.  They should form a rectangle.
    >>> bilinear_interpolation(12, 5.5,
    ...                        [(10, 4, 100),
    ...                         (20, 4, 200),
    ...                         (10, 6, 150),
    ...                         (20, 6, 300)])
    165.0

  '''
  # See formula at:  http://en.wikipedia.org/wiki/Bilinear_interpolation
  points = sorted(points)               # order points by x, then by y
  (x1, y1, q11), (_x1, y2, q12), (x2, _y1, q21), (_x2, _y2, q22) = points
  if x1 != _x1 or x2 != _x2 or y1 != _y1 or y2 != _y2:
    raise ValueError('points do not form a rectangle')
  if not x1 <= x <= x2 or not y1 <= y <= y2:
   raise ValueError('(x, y) not within the rectangle')
  return (q11 * (x2 - x) * (y2 - y) + q21 * (x - x1) * (y2 - y) + q12 * (x2 - x) * (y - y1) +\
  q22 * (x - x1) * (y - y1)) / ((x2 - x1) * (y2 - y1) + 0.0)


def starting_date(year=2001,month=1,day=17,hour=12):
  """
  **Purpose:**
  
  starting_date creates the start date string used in the erddap link generating function.  Day and hour are required only for 
  the 6 hourly calculations.
  
  **Input:**
  
    year:   is the start date year
  
    month:  is the start date month
  
    day:    is the start date day
    
    hour    is the start date hour in 00,06,12, or 18
  
  
  **Output:**
  
    start_date:  is the string start date required for the erddap link creator
  
  """
  stryear  = str(year)
  strmonth = str(month)
  strmonth = strmonth.rjust(2,'0')
  strday   = str(day)
  strday   = strday.rjust(2,'0')
  strhour  = str(hour)
  strhour  = strhour.rjust(2,'0')
  start_date = stryear + '-' + strmonth + '-' + strday + 'T' + strhour + ':00:00Z'
  return start_date


def ending_date(year,month,day=17,hour=12):
  """
  **Purpose:**
  
  ending_date creates the end date string used in the erddap link generating function.  Day and hour are required only for 
  the 6 hourly calculations.
  
  **Input:**
  
    year:   is the end date year
  
    month:  is the end date month
  
    day:    is the end date day
    
    hour    is the end date hour in 00,06,12, or 18
  
  
  **Output:**
  
    end_date:  is the string end date required for the erddap link creator
  
  """
  stryear  = str(year)
  strmonth = str(month)
  strmonth = strmonth.rjust(2,'0')
  strday   = str(day)
  strday   = strday.rjust(2,'0')
  strhour  = str(hour)
  strhour  = strhour.rjust(2,'0')
  end_date = stryear + '-' + strmonth + '-' + strday + 'T' + strhour + ':00:00Z'
  return end_date


def erddap_link_creator(start_date,end_date,latitude,longitude):
  """
  **Purpose:**
  
  erddap_link_creator generates the desired erddap link based on given start_date,end_date, latitude, and longitude
  
  **Input:**
  
    start_date:    is the start date string created by the starting_date function
  
    end_date:      is the end date string created by the ending_date function
  
    latitude:      is the desired latitude you want to interpolate on.  The function will pull out both floor and ceiling
                   latitudes if your desired point is in between grid points
    
    longitude:     is the desired longitude you want to interpolate on.  The function will pull out both floor and ceiling
                   longitudes if your desired point is in between grid points
                 
  
  **Output:**
  
    link: is the erddap link to your desired data.  Pasting this link into a web browser would start a download for it.
  
  """
  import math
  import numpy as np
  #determine lat and lon corners for interpolation
  #Latitude floor/ceiling
  latitude5 = latitude +.5
  lat_ceil  = math.ceil(latitude5)-.5
  lat_floor = math.floor(latitude5)-.5
  #Longitude floor/ceiling
  longitude5 = longitude +.5
  lon_ceil   = math.ceil(longitude5)-.5
  lon_floor  = math.floor(longitude5)-.5
  strlonC = str(lon_ceil)
  strlonF = str(lon_floor)
  strlatC = str(lat_ceil)
  strlatF = str(lat_floor)
  link = 'http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnWPr.nc?pmsl[(' + start_date + '):1:(' + end_date +\
')][(' + strlatF + '):1:(' + strlatC + ')][(' + strlonF + '):1:(' + strlonC + ')]'
  return link
    

def points_creator(lat,lon,pmsl):
  import numpy as np
  lat_index = np.arange(-89.5,90.5,1)
  lon_index = np.arange(0.5,360.5,1)
  round_up_lat   = round(lat) + .5
  round_down_lat = round(lat) - .5
  round_up_lon   = round(lon) + .5
  round_down_lon = round(lon) - .5
  #round_up_lat   = np.ceil(lat)
  #round_down_lat = np.floor(lat)
  #round_up_lon   = np.ceil(lon)
  #round_down_lon = np.floor(lon)
  points =  [(round_up_lat, round_up_lon, pmsl[np.where(round_up_lat==lat_index)[0],np.where(round_up_lon==lon_index)[0]][0]),
             (round_down_lat, round_up_lon, pmsl[np.where(round_down_lat==lat_index)[0],np.where(round_up_lon==lon_index)[0]][0]),
             (round_up_lat, round_down_lon, pmsl[np.where(round_up_lat==lat_index)[0],np.where(round_down_lon==lon_index)[0]][0]),
             (round_down_lat, round_down_lon, pmsl[np.where(round_down_lat==lat_index)[0],np.where(round_down_lon==lon_index)[0]][0])]
#  points =  [(round_up_lat, round_up_lon, pmsl[np.where(round_up_lat+.5==lat_index)[0],np.where(round_up_lon+.5==lon_index)[0]][0]),
#             (round_up_lat, round_down_lon, pmsl[np.where(round_down_lat+.5==lat_index)[0],np.where(round_up_lon+.5==lon_index)[0]][0]),
#             (round_down_lat, round_up_lon, pmsl[np.where(round_up_lat+.5==lat_index)[0],np.where(round_down_lon+.5==lon_index)[0]][0]),
#             (round_down_lat, round_down_lon, pmsl[np.where(round_down_lat+.5==lat_index)[0],np.where(round_down_lon+.5==lon_index)[0]][0])]
  return points


def interpolate_grid(pmsl,lat,lon):
  import numpy as np
  grid = []
  for i in range(len(lat[:,0])):
    grow = []
    for j in range(len(lon[0,:])):
      lat_i = lat[i,j]
      lon_i = lon[i,j]
      points = points_creator(lat_i,lon_i,pmsl)
      pmsl_i = bilinear_interpolation(lat_i, lon_i, points)
      grow.append(pmsl_i)
    grid.append(grow)
  interpolated_grid = np.array(grid).astype('float64')
  return interpolated_grid
