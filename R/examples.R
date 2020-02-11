#You will need these librarys to use the wiDB functions
library(httr)
library(jsonlite)

#Source the functions
source("wiDB_functions.R")

#Find all sites with tap water data since September, 2019
sites = wiDB_sites(minDate = "2019-09-01", types = "Tap")

#Download data for US precipitation in the 1990s
vals = wiDB_data(minDate = "1990-01-01", maxDate = "2000-01-01", countries = "US", types = "Precipitation")

#Download data for US Rivers and streams, requesting a subset of data fields
vals = wiDB_data(minDate = "1980-01-01", maxDate = "2000-01-01", countries = "US", types = "River_or_stream", fields = "Site_Name,Latitude,Longitude,d2H")
