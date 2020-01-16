Protocol for Spatial_WI Database API

Open to Public

Base URL: http://wateriso.utah.edu/api/v1

***********************************************************************************************************
[GET]	/sites.php?option1=sth&option2=sth&option3=sth&return=sth

default response:
{
  "sites":  [
               {
	               "Site_ID"  : STRING,
	               "Latitude" : DOUBLE,
	               "Longitude": DOUBLE
               },
               ...
            ],

  "latlong": {
                "minLat": DOUBLE,
                "maxLat": DOUBLE,
                "minLong": DOUBLE,
                "maxLong": DOUBLE
              },
  "elevation":  {
                  "minElev": DOUBLE,
                  "maxElev": DOUBLE
                },
  "countries": [STRING, STRING,…],

  "states" : [STRING, STRING,…],

  "types" : [ STRING, STRING, …],

  "dates" : {
               "minDate" : STRING,  (format: "yyyy-MM-dd" )
               "maxDate" : STRING   (format: "yyyy-MM-dd" )
            },

  "projects": [
                {
	                "Project_ID":    STRING,
	                "Project_Name":  STRING
                },
	              ...
             ]
}

If there is an error, response will be as follows:
{
  "status": {
              "Message" : STRING
            }
}

#######################################################################################################################
# Input options are listed as below:                                                                                  #
# Usage: minlat=-10&maxlat=50.6&countries=US,CA                                                                       #
#                                                                                                                     #
# name            Description                               value                                                     #
# --------------------------------------------------------------------------------------------------------------------#
# minLat          a min value of Latitude                   DOUBLE  -90 to 90, South is negative                      #
# maxLat          a max value of Latitude                   DOUBLE  -90 to 90, South is negative                      #
# minLong         a min value of Longitude                  DOUBLE  -180 to 180, West is negative                     #
# maxLong         a max value of Longitude                  DOUBLE  -180 to 180, West is negative                     #
# minElev         a min value of Elevation                  DOUBLE  if elevation is not provided, value=99999         #
# maxElev         a max value of Elevation                  DOUBLE  if elevation is not provided, value=-99999        #
# minDate         a min value of Collection Date            STRING (format: yyyy-MM-dd e.g. minDate=2018-11-01)       #
# maxDate         a max value of Collection Date            STRING (format: yyyy-MM-dd e.g. maxDate=2018-12-31)       #
# countries       one or multiple countries                 STRING  comma separated (e.g. countries=US,KG,CA)         #
# states          one or multiple states                    STRING  comma separated (e.g. states=ID,ON,WA)            #
# types           one or multiple types of samples          STRING  comma separated (e.g. types=Lake,Precipitation)   #
# projects        one or multiple project ids               STRING  comma separated (e.g. projectIds=00113,00161)     #
#######################################################################################################################

#######################################################################################################################
# Return value                                                                                                        #
# Users can choose what they want to return, options are as below, all options are comma separated, please see usage  #
# sites   latlong   elevation   countries   states    types   dates   projects                                        #
# Usage: return=sites,latlong,types                                                                                   #
# Default return will includes all of the return options.                                                             #
#######################################################################################################################


***********************************************************************************************************************
[GET]	/download.php?option1=sth&option2=sth&option3=sth&return=sth
This api will return a .zip file containing a data file, a project file and header file.

If there is an error, response will be as follows:
{
  "status": {
              "Message" : STRING
            }
}

The data file (csv) contains the default information as follows. The return parameters are applicable for this file, except Project_ID is always returned.
The accepted format is comma separated, e.g. return=Site_Name,Latitude,Longitude
#######
Site_Name
Latitude
Longitude
Elevation
Sample_ID
Type
Start_Date
Start_Time_Zone
Collection_Date
Collection_Time_Zone
Phase
Depth_meters
Sample_Comments
d2H
d18O
d2H_Analytical_SD
d18O_Analytical_SD
WI_Analysis_Source
Project_ID
########

NOTE: if the Proprietary of a project is enabled, the API will NOT return values for d2H, d18O, d2H_Analytical_SD, or d18O_Analytical_SD. Instead, if a value is available for any of these fields, return "9999", and if no value is available, return "-9999".

The project file (csv) contains all project info that appears in the data file, including Project_ID, Contact_Name, Contact_Email, Citation, URL and Project_Name. The option and return parameters are not applicable for this file.

The header file provides a description of the fields included in the tables of data file. The option and return parameters are not applicable for this file.

#######################################################################################################################
# Input options are listed as below:                                                                                  #
# Usage: minlat=-10&maxlat=50.6&countries=US,CA                                                                       #
#                                                                                                                     #
# name            Description                               value                                                     #
# --------------------------------------------------------------------------------------------------------------------#
# minLat          a min value of Latitude                   DOUBLE  -90 to 90, South is negative                      #
# maxLat          a max value of Latitude                   DOUBLE  -90 to 90, South is negative                      #
# minLong         a min value of Longitude                  DOUBLE  -180 to 180, West is negative                     #
# maxLong         a max value of Longitude                  DOUBLE  -180 to 180, West is negative                     #
# minElev         a min value of Elevation                  DOUBLE  if elevation is not provided, value=99999         #
# maxElev         a max value of Elevation                  DOUBLE  if elevation is not provided, value=-99999        #
# minDate         a min value of Collection Date            STRING (format: yyyy-MM-dd e.g. minDate=2018-11-01)       #
# maxDate         a max value of Collection Date            STRING (format: yyyy-MM-dd e.g. maxDate=2018-12-31)       #
# countries       one or multiple countries                 STRING  comma separated (e.g. countries=US,KG,CA)         #
# states          one or multiple states                    STRING  comma separated (e.g. states=ID,ON,WA)            #
# types           one or multiple types of samples          STRING  comma separated (e.g. types=Lake,Precipitation)   #
# projects        one or multiple project ids               STRING  comma separated (e.g. projectIds=00113,00161)     #
#######################################################################################################################
