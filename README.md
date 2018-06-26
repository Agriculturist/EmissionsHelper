# EmissionsHelper
An R package to assist with Emissions Modeling Work

This is a collection of support functions for processing wind models.  The primary purpose of this package is to simplify data preparation which may be required with developing advanced wind models.  Wind models can benefit from many of the features R, the goal of this package is to aid in developing wind models by making data preparation more intuative.


## Installing EmissionsHelper

Instructions for installing devtools can be found at https://www.r-project.org/nosvn/pandoc/devtools.html.

library(devtools)

install_github("agriculturist/EmissionsHelper")


## Using EmissionsHelper

library(EmissionsHelper)


## Functions

This library is intended to ease initial dataprocessing associated with emissions modeling work.

### renameAllDataframeColumns
*renameAllDataframeColumns* is a self commenting approach to rename dataframe columns if a header does not import properly.  

### timeCeiling and timeFloor
*timeCeiling* and *timeFloor* are useful for statistics over time intervals.

### reformatTime
*reformatTime* infers formatting parameters from a character date.

reformatTime("1/31/2012T00:00:00")

reformatTime("2018-01-05 13:45:01")

### calculateSonicTemperature
*calculateSonicTemperature* calculates the sonic temperature from windspeed.

### calculateCovariances3DAnemometer
*calculateCovariances3DAnemometer* calculates covariances from a 3D anemometer.

### calculateMeansOverTime
*calculateMeansOverTime* averages numbers over a certain time interval.


