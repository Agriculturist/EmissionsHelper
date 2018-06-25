# EmissionsHelper
An R package to assist with Emissions Modeling Work

This is a collection of support functions for processing wind models.  The primary purpose of this package is to simplify data wrangling portion associated with wind models.  Wind models do not require the complexity of R. To mitigate the complexity, this package seeks to use intuative self-commenting functions for processing data.


## Installing EmissionsHelper

Instructions for installing devtools can be found at https://www.r-project.org/nosvn/pandoc/devtools.html.

library(devtools)

install_github("agriculturist/EmissionsHelper")


## Using EmissionsHelper

library(EmissionsHelper)


## Functions

This package feautures a reformatTime function which infers formatting parameters from a character date.

reformatTime("1/31/2012T00:00:00")

reformatTime("2018-01-05 13:45:01")
