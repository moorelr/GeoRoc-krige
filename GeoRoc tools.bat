echo off

set /p proceed="Do you want to download & parse a new GeoRoc file? (enter y/n) : "
if %proceed%==y (rscript "Pull georoc data.r")

set /p proceed="Do you want to calculate new element/isotope summary plots? (enter y/n) : "
if %proceed%==y (rscript "Summary statistics.r")

set /p proceed="Do you want to generate a new map of sample locations? (enter y/n) : "
if %proceed%==y (rscript "Georoc_maps.r")

set /p proceed="Do you want to plot a new semivariogram and krige? (enter y/n) : "
if %proceed%==y (rscript "Geostatistics.r")

cmd /k 