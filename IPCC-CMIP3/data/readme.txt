Julian Ramirez-Villegas
International Center for Tropical Agriculture, CIAT
Cali, Colombia
E-mail: j.r.villegas@cgiar.org, dawnpatrolmustaine@gmail.com
July 01, 2010


GENERAL DESCRIPTION
-----------------------------------------------------
Script Name: averageAndMapGCMFields.R
Language: R
Operating system: Windows, Linux
Version: >= 2.10.0
Licence: GPL >= 2.0
Requires packages: raster, rgdal, sp, maptools

This script was created for mapping of GCM anomalies and predicted values (actual values). The script will provide a function through which you will be able to graph any set of GCMs and their average, for any of 7 different timeslices, for three different SRES emission scenarios, and for the baseline climatic scenario.

SRES emission scenarios that are available are SRES-A1B, SRES-A2, and SRES-B1. For each of these emission scenarios, a set of timeslices are available as averages of 30 years centered in the decade of interest: 2020s (2010-2039), 2030s (2020-2049), 2040s (2030-2059), 2050s (2040-2069), 2060s (2050-2079), 2060s (2060-2089) and 2070s (2080-2099).

For the baseline (20C3M), or 20th century data, the only time slice provided is the average of the period between 1961-1990.


USAGE:
-----------------------------------------------------
To use this script you need to source it in the R command prompt (download the latest R version from http://www.r-project.org/, and install required packages with "install.packages(package-name)", if you dont have R installed in your system):

source("averageAndMapGCMFields.R")

Alternatively, if you're using the R GUI (R Graphical User Interface), you can go to "File" > "Source R code...", and browse the script within your file system. You will probably need to change your working directory first (use the 'setwd()' command, or go to "File" > "Change dir...".

This script needs to be provided with a list of GCMs in csv format. This file must be present within the same folder as the script itself, otherwise the script will fail finding the file. The file name is "gcm_chars.csv".

We provide three additional files with this script: "global_adm0.dbf", "global_adm0.shx", and "global_adm0.shp", all conforming a single shapefile (ESRI Shapefile), that can be used as input to the mapping function in the script.

After sourcing the script, a function will become available:

mapGCMFields()

With the following arguments:

mapGCMFields(gcmList, drive, procdir, scenario, type, period, xn=-180, xx=180, yn=-90, yx=90, wt=5, worldshapefile, temp=T, prec=T)

-gcmList: Numeric vector. Indicates the number of the GCMs to be displayed, eg. c(1,2,4,6,10,20). This list of GCMs will be automatically displayed after sourcing the script.
-drive: String. Is the drive letter where the data is stored (eg. W:/). The standard structure the script will follow is [drive]/climate_change/IPCC_CMIP3/
-procdir: String. Is the folder where you want to store your output charts (default is C:/ for windows versions and /home/username/ for linux distros). If you either dont provide anything or provide a path that does not exist, the function will use the defaults.
-scenario: String. can be 20C3M, SRES_A1B, SRES_A2, or SRES_B1, either in upper or lower case. Anything different will produce an error.
-type: String. Can be "anomalies", or "actual", either in upper or lower case. Anything different will produce an error.
-period: String. Can be 2010_2039, 2020_2049, 2030_2059, 2040_2069, 2050_2079, 2060_2089, 2070_2099. Anything different will produce an error.
-xn: Numeric. Minimum longitude of the bounding box (square area of the world) you want to graph. Should not be less than -180 degrees.
-xx: Numeric. Maximum longitude of the bounding box (square area of the world) you want to graph. Should not be greater than 180 degrees.
-yn: Numeric. Minimum latitude of the bounding box (square area of the world) you want to graph. Should not be less than -90 degrees.
-yx: Numeric. Maximum latitude of the bounding box (square area of the world) you want to graph. Should not be less than 90 degrees.
-wt: Numeric. Width in inches of the plot (multi-page PDF plots). Big numbers will cause the PDF to be very heavy, so in this script we have limited the width between 5 and 15 inches.
-worldshapefile: String. Path to the shapefile of administrative boundaries of the world, not used if none provided or if the one provided does not exist in your file system.
-temp: Logical (T/F). Will map temperature data if TRUE
-prec: Logical (T/F). Will map precipitation data if TRUE \n")


RESULT
-----------------------------------------------------

A PDF file with each page containing the average predicted temperature and rainfall for the selected timeslice, and the selected set of models (each page containing a model), and the last two pages containing the average temperature and precipitation among the set of GCMs, and their respective standard deviations.

Naming of the PDF file is done as follows:

"Figs_[xn]WE[xx]WE[yn]NS[yx]NS_[type]_[scenario]_[period].pdf"

Where the stuff in brackets are parameters of the function, explained in the USAGE section of this document.
