# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++
# Process MRI datasets
# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++

# RUN THIS AS: python ProcessMRIData.py PERIOD

# This python script is to be run under Linux, with the proper software (convsh1.90, cdo, GDAL) available
# within any of the following folders:
#      -/bin/
#      -/usr/local/bin/
#      -/USERNAME/bin/
# convsh1.90 is needed for converting from GrADS to NETCDF
# cdo is required to transform precipitation flux units
# GDAL is required to the final transformation from NETCDF to Arc/Info ESRI ASCII
#
# The period must be specified as an argument. Any of these are available: 1979-2003, 2015-2039, 2075-2100
#
# Outputs are stored within the input folder in the following format VAR_sfc_TYPE_day.nc, options are:
#
# - 0_sfc_max_day.nc: Maximum daily surface air temperature at 2m (K)
# - 1_sfc_max_day.nc: Maximum daily wind speed at 10m (m/s)
# - 2_sfc_max_day.nc: Total precipitation flux (kg/m2/s)
# - 0_sfc_min_day.nc: Minimum daily surface air temperature at 2m (K)
# - 0_sfc_avr_day.nc: Total precipitation flux (km/m2/s)
# - 1_sfc_avr_day.nc: Average daily surface air temperature at 2m (K)

# Import system modules
import sys, string, os, gdal
os.system("cls")
period = sys.argv[1] ##SP0A, SF0A, SN0A


basedir = "X:\MRI_data\MRI_outputs1"
path_period = basedir + "\\" + period
baseout = "K:\MRIData\MRIAAIGrid"
path_output = baseout + "\\" + period
if not os.path.exists(path_output):
	os.system("mkdir " + path_output)

# Listing these folders
datelist = os.listdir(path_period)

# List of months and of variable type (max, min, avg)
mthlist = "JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
ndaysmx = "31","28","31","30","31","30","31","31","30","31","30","31"
typelist = "avr","max","min"

print 'Processing the list of dates ' 
print datelist

for dayofyear in datelist:
	
	## Creating output folder
	
	if not os.path.exists(path_output + "\\" + dayofyear):
		os.system("mkdir " + path_output + "\\" + dayofyear)
	
	##Name structure is OUT_YYYYMMDDHHHH
	##HHHH is always 0000
	
	dayofyear_mtrx = string.split(dayofyear, "_")
	date = dayofyear_mtrx[1]
	
	##Capturing the date as strings (text type variables)
	
	year = date[0:4]
	month = date[4:6]
	day = date[6:8]
	
	##Converting strings to values
	
	mthval = int(month)
	dayval = int(day)
	yerval = int(year)
	
	monthstr = mthlist[mthval-1]
	ndaysmth = ndaysmx[mthval-1]
	
	print 'Processing day ' + str(dayval) + ' of month ' + monthstr + ' of ' + str(yerval)
	
	for gridtype in typelist:
		
		if gridtype == "avr":
			nvars = 2
		elif gridtype == "max":
			nvars = 3
		elif gridtype == "min":
			nvars = 1
		
		
		for i in range(0, nvars):
			print "gdal_translate -of AAIGRID -sds " + basedir + "\\" + period + "\\" + dayofyear + "\\" + str(i) + "_sfc_" + gridtype + "_day_" + str(day) + ".nc " + baseout + "\\" + period + "\\" + dayofyear + "\\" + str(i) + "_sfc_" + gridtype + "_day_" + str(day) + ".asc"
			os.system("gdal_translate -of AAIGRID -sds " + basedir + "\\" + period + "\\" + dayofyear + "\\" + str(i) + "_sfc_" + gridtype + "_day_" + str(day) + ".nc " + baseout + "\\" + period + "\\" + dayofyear + "\\" + str(i) + "_sfc_" + gridtype + "_day_" + str(day) + ".asc")

	print '    .Day done!'

print 'Process done!'
