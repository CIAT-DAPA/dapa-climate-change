# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++
# Process MRI datasets
# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++

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
import sys, string, os

# Clearing screen and getting the arguments
os.system("clear")
period = sys.argv[1]

# Defining base dir of the data

basedir = "//media//Data-1//MRI_data"

# Path where the set of folders (daily folders) are

path_period = basedir + "//" + period

# Listing these folders

datelist = os.listdir(path_period)
datelist = [dayofyear for dayofyear in datelist if dayofyear != ".directory"]

# List of months and of variable type (max, min, avg)

mthlist = "JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
typelist = "avr","max","min"

print 'Processing the list of dates ' 
print datelist

for dayofyear in datelist:
	
	dayofyear_mtrx = string.split(dayofyear, "_")
	date = dayofyear_mtrx[1]
	
	year = date[0:4]
	month = date[4:6]
	day = date[6:8]
	
	mthval = int(month)
	dayval = int(day)
	yerval = int(year)
	
	monthstr = mthlist[mthval-1]
	
	print 'Processing day ' + str(dayval) + ' of month ' + monthstr + ' of ' + str(yerval)
	
	comdate = day + monthstr + year
	
	for gridtype in typelist:
		
		# Creating the control file
		
		print '    .Fixing the control file for ' + gridtype
		
		os.system("awk -f " + basedir + "//scripts//Mk_ctl_File.awk " + basedir + "//ctl_files//sfc_" + gridtype + "_day.ctl " + day + monthstr + year + " " + gridtype + " > " + basedir + "//" + period + "//" + dayofyear + "//sfc_" + gridtype + "_day.ctl") 
		
		print '    .Done!'
		
		if gridtype == "avr":
			nvars = 2
		elif gridtype == "max":
			nvars = 3
		elif gridtype == "min":
			nvars = 1
		
		print '    .Converting from GrADS to NETCDF'
		
		for i in range(0, nvars):
			
			# Creating the NETCDF files from GrADS files
			
			os.system("grads2nc.tcl " + basedir + "//" + period + "//" + dayofyear + "//sfc_" + gridtype + "_day.ctl " + str(i) + " " + basedir + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc")
			
			print '    .Conversion done!'
		
		
		# Now compressing the input files to save space
		
		print '    .Compressing GrADS files'
		
		os.system("gzip -f " + basedir + "//" + period + "//" + dayofyear + "//sfc_" + gridtype + "_day.dr " + basedir + "//" + period + "//" + dayofyear + "//sfc_" + gridtype + "_day.ctl") 
		
		print '    .Compression done!'
	
	# - 2_sfc_max_day.nc: Total precipitation flux (kg/m2/s)
	# - 0_sfc_avr_day.nc: Total precipitation flux (km/m2/s)
	
	# Now calculating precipitation in mm/day rather than in kg/m2/s for 2_sfc_max_day.nc and 0_sfc_avr_day.nc
	
	print '    .Calculating pp in mm/day'
	
	os.system("cdo -f nc -mulc,86400 " + basedir + "//" + period + "//" + dayofyear + "//0_sfc_avr_day.nc " + basedir + "//" + period + "//" + dayofyear + "//0_test.nc")
	os.system("cdo -f nc -mulc,86400 " + basedir + "//" + period + "//" + dayofyear + "//2_sfc_max_day.nc " + basedir + "//" + period + "//" + dayofyear + "//2_test.nc")
	
	os.system("rm " + basedir + "//" + period + "//" + dayofyear + "//0_sfc_avr_day.nc")
	os.system("rm " + basedir + "//" + period + "//" + dayofyear + "//2_sfc_max_day.nc")
	
	os.system("mv " + basedir + "//" + period + "//" + dayofyear + "//0_test.nc " + basedir + "//" + period + "//" + dayofyear + "//0_sfc_avr_day.nc")
	os.system("mv " + basedir + "//" + period + "//" + dayofyear + "//2_test.nc " + basedir + "//" + period + "//" + dayofyear + "//2_sfc_max_day.nc")
	
	print '    .Calculation done!'
	
	# Now producing final output in ASCII format
	
	print '    .Converting from NETCDF to Arc/Info ASCII Grids'
	
	for gridtype in typelist:
		
		if gridtype == "avr":
			nvars = 2
		elif gridtype == "max":
			nvars = 3
		elif gridtype == "min":
			nvars = 1
		
		for i in range(0, nvars):
			
			# GDAL command
			
			os.system("gdal_translate -of AAIGRID " + basedir + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc " + basedir + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.asc")
			
			# Compressing NETCDF file
			
			os.system("gzip -f " + basedir + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc")
	
	print '    .Day done!'

print 'Process done!'
