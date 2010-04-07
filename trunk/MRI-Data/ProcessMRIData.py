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
import sys, string, os

# Clearing screen and getting the arguments
os.system("clear")
period = sys.argv[1] ##SP0A, SF0A, SN0A

# Defining base dir of the data

basedir = "//mnt//MRI_input"

# Path where the set of folders (daily folders) are

path_period = basedir + "//" + period

# IMPORTANT: Change the output paths

baseout = "//mnt//GIS-HD710//MRI_data//MRI_outputs"
path_output = baseout + "//" + period

if not os.path.exists(path_output):
	os.system("mkdir " + path_output)

# Listing these folders

datelist = os.listdir(path_period)
#datelist = [dayofyear for dayofyear in datelist if dayofyear != ".directory"] ##this part is to make it work on openSUSE

# List of months and of variable type (max, min, avg)

mthlist = "JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
ndaysmx = "31","28","31","30","31","30","31","31","30","31","30","31"
typelist = "avr","max","min"

print 'Processing the list of dates ' 
print datelist

for dayofyear in datelist:
	
	## Creating output folder
	
	if not os.path.exists(path_output + "//" + dayofyear):
		os.system("mkdir " + path_output + "//" + dayofyear)
	
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
	
	comdate = day + monthstr + year
	
	for gridtype in typelist:
		
		# Creating the control file
		
		print '    .Fixing the control file for ' + gridtype
		
		##Need to change the control (CTL) files folder
		
		os.system("awk -f //home//precis//MRI_tools//Mk_ctl_File.awk //home//precis//MRI_tools//control_files//sfc_" + gridtype + "_day.ctl " + day + monthstr + year + " " + gridtype + " " + ndaysmth + " > " + basedir + "//" + period + "//" + dayofyear + "//sfc_" + gridtype + "_day.ctl")
		
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
			
			os.system("grads2nc.tcl " + basedir + "//" + period + "//" + dayofyear + "//sfc_" + gridtype + "_day.ctl " + str(i) + " " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc")
			
			print '    .Conversion done!'
			
			print '    .Splitting into daily files'
			
			os.system("cdo splitday " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc" + " " + str(i) + "_sfc_" + gridtype + "_day_")
			 
			os.system("mv *.nc " + baseout + "//" + period + "//" + dayofyear + "//")
			
			print '    .Split done!'
			
			#Calculating ppt in mm/day
			
	for i in range(1, int(ndaysmth)+1):
		
		print '    .Calculating pp in mm/day' + str(i)
		
		os.system("cdo -f nc -mulc,86400 " + baseout + "//" + period + "//" + dayofyear + "//0_sfc_avr_day_" + str(i) + ".nc " + baseout + "//" + period + "//" + dayofyear + "//0_test.nc")
		
		os.system("cdo -f nc -mulc,86400 " + baseout + "//" + period + "//" + dayofyear + "//2_sfc_max_day_" + str(i) + ".nc " + baseout + "//" + period + "//" + dayofyear + "//2_test.nc")
		
		os.system("rm " + baseout + "//" + period + "//" + dayofyear + "//0_sfc_avr_day_" + str(i) + ".nc")
		os.system("rm " + baseout + "//" + period + "//" + dayofyear + "//2_sfc_max_day_" + str(i) + ".nc")
		
		os.system("mv " + baseout + "//" + period + "//" + dayofyear + "//0_test.nc " + baseout + "//" + period + "//" + dayofyear + "//0_sfc_avr_day_" + str(i) + ".nc")
		os.system("mv " + baseout + "//" + period + "//" + dayofyear + "//2_test.nc " + baseout + "//" + period + "//" + dayofyear + "//2_sfc_max_day_" + str(i) + ".nc")
		
		print '    .Calculation done!'
		
		# Now compressing the input files to save space
		## Can be changed for rm to save disk space or comment these four lines to save processing time
		
		#print '    .Removing GrADS files'
		
		#os.system("rm " + basedir + "//" + period + "//" + dayofyear + "//sfc_" + gridtype + "_day.dr")
		#os.system("rm " + basedir + "//" + period + "//" + dayofyear + "//sfc_" + gridtype + "_day.ctl") 
		
		#print '    .Deletion done!'
	
	# - 2_sfc_max_day.nc: Total precipitation flux (kg/m2/s)
	# - 0_sfc_avr_day.nc: Total precipitation flux (km/m2/s)
	
	# Now calculating precipitation in mm/day rather than in kg/m2/s for 2_sfc_max_day.nc and 0_sfc_avr_day.nc
	
	#print '    .Calculating pp in mm/day'
	
	#os.system("cdo -f nc -mulc,86400 " + baseout + "//" + period + "//" + dayofyear + "//0_sfc_avr_day.nc " + baseout + "//" + period + "//" + dayofyear + "//0_test.nc")
	#os.system("cdo -f nc -mulc,86400 " + baseout + "//" + period + "//" + dayofyear + "//2_sfc_max_day.nc " + baseout + "//" + period + "//" + dayofyear + "//2_test.nc")
	
	#os.system("rm " + baseout + "//" + period + "//" + dayofyear + "//0_sfc_avr_day.nc")
	#os.system("rm " + baseout + "//" + period + "//" + dayofyear + "//2_sfc_max_day.nc")
	
	#os.system("mv " + baseout + "//" + period + "//" + dayofyear + "//0_test.nc " + baseout + "//" + period + "//" + dayofyear + "//0_sfc_avr_day.nc")
	#os.system("mv " + baseout + "//" + period + "//" + dayofyear + "//2_test.nc " + baseout + "//" + period + "//" + dayofyear + "//2_sfc_max_day.nc")
	
	#print '    .Calculation done!'
	
	# Now producing final output in ASCII format
	#
	#print '    .Converting from NETCDF to Arc/Info ASCII Grids'
	#
	for gridtype in typelist:
		
		if gridtype == "avr":
			nvars = 2
		elif gridtype == "max":
			nvars = 3
		elif gridtype == "min":
			nvars = 1
		
		
		for i in range(0, nvars):
			os.system("rm " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc")
			
	#		# GDAL command
	#		
	#		os.system("gdal_translate -of AAIGRID -sds " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.asc")
	#		
	#		# Compressing NETCDF file
	#		
	#		os.system("gzip -f " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc")
	#
	print '    .Day done!'

print 'Process done!'
