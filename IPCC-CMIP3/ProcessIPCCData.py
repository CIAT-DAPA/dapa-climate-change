# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++
# Process MRI datasets
# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++

# This python script is to be run under Linux, with the proper software (cdo) available
# within any of the following folders:
#      -/bin/
#      -/usr/local/bin/
#      -/USERNAME/bin/
# cdo is required to separate years and then months
# GDAL is required to the final transformation from NETCDF to Arc/Info ESRI ASCII (can be omitted)
#

# Import system modules
import sys, string, os, glob

# Clearing screen and getting the arguments
os.system("clear")

#model = sys.argv[1]
scenario = sys.argv[1] #20C3M, SRES_A1b, SRES_A2, SRES_B1

#initfile = "ipsl_cm4.sresa2.tas_A1_2000-2100.nc" #sys.argv[3]

# Defining base dir of the data

basedir = "//media//Data-1//climate_change//IPCC_CMIP3"
scenpath = basedir + "//" + scenario

# Listing the models

modlist = glob.glob(scenpath + "//ncar_ccsm3_*")
for model in modlist:
	
	# Path where everything is located
	
	modname = os.path.split(model)[1]
	
	print 'Processing ' + modname
	
	modpath = scenpath + "//" + modname
	yearly_path = modpath + "//yearly_files"
	multiyr_path = modpath + "//multiyr_avgs"
	
	if not os.path.exists(yearly_path):
		os.system("mkdir " + yearly_path)
	
	if not os.path.exists(multiyr_path):
		os.system("mkdir " + multiyr_path)
	
	# Listing NetCDF files to process
	
	filelist = glob.glob(modpath + "//*.nc")
	
	for fname in filelist:
		
		fnm = os.path.split(fname)[1]
		varname = string.split(fnm, ".")[0]
		period = string.split(fnm, ".")[1]
		
		if varname == "pr":
			nwvname = "prec"
		elif varname == "tas":
			nwvname = "tmean"
		elif varname == "tasmin":
			nwvname = "tmin"
		elif varname == "tasmax":
			nwvname = "tmax"
		
		print 'Processing variable ' + varname + "(" + nwvname + ")"
		
		# Splitting the initial file into yearly files and copying into the yearly_path folder (2014, 2024, 2084)
		
		os.system("cdo splityear " + modpath + "//" + fnm + " " + nwvname + "_")
		os.system("mv *.nc " + yearly_path + "//")
		
		# Splitting the initial file into monthly files and copying into the multiyr_path folder
		
		os.system("cdo splitmon " + modpath + "//" + fnm + " " + nwvname + "_")
		os.system("mv *.nc " + multiyr_path + "//")
		
		# Listing the months
		
		monthlist = glob.glob(multiyr_path + "//" + nwvname + "_*.nc")
		
		# Calculating the multiyear (30yr, or 20yr) averages
		
		for mth in monthlist:
			
			filename = os.path.split(mth)[1]
			vname = string.split(filename, "_")[0]
			month = string.split(filename, "_")[1][0:2]
			
			print 'Processing month ' + month
			
			if period == "2046-2065":
				os.system("cdo runavg,20 " + multiyr_path + "//" + filename + " " + multiyr_path + "//" + vname + "_myr_" + month + ".nc")
			else:
				os.system("cdo runavg,30 " + multiyr_path + "//" + filename + " " + multiyr_path + "//" + vname + "_myr_" + month + ".nc")
			os.system("rm " + multiyr_path + "//" + filename)
			
			# Now splitting into yearly to take the needed and/or available files
			
			os.system("cdo splityear " + multiyr_path + "//" + vname + "_myr_" + month + ".nc " + vname + "_myr_" + month + "_")
			os.system("mv *.nc " + multiyr_path + "//")
			
			if scenario == "20C3M":
				if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(1975) + ".nc"):
					if not os.path.exists(multiyr_path + "//1961_1990"):
						os.system("mkdir " + multiyr_path + "//1961_1990")
					os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(1975) + ".nc " + multiyr_path + "//1961_1990//" + vname + "_" + month + ".nc")
				elif os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(1976) + ".nc"):
					if not os.path.exists(multiyr_path + "//1961_1990"):
						os.system("mkdir " + multiyr_path + "//1961_1990")
					os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(1976) + ".nc " + multiyr_path + "//1961_1990//" + vname + "_" + month + ".nc")
			else:
				if period == "2046-2065":
					if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2055) + ".nc"):
						if not os.path.exists(multiyr_path + "//2040_2069"):
							os.system("mkdir " + multiyr_path + "//2040_2069")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2055) + ".nc " + multiyr_path + "//2040_2069//" + vname + "_" + month + ".nc")
					elif os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2056) + ".nc"):
						if not os.path.exists(multiyr_path + "//2040_2069"):
							os.system("mkdir " + multiyr_path + "//2040_2069")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2056) + ".nc " + multiyr_path + "//2040_2069//" + vname + "_" + month + ".nc")
						
				else:
					if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2024) + ".nc"):
						if not os.path.exists(multiyr_path + "//2010_2039"):
							os.system("mkdir " + multiyr_path + "//2010_2039")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2024) + ".nc " + multiyr_path + "//2010_2039//" + vname + "_" + month + ".nc")
					
					if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2034) + ".nc"):
						if not os.path.exists(multiyr_path + "//2020_2049"):
							os.system("mkdir " + multiyr_path + "//2020_2049")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2034) + ".nc " + multiyr_path + "//2020_2049//" + vname + "_" + month + ".nc")
					
					if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2044) + ".nc"):
						if not os.path.exists(multiyr_path + "//2030_2059"):
							os.system("mkdir " + multiyr_path + "//2030_2059")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2044) + ".nc " + multiyr_path + "//2030_2059//" + vname + "_" + month + ".nc")
					
					if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2054) + ".nc"):
						if not os.path.exists(multiyr_path + "//2040_2069"):
							os.system("mkdir " + multiyr_path + "//2040_2069")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2054) + ".nc " + multiyr_path + "//2040_2069//" + vname + "_" + month + ".nc")
					
					if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2064) + ".nc"):
						if not os.path.exists(multiyr_path + "//2050_2079"):
							os.system("mkdir " + multiyr_path + "//2050_2079")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2064) + ".nc " + multiyr_path + "//2050_2079//" + vname + "_" + month + ".nc")
					
					if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2074) + ".nc"):
						if not os.path.exists(multiyr_path + "//2060_2089"):
							os.system("mkdir " + multiyr_path + "//2060_2089")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2074) + ".nc " + multiyr_path + "//2060_2089//" + vname + "_" + month + ".nc")
					elif os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2075) + ".nc"):
						if not os.path.exists(multiyr_path + "//2060_2089"):
							os.system("mkdir " + multiyr_path + "//2060_2089")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2075) + ".nc " + multiyr_path + "//2060_2089//" + vname + "_" + month + ".nc")
					
					if os.path.exists(multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2084) + ".nc"):
						if not os.path.exists(multiyr_path + "//2070_2099"):
							os.system("mkdir " + multiyr_path + "//2070_2099")
						os.system("mv " + multiyr_path + "//" + vname + "_myr_" + month + "_" + str(2084) + ".nc " + multiyr_path + "//2070_2099//" + vname + "_" + month + ".nc")
			
		os.system("rm " + multiyr_path + "//*.nc")
		
		print 'Monthly calculation done'
		
		# Listing the yearly files to split them into months
		
		filelist = glob.glob(yearly_path + "//*.nc")
		
		print 'Processing the list of dates ' 
		
		for path in filelist:
			
			filename = os.path.split(path)[1]
			vname = string.split(filename, "_")[0] #nwvname + "_"
			year = string.split(filename, "_")[1][0:4]
			
			if not os.path.exists(yearly_path + "//" + year):
				os.system("mkdir " + yearly_path + "//" + year)
			
			print 'Processing year ' + year
			
			os.system("mv " + yearly_path + "//" + filename + " " + yearly_path + "//" + year + "//")
			
			print 'Splitting into monthly files'
			
			os.system("cdo splitmon " + yearly_path + "//" + year + "//" + filename + " " + vname + "_")
			os.system("mv *.nc " + yearly_path + "//" + year + "//")
			os.system("rm " + yearly_path + "//" + year + "//" + filename)
			
print 'Process done!'
