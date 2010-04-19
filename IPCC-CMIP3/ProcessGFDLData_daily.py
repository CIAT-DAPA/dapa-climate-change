# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++
# Process GFDL datasets
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

model = sys.argv[1] #gfdl_cm2_0, gfdl_cm2_1
scenario = sys.argv[2] #20CM3, A2, A1B

initfile = "tasmax_A2.19000101-20001231.nc" #sys.argv[3]
variable = initfile[0:6]

if scenario == "20CM3":
	yearinit = 1900
	yearend = 2000
else:
	yearinit = 2001
	yearend = 2100

# Defining base dir of the data

basedir = "//media//Data-1//climate_change//IPCC_CMIP3_data"

# Path where the set of folders (daily folders) are

path_model_scen = basedir + "//" + model + "//" + scenario
yearly_path = path_model_scen + "//yearly_files"
multiyr_path = path_model_scen + "//multiyr_avgs"

if not os.path.exists(yearly_path):
	os.system("mkdir " + yearly_path)

if not os.path.exists(multiyr_path):
	os.system("mkdir " + multiyr_path)

# Calculating monthly averages for selected files

os.system("cdo monmean " + path_model_scen + "//" + initfile + " " + path_model_scen + "//" + variable + "_monthly.nc")

# Splitting the summarized file into yearly files and copying into the yearly_path folder (2014, 2024, 2084)

os.system("cdo splityear " + path_model_scen + "//" + variable + "_monthly.nc" + " " + variable + "_")
os.system("mv *.nc " + yearly_path + "//")

# Splitting the initial file into monthly files and copying into the multiyr_path folder

os.system("cdo splitmon " + path_model_scen + "//" + variable + "_monthly.nc" + " " + variable + "_")
os.system("mv *.nc " + multiyr_path + "//")

# Listing the months

monthlist = glob.glob(multiyr_path + "//*.nc")

# Calculating the multiyear (30yr) averages

for mth in monthlist:
	
	filename = os.path.split(mth)[1]
	varname = string.split(filename, "_")[0]
	month = string.split(filename, "_")[1][0:2]
	
	print 'Processing month ' + month
	
	os.system("cdo runavg,30 " + multiyr_path + "//" + filename + " " + multiyr_path + "//" + varname + "_myr_" + month + ".nc")
	os.system("rm " + multiyr_path + "//" + filename)
	
	# Now splitting into yearly to take the three needed files
	
	os.system("cdo splityear " + multiyr_path + "//" + varname + "_myr_" + month + ".nc " + varname + "_myr_" + month + "_")
	os.system("mv *.nc " + multiyr_path + "//")
	
	if scenario == "20CM3":
		if not os.path.exists(multiyr_path + "//1960_1999"):
			os.system("mkdir " + multiyr_path + "//1960_1999")
		
		os.system("mv " + multiyr_path + "//" + varname + "_myr_" + month + "_" + str(1974) + ".nc " + multiyr_path + "//1960_1999//" + filename)
	else:
		if not os.path.exists(multiyr_path + "//2010_2039"):
			os.system("mkdir " + multiyr_path + "//2010_2039")
		
		if not os.path.exists(multiyr_path + "//2040_2069"):
			os.system("mkdir " + multiyr_path + "//2040_2069")
		
		if not os.path.exists(multiyr_path + "//2070_2099"):
			os.system("mkdir " + multiyr_path + "//2070_2099")
	
		os.system("mv " + multiyr_path + "//" + varname + "_myr_" + month + "_" + str(2024) + ".nc " + multiyr_path + "//2010_2039//" + filename)
		os.system("mv " + multiyr_path + "//" + varname + "_myr_" + month + "_" + str(2054) + ".nc " + multiyr_path + "//2040_2069//" + filename)
		os.system("mv " + multiyr_path + "//" + varname + "_myr_" + month + "_" + str(2084) + ".nc " + multiyr_path + "//2070_2099//" + filename)
	
	os.system("rm " + multiyr_path + "//*" + month +"*.nc")

print 'Monthly calculation done'

# Listing these files

filelist = glob.glob(yearly_path + "//*.nc")

print 'Processing the list of dates ' 
#print datelist

for path in filelist:
	
	filename = os.path.split(path)[1]
	varname = string.split(filename, "_")[0]
	year = string.split(filename, "_")[1][0:4]
	
	if not os.path.exists(yearly_path + "//" + year):
		os.system("mkdir " + yearly_path + "//" + year)
	
	print 'Processing year ' + year
	
	os.system("mv " + yearly_path + "//" + filename + " " + yearly_path + "//" + year + "//")
	
	print 'Splitting into monthly files'
	
	os.system("cdo splitmon " + yearly_path + "//" + year + "//" + filename + " " + varname + "_")
	os.system("mv *.nc " + yearly_path + "//" + year + "//")
	os.system("rm " + yearly_path + "//" + year + "//" + filename)
	
print 'Process done!'
