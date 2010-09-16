#-----------------------------------------------------------------------
# Description: Convert outputs PRECIS (.pp) to ESRI-Grids
# Author: Carlos Navarro
# Date: 13/09/10
#-----------------------------------------------------------------------

# Import system modules
import os, sys, glob, string

if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - ie: python ConvertPP2Grid.py //mnt//GeoData-717//PrecisData//archive genam 00001 pa //mnt//GeoData-719//PrecisData//PRECIS_Grids// "
	print "   - ie: python ConvertPP2Grid.py W:\PrecisData\archive genam 00001 pa W:\RCM_Data\ HadCM3Q0 SRES_A2"
	sys.exit(1)

# Notes:
# runid: The full list of runid are in trunk\dapa-climate-change\PRECIS\Progress_Runs_Precis.xls
# variable: Variable in the PRECIS model, corresponding to the STASH code. The full list are in
#           trunk\dapa-climate-change\PRECIS\Metereological_Variables_PRECIS.xlsx
# type: The time perior over which the data gas been processed and the amount of data in the file.
#       The possibilities are:
#           pa = Timeseries of daily data spanning 1 month
#           pm = Monthly average for 1 month
#           ps = 3-month seasonal average data for 1 season
#           py = Annual average data for 1 year
# dirout: The structure of outputs files are <RCM_Data\PRECIS_datatype\GCM\Scenario\Year\GCM_scenario_variable_YYYYMM.asc>


# Arguments
dirbase = sys.argv[1]
runid = sys.argv[2]
variable = sys.argv[3]
type = sys.argv[4]
dirout = sys.argv[5]
gcm = sys.argv[6]
scenario = sys.argv[7]

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                     PP PRECIS TO GRID                      "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

# Dictionary with single letter date stamp equivalences
decDc = {"e": 1940, "f": 1950, "g": 1960, "h": 1970, "i": 1980, "j": 1990, "k": 2000, "l": 2010,
         "m": 2020, "n": 2030, "o": 2040, "p": 2050, "q": 2060, "r": 2070, "s": 2080, "t": 2090}

# Dictionary month equivalences
monDc = {"jan": 1, "feb": 2, "mar": 3, "apr": 4, "may": 5, "jun": 6, 
         "jul": 7, "aug": 8, "sep": 9, "oct": 10, "nov": 11, "dec": 12}

# Get a list of pp files into base dir
ppList = glob.glob(dirbase + "\\" + runid + "\\" + str(variable) + "\\" + runid + "a." + type + "*" + variable + "*.pp")
for pp in ppList:
    print pp

    # Extact year of the file name
    decade = os.path.basename(pp)[9:10]
    year = os.path.basename(pp)[10:11]
    month = os.path.basename(pp)[11:14]
    date = int(decDc [decade]) + int(year)

    #Creating outputs folders to nc files by years
    diroutNC = dirout + "\\PRECIS_nc\\" + gcm + "\\" + scenario + "\\" + str(date)
    if not os.path.exists(diroutNC):
        os.system('mkdir ' + diroutNC)

    # Convert pp files to nc format and split into the daily files
    # Structure nc filenames: gcm_scenario_variable_YYYYMM.nc
    
    print "Converting pp files to nc format and split into the daily files"
    
    if month < 10:
        print "    ---> Processing " + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "0" + str(monDc [month])
        outnc = diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "0" + str(monDc [month]) + ".nc"
        os.system("convsh1.91 -i pp -o outnc")
        print "    ---> Converted"

        # Split into the daily files    
        spltnc = 
        os.system("cdo splitday " + outnc + " " + str(i) + "_sfc_" + gridtype + "_day_")
        os.system("mv *.nc " + baseout + "//" + period + "//" + dayofyear + "//")
        print "    ---> Splited"

    else:
        print "    ---> Processing " + gcm + "_" + scenario + "_" + variable + "_" + str(date) + str(monDc [month])
        outnc = diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + str(monDc [month]) + ".nc"
        os.system("convsh1.91 -i pp -o outnc")
        print "    ---> Converted"

        # Split into the daily files
        spltnc = 
        os.system("cdo splitday " + outnc + " " + str(i) + "_sfc_" + gridtype + "_day_")
        os.system("mv *.nc " + baseout + "//" + period + "//" + dayofyear + "//")
        print "    ---> Splited"

    print "    ---> Convert to nc and split done!!"

    # Convert NETCDF files to ASCII format
    # Structure nc filenames: gcm_scenario_variable_YYYYMM.asc

    #Creating outputs folders to asc by years
    diroutASC = dirout + "\\PRECIS_AAIGrid" + gcm + "\\" + scenario + "\\" + str(date)
    if not os.path.exists(diroutASC):
        os.system('mkdir ' + diroutASC) 
    
    print "Converting NETCDF files to AAIGrid format"

    if month < 10:
        print "    ---> Processing " + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "0" + str(monDc [month])
        outasc = 
        outnc = diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "0" + str(monDc [month]) + ".nc"
        os.system("gdal_translate -of AAIGRID -sds " outnc + " " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.asc")
        print "    ---> Converted"

    else:
        print "    ---> Processing " + gcm + "_" + scenario + "_" + variable + "_" + str(date) + str(monDc [month])
        outasc = 
        outnc = diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + str(monDc [month]) + ".nc"
        os.system("gdal_translate -of AAIGRID -sds " + outnc + " " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.asc")
        print "     " + gcm + "_" + scenario + "_" + variable + "_" + str(date) + str(monDc [month]) + " Converted"
        print "    ---> Converted"

	#		# Compressing NETCDF file
	#		
	#		os.system("gzip -f " + baseout + "//" + period + "//" + dayofyear + "//" + str(i) + "_sfc_" + gridtype + "_day.nc")
	#
	print '    .Day done!'
    
estructura outputs = W:\RCM_Data\HadCM3Q0\SRES_A2\1949\HadCM3Q0_SRES_A2_00001_194912.asc
##    decada = out[3:4]
##    #Convert pp to ASCII
##    os.system("convsh1.91 -i pp -o .nc")

##estructura = genama.pa f9dec .00001.pp

