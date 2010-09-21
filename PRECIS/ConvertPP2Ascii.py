#-----------------------------------------------------------------------
# Description: Convert outputs PRECIS (.pp) to ASCII
# Author: Carlos Navarro
# Date: 13/09/10
#-----------------------------------------------------------------------

# Import system modules
import os, sys, glob, string

if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - ie linux: python ConvertPP2Ascii.py //mnt//GeoData-717//PrecisData//archive genam 00001 pa //data3//precis// HadCM3Q3 SRES_A1B"
	sys.exit(1)

# --------------------------------------------------------------------------------------------------------------------------
# Notes:
# dirbase:  Path pp files
# runid:    The full list of runids are in trunk\dapa-climate-change\PRECIS\Progress_Runs_Precis.xls
# variable: Variable in the PRECIS model, corresponding to the STASH code. The full list are in
#           trunk\dapa-climate-change\PRECIS\Metereological_Variables_PRECIS.xlsx
# type:     The time period over which the data gas been processed and the amount of data in the file.
#           The possibilities are:
#               pa = Timeseries of daily data spanning 1 month
#               pm = Monthly average for 1 month
#               ps = 3-month seasonal average data for 1 season
#               py = Annual average data for 1 year
# Model:    The full list of models are in trunk\dapa-climate-change\PRECIS\PRECIS_Scenarios.txt
# Scenario: The full list of scenarios are in trunk\dapa-climate-change\PRECIS\PRECIS_Scenarios.txt
# dirout:   The structure of outputs files are <RCM_Data\PRECIS_datatype\GCM\Scenario\Year\GCM_scenario_variable_YYYYMM.asc>
#-----------------------------------------------------------------------------------------------------------------------------

# Arguments

dirbase = sys.argv[1]
runid = sys.argv[2]
variable = sys.argv[3]
type = sys.argv[4]
dirout = sys.argv[5]
gcm = sys.argv[6]
scenario = sys.argv[7]

os.system('clear')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                          PP PRECIS TO ASCII                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
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

    # Extact year of the file name

    decade = os.path.basename(pp)[9:10]
    year = os.path.basename(pp)[10:11]
    month = os.path.basename(pp)[11:14]
    date = int(decDc [decade]) + int(year)

    #Creating outputs folders to nc files by years

    diroutNC = dirout + "\\PRECIS_NC\\" + gcm + "\\" + scenario + "\\" + str(date)
    if not os.path.exists(diroutNC):
        os.system('mkdir ' + diroutNC)

    if int(monDc [month]) < 10 and not os.path.exists(diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "0" + str(monDc [month]) + ".nc"):


        # Convert pp files to nc format and split into the daily files
        # Structure nc filenames: gcm_scenario_variable_YYYYMM.nc
   
        print "    > Converting pp files to nc format and split into the daily files\n"
        
        print "    ---> Processing " + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "0" + str(monDc [month])
        outnc = diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "0" + str(monDc [month])
        os.system("conv2nc.tlc " + pp + " 1 " + outnc + ".nc")
        print "    ---> Converted to NetCDF"

        # Split into the daily files

        os.system("cdo splitday " + outnc + ".nc " + outnc)
        os.remove(outnc + ".nc ")
        print "    ---> Splited\n"

        # Compress monthly NC files and remove
        
        os.system("gzip -f " + outnc + ".nc ")
        os.remove(outnc + ".nc ")
        
    if int(monDc [month]) > 9 and not os.path.exists(diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + str(monDc [month]) + ".nc"):

        # Convert pp files to nc format and split into the daily files
        # Structure nc filenames: gcm_scenario_variable_YYYYMM.nc
   
        print "    > Converting pp files to nc format and split into the daily files\n"
        
        print "    ---> Processing " + gcm + "_" + scenario + "_" + variable + "_" + str(date) + str(monDc [month])
        outnc = diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + str(monDc [month])
        os.system("conv2nc.tlc pp + " + " 1 " + outnc + ".nc")
        print "    ---> Converted to NetCDF"

        # Split into the daily files

        os.system("cdo splitday " + outnc + ".nc " + outnc)
        os.remove(outnc + ".nc ")
        print "    ---> Splited\n"

        # Compress monthly NC files and remove
        
        os.system("gzip -f " + outnc + ".nc ")
        os.remove(outnc + ".nc ")


    # Convert NETCDF files to ASCII format
    # Structure nc filenames: gcm_scenario_variable_YYYYMMDD.asc

    print "    > Converting nc files to ASCII format"
    
    #Creating outputs folders to asc by years
    
    diroutASC = dirout + "\\PRECIS_AAIGrid\\" + gcm + "\\" + scenario + "\\" +  str(date)
    if not os.path.exists(diroutASC):
        os.system('mkdir ' + diroutASC)

    ncList = glob.glob(diroutNC + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "0" + str(monDc [month] + "*.nc")    
    for nc in ncList:

        print "    ---> Processing " + gcm + "_" + scenario + "_" + variable + "_" + str(date) + "_" + str(monDc [month]
        outASC = diroutASC + "\\" + os.path.basename(nc)[:-4] + ".asc"
        os.system("gdal_translate -of AAIGRID -sds " + nc + " " + outASC)
        print "    ---> Converted to ASCII"

        # Remove daily NC files        
        os.remove(nc)


print "Done!!!"