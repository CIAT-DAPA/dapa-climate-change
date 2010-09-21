#------------------------------------------------------------------------------
# Description: Convert outputs PRECIS in Ascii format(.asc) to ESRI-Grid format
# Author: Carlos Navarro
# Date: 20/09/10
#-----------------------------------------------------------------------------

# Import system modules
import arcgisscripting, os, sys, glob, string
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - ie windows: python ConvertAscii2Grid.py K:\RCM_Data\PRECIS_AAIGrids\ 00001 K:\RCM_Data\PRECIS_Grids\ HadCM3Q3 SRES_A1B"
	sys.exit(1)

# --------------------------------------------------------------------------------------------------------------------------
# Notes:
# dirbase:  Path asc files
# variable: Variable in the PRECIS model, corresponding to the STASH code. The full list are in
# dirout:   The structure of outputs files are <RCM_Data\PRECIS_datatype\GCM\Scenario\Year\GCM_scenario_variable_YYYYMM.asc>
# Model:    The full list of models are in trunk\dapa-climate-change\PRECIS\PRECIS_Scenarios.txt
# Scenario: The full list of scenarios are in trunk\dapa-climate-change\PRECIS\PRECIS_Scenarios.txt
# Ini/Fin year: Init final year of processing 
#-----------------------------------------------------------------------------------------------------------------------------

# Arguments

dirbase = sys.argv[1]
variable = sys.argv[2]
dirout = sys.argv[3]
gcm = sys.argv[4]
scenario = sys.argv[5]
inityear = sys.argv[6]
finalyear = sys.argv[7]

os.system('clear')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                       ASCII PRECIS TO GRIDS                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"


for year in range(inityear, finalyear + 1, 1):
    
    # Set workspace
    gp.workspace = dirbase + "\\" + gcm + "\\" + scenario + "\\" + str(year)

    #Create output folders    
    diroutGRID = dirout + "\\" + gcm + "\\" + scenario + "\\" +  str(year)
    if not os.path.exists(diroutGRID):
        os.system('mkdir ' + diroutGRID)

    # Get a list of asc 
    ascList = glob.glob(gp.workspace + "\\" + gcm + "_" + scenario + "_" + variable + "_" + str(year) + "*.asc")
        
    for asc in ascList:
        
        print "    ---> Processing " + asc
        nameGRID = string.split(asc, "_")
        outGRID = diroutGRID + "\\" + variable + "_" + str(nameGRID[4:5])
        gp.ASCIIToRaster_conversion(asc, outGRID, "INTEGER")
        print "    ---> Converted to GRID"

        #Compressing and delete asc
        os.system('7za a ' + asc + ".gz " + asc)
        gp.delete_management(asc)

print "Done!!!"

ppList = glob.glob(dirbase + "\\" + runid + "\\" + str(variable) + "\\" + runid + "a." + type + "*" + variable + "*.pp")