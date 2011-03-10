#------------------------------------------------------------------------------------------------------------------------------------------------------
# Description: Describe daily, monthly, anual and 30yr mean PRECIS outputs data
# Author: 	   Carlos Navarro
# Date:  	   10/03/11
# -----------------------------------------------------------------------------------------------------------------------------------------------------

# Import system modules
import arcgisscripting, os, sys, string

if len(sys.argv) < 6:
	os.system('clear')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python Describe_Grids.py D:\climate_change\RCM_Data\SRES_A1B\HadCM3Q3 1959 2099 daily D:\climate_change\RCM_Data\SRES_A1B\_describes"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
type = sys.argv[4]
dirout = sys.argv[5]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

os.system('cls')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                         DESCRIBE GRIDS PRECIS                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

gp = arcgisscripting.create(9.3)

scenario =  str(dirbase).split("\\")[3]
model = dirbase.split("\\")[-1]

txtDescribe = dirout + "\\" + str(scenario) + "_" + str(model) + "_" + str(type) + ".txt"
if os.path.isfile(txtDescribe):
	outFile = open(txtDescribe, "a")
else:
	outFile = open(txtDescribe, "w")
outFile.write("SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")

if type == "daily":

    for year in range(inityear, finalyear + 1, 1):

        if os.path.exists(dirbase + "\\" + type + "_grids\\" + str(year)):
        
            variablelist = sorted(os.listdir(dirbase + "\\" + type + "_grids\\" + str(year)))
            
            for variable in variablelist:
            
                # Set workspace
                gp.workspace = dirbase + "\\" + type + "_grids\\" + str(year) + "\\" + variable
                print "  > Processing " + gp.workspace
                
                rasters = sorted(gp.ListRasters("", "GRID"))
                for raster in rasters:
                    print "  " + str(model) + "\t" + variable + "\t" + str(year) + "\t" + raster + "\n"
                    MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
                    MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
                    MEA = gp.GetRasterProperties_management(raster, "MEAN")
                    STD = gp.GetRasterProperties_management(raster, "STD")
                    CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")

                    #Write txt describe file
                    outFile = open(txtDescribe, "a")
                    outFile.write(scenario + "\t" + str(model) + "\t" + str(year) + "\t" + str(raster) + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

print "  Process done!!!"