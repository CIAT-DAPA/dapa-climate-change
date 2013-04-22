#-------------------------------------------------------------------------
# Description: Calculate Zonal Statistics as Table of grids in a workspace
# Author: Carlos Navarro
# Date: September 07th, 2011
#-------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python ZonalStatisticsAsTable.py <dirbase> <dirout> <mask> <wildcard>"
	print "   - ie: python ZonalStatisticsAsTable.py D:\Workspace D:\Workspace\_zonal_calcs D:\Workspace\mask\shapefile.shp ALL"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]
wildcard = sys.argv[4]


# Create output folder
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "   ZONAL STATISTICS AS TABLE    "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

# Set workspace
gp.workspace = dirbase 

# Get a list of grids in the workspace
print "\t ..listing grids into " + dirbase
if wildcard == "ALL":
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))
	
# Lopping around the grids
for raster in rasters:
	
	# Set output
	outTable = dirout + "\\" + os.path.basename(raster)[:-4] + ".dbf"
	
	# Zonal Statistical as table function
	gp.ZonalStatisticsAsTable_sa(mask, "FID", ds, outTable, "DATA")
	
	gp.joinfield (diroutzonal + "\\tmean1_5_01.dbf", "FID_", outTable, "FID_", "MEAN")
	if not os.path.basename(ds) == "tmean1_5_01.asc":	
		gp.delete_management(outTable)

# Join dbfs files extracted
print "\n .. Joining outputs"
		
dbfList = sorted(glob.glob(dirout + "\\*.dbf"))
for dbf in dbfList:
	InData = dbf[0]
	if not dbf == dbf[0]:
		fielraster = os.path.basename(dbf)[:-4].split("_")[-2:]
		gp.joinfield (InData, "mask", dbf, "mask", fielraster[0] + "_" + fielraster[1])
		os.remove(dbf)

os.rename(InData, dirout + "\\extracts.dbf")

xmlList = sorted(glob.glob(dirout + "\\*.xml"))
for xml in xmlList:
	os.remove(xml)
		
print "Process done!!"

		