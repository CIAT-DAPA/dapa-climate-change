#-------------------------------------------------------------------------
# Description: Calculate Zonal Statistics as Table of grids in a workspace
# Author: Carlos Navarro
# Date: September 07th, 2011
#-------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python ZonalStatisticsAsTable.py <dirbase> <dirout> <mask> <wildcard>"
	print "   - ie: python ZonalStatisticsAsTable.py D:\CIAT\Projects\col-cormacarena\01-datos-clima\anomalias\ideam D:\CIAT\Projects\col-cormacarena\01-datos-clima\_masks\llanos_adm2.shp D:\CIAT\Projects\col-cormacarena\01-datos-clima\evaluaciones\03-statistics-departments"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]


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
rcplist = "rcp26", "rcp45", "rcp85"
varlist = "prec_ann_per", "tmax_ann", "tmin_ann"

for rcp in rcplist:
	
	for var in varlist:
	
		raster = dirbase + "\\" + rcp + "\\" + var + ".tif"
		outTable = dirout + "\\" + rcp + "_" + os.path.basename(raster)[:-4] + ".dbf"
		
		if not gp.Exists(outTable):
			
			print raster
			
			# Zonal Statistical as table function
			gp.ZonalStatisticsAsTable_sa(mask, "FID", raster, outTable, "DATA")


# Join dbfs files extracted
print "\n .. Joining outputs"
		
dbfList = sorted(glob.glob(dirout + "\\" + "*.dbf"))
for dbf in dbfList:
	print dbf
	InData = dbfList[0]
	if not dbf == dbfList[0]:
		fielraster = os.path.basename(dbf)[:-4].split("_")[-2:]
		gp.joinfield (InData, "FID_", dbf, "FID_", "MEAN")
		os.remove(dbf)

os.rename(InData, dirout + "\\dpto_statistics.dbf")

xmlList = sorted(glob.glob(dirout + "\\*.xml"))
for xml in xmlList:
	os.remove(xml)
		
print "Process done!!"

	