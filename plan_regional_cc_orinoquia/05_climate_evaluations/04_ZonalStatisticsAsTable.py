#-------------------------------------------------------------------------
# Description: Calculate Zonal Statistics as Table of grids in a workspace
# Author: Carlos Navarro
# Date: April 2016
#-------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python ZonalStatisticsAsTable.py <dirbase> <dirout> <mask> <wildcard>"
	print "   - ie: python ZonalStatisticsAsTable.py X:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\anomalias\hur\anomalies \\dapadfs\workspace_cluster_6\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\_masks\llanos_adm2.shp X:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\evaluaciones\02-anomalies\dptos_statistics"
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
# varlist = "prec", "tmax", "tmin"
var = "hur"

for rcp in rcplist:
	
	# for var in varlist:
	
	# for month in range(1, 12+1, 1):

	raster = dirbase + "\\" + rcp + "\\ensemble\\" + var + "_ann.tif"
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

	