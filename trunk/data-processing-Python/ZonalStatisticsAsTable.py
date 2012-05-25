#--------------------------------------------------
# Description: Zonal Statistics
# Author: Carlos Navarro
# Actualizado: 07/09/11
#--------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "   ZONAL STATISTICS AS TABLE    "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python ZonalStatisticsAsTable.py <dirbase> <mask> <outdir>"
	print "   - ie: python ZonalStatisticsAsTable.py D:\Workspace\MRI_PRECIS\future D:\Workspace\MRI_PRECIS\Shapefiles\dissolve.shp D:\Workspace\MRI_PRECIS\charts"
	sys.exit(1)

dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)


gp.CheckOutExtension("Spatial")

modellist = sorted(os.listdir(dirbase))

for model in modellist:
	
	periodlist = "2020_2049", "2040_2069"

	for period in periodlist:
		print dirbase + "\\" + str(model) + "\\" + str(period) + "\n"
		gp.workspace = dirbase + "\\" + str(model) + "\\" + str(period)
		diroutzonal = dirout + "\\" + str(model) + "\\" + str(period)
		if not os.path.exists(diroutzonal):
			os.system('mkdir ' + diroutzonal)
		ascList = glob.glob(dirbase + "\\" + str(model) + "\\" + str(period) + "\\tmean*.asc")
		for ds in ascList:
			print os.path.basename(ds)
						
			outTable = diroutzonal + "\\" + os.path.basename(ds)[:-4] + ".dbf"
			gp.ZonalStatisticsAsTable_sa(mask, "FID", ds, outTable, "DATA")
			
			gp.joinfield (diroutzonal + "\\tmean1_5_01.dbf", "FID_", outTable, "FID_", "MEAN")
			if not os.path.basename(ds) == "tmean1_5_01.asc":	
				gp.delete_management(outTable)