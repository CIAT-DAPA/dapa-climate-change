#-------------------------------------------------------------------------
# Description: Calculate Zonal Statistics as Table of grids in a workspace
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: September 07th, 2011
#-------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python ZonalStatisticsAsTable.py <dirbase> <dirout> <mask> <wildcard>"
	print "   - ie: python ZonalStatisticsAsTable.py D:\Workspace\grids D:\Workspace\_zonal_calcs D:\Workspace\mask\shapefile.shp ALL"
	print " dirbase	: Is the folder where your ESRI-Grid files are located"
	print " dirout	: Is the folder where the zonal statistics tables are created"
	print " Mask : Polygon shapefile with full path and extension"
	print " dirout	: Is the folder where the zonal statistics tables are created"
	print "	wildcard : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace. "
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
wildcard = sys.argv[4]


#Create output folder
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

#Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "   ZONAL STATISTICS AS TABLE    "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

#Set workspace
gp.workspace = dirbase 

#Get a list of grids in the workspace
print "\t ..listing grids into " + dirbase
if wildcard == "ALL":
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))
	
#Lopping around the grids
for raster in rasters:
	
	#Set output
	outTable = dirout + "\\" + os.path.basename(raster) + ".dbf"
	
	#Zonal Statistical as table function
	if not os.path.exists(outTable):	
		gp.AddMessage(  "==>Processing: " + " " + os.path.basename(outTable)[:-4] )
		gp.ZonalStatisticsAsTable_sa(mask, "FID", raster, outTable, "DATA")
		gp.addfield (outTable, "VARIABLE", "text", "", "", "30")
		result = gp.GetCount_management(outTable)
		count = int(result.GetOutput(0))
		rows = gp.UpdateCursor(outTable, "FID_ =0")
		for row in rows:
			row.VARIABLE = os.path.basename(outTable)[:-4]
			rows.updateRow(row)
		del rows	
		
#Merge tables dbf		
dbfList = sorted(glob.glob(dirout + "\\*.dbf"))
for dbf in dbfList:
	outFile = open(dirout + '\\'+ 'list_merge.txt', 'a') 
	outFile.write(dbf + ';' )
	outFile.close()
	l=open(dirout + '\\'+ 'list_merge.txt')
	lines = [i for i in l.readlines()]
	gp.OverwriteOutput = 1
	gp.merge_management(lines[0], dirout + '\\ZonalStatisticsAsTable.dbf')

# Remove trash files
for dbf in dbfList:
	os.remove(dbf)
	os.remove(dbf+'.xml')
	
# Remove trash files	
l.close()
os.remove(dirout + '\\ZonalStatisticsAsTable.dbf.xml')	
os.remove(dirout + '\\list_merge.txt')	
		
gp.AddMessage("\n \t ====> DONE!! <====")  

		