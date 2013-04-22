#----------------------------------------------------------------------------------------
# Description: Extract points values form raster in a workspace (i.e. WorldClim dataset)
# Author: Carlos Navarro
# Date: March 18th, 2011
#----------------------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python ExtractValues.py <dirbase> <diroutfile> <points> <wildcard>"
	print "   - ex: python ExtractValues.py \\dapadfs\data_cluster_4\observed\gridded_products\worldclim\Global_30s D:\Workspace D:\Workspace\mask\points.shp ALL"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
wildcard = sys.argv[4]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Create output folder
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

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
	
	# Define outputs
	OutPointsFC = dirout + "\\" + os.path.basename(raster) + ".dbf"

	if not gp.Exists(OutPointsFC):
	
		# Sample function
		gp.Sample_sa(raster, mask, OutPointsFC, "")
		print "\t", os.path.basename(OutPointsFC), "extracted"

# Join dbfs files extracted
print "\n .. Joining outputs"
		
dbfList = sorted(glob.glob(dirout + "\\*.dbf"))
for dbf in dbfList:
	InData = dirout + "\\" + "bio_1.dbf"
	if not os.path.basename(dbf)[-9:] == "bio_1.dbf":
		fielraster = os.path.basename(dbf)[:-4].split("_")[-2:]
		gp.joinfield (InData, "mask", dbf, "mask", fielraster[0] + "_" + fielraster[1])

os.rename(dirout + "\\" + "bio_1.dbf", dirout + "\\extracts.dbf")

# Delete trash
xmlList = sorted(glob.glob(dirout + "\\*.xml"))
for xml in xmlList:
	os.remove(xml)
		
print "Process done!!"
