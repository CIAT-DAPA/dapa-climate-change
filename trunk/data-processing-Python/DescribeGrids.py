# ---------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: November 23th, 2011
# Purpouse: Describe grids into a workspace
# ---------------------------------------------------------------------------

import arcgisscripting, os, sys, string

gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python DescribeGrids.py <dirbase> <diroutfile> <wildcard>"
	print "   - ex: python DescribeGrids.py D:\Workspace D:\Workspace\describe.txt prec"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
diroutfile = sys.argv[2]

# Clear screen
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     DESCRIBE GRIDS      "
print "~~~~~~~~~~~~~~~~~~~~~~~~~\n"

# Create or open output txt file
if not os.path.isfile(diroutfile):
    outFile = open(diroutfile, "w")
	outFile.write("GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" 
					+ "\t" + "TOP" + "\t" + "LEFT" + "\t" + "RIGHT" + "\t" + "BOTTOM" + "\t" + "CELLSIZEX" + "\t" + "CELLSIZEY" + "\t" 
					+ "VALUETYPE" + "\t" + "COLUMNCOUNT" + "\t" + "ROWCOUNT" + "\t" + "BANDCOUNTUSER" + "\n")
	outFile.close()
	
# Set workspace
gp.workspace = dirbase 
	
# Get a list of grids in the workspace
print "\t ..listing grids into " + dirbase
if wildcard == "ALL":
	rasters = sorted(gp.ListRasters("*", "GRID"))
else:	
	rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))
	
# Lopping around the grids
for raster in sorted(rasters):
	
	# Parameters
	MIN = gp.GetRasterProperties_management(raster, "MINIMUM")
	MAX = gp.GetRasterProperties_management(raster, "MAXIMUM")
	MEA = gp.GetRasterProperties_management(raster, "MEAN")
	STD = gp.GetRasterProperties_management(raster, "STD")
	TOP = gp.GetRasterProperties_management(raster, "TOP")
	LEF = gp.GetRasterProperties_management(raster, "LEFT")
	RIG = gp.GetRasterProperties_management(raster, "RIGHT")
	BOT = gp.GetRasterProperties_management(raster, "BOTTOM")
	CEX = gp.GetRasterProperties_management(raster, "CELLSIZEX")
	CEY = gp.GetRasterProperties_management(raster, "CELLSIZEY")
	VAL = gp.GetRasterProperties_management(raster, "VALUETYPE")
	COL = gp.GetRasterProperties_management(raster, "COLUMNCOUNT")
	ROW = gp.GetRasterProperties_management(raster, "ROWCOUNT")
	BAN = gp.GetRasterProperties_management(raster, "BANDCOUNTUSER")
	
	# Writting grid characteristics
	outFile = open(diroutfile, "a")
	outFile.write(raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" 
					+ TOP.getoutput(0) + "\t" + LEF.getoutput(0) + "\t" + RIG.getoutput(0) + "\t" + BOT.getoutput(0) + "\t" + CEX.getoutput(0) + "\t" + CEY.getoutput(0)
					+ VAL.getoutput(0) + "\t" + COL.getoutput(0) + "\t" + ROW.getoutput(0) + "\t" + BAN.getoutput(0) + "\n")
	
	print "\t", raster, "described"
	outFile.close()

print "\n \t Process done!!"
