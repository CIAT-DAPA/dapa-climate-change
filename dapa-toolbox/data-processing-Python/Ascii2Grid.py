# ---------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Ascii2Grid.py <dirbase> <dirout> <wildcard> <type> <remove>"
	print "   - ex: python Ascii2Grid.py D:\Workspace D:\Workspace prec INTEGER NO"
	print " dirbase	: Is the folder where your ASCII files are located"
	print " dirout	: Is the folder where the ESRI-grids files are created"
	print "	wildcard : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace. "
	print "	type : The data type of the output raster dataset. INTEGER: An integer raster dataset will be created. FLOAT: A floating-point raster dataset will be created"
	print "	remove : Remove ESRI-Ascii files of base directory after converted "
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
wildcard = sys.argv[3]
type = sys.argv[4]
remove = sys.argv[5]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear screen
os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

# Create output folder
if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)

# Get a list of grids in the workspace
print "\t..listing asciis into " + dirbase
if wildcard == "ALL":
	asclist = sorted(glob.glob(dirbase + "\\*.asc"))
else:	
	asclist = sorted(glob.glob(dirbase + "\\" + wildcard + "*.asc"))


# Lopping around the asciis
for asc in asclist:
	if not gp.Exists(os.path.basename(asc)[:-4]):
		#Convert function
		gp.ASCIIToRaster_conversion(asc, dirout + "\\" + os.path.basename(asc)[:-4], type)
		gp.AddMessage( "\t" + os.path.basename(asc) + " Converted"  )

		#Remove asciis
		if remove == "YES":
			os.remove(asc)
			os.remove(asc[:-4]+'.prj')
	
gp.AddMessage("\n \t ====> DONE!! <====")