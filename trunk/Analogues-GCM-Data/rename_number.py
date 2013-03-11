# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python rename_number.py C:\Analogues_GCM_data\ExtractWorld_tiles\SRES_B1\Global_2_5min"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear window
os.system('cls')

# Get a list of grids in the workspace of each folder
# varlist = "bio", "dtr", "prec", "tmean"
var = "bio"
period = "2020_2049"
modellist = sorted(glob.glob(dirbase + "\\*"))
sres = "b1"
for model in modellist[1:3]:
	print model
	# for var in varlist:		
	for month in range(19, 19 + 1, 1):
		for tile in range(0, 100, 1):
			ascmod = dirbase + "\\" + os.path.basename(model) + "\\" + period + "\\" + var + "_asciis\\" + sres + "_" + period + "_" + os.path.basename(model) + "_" + var + "_" + str(month) + "_" + str(tile) + "_1.asc"
			tifmod = dirbase + "\\" + os.path.basename(model) + "\\" + period + "\\" + var + "_tiffs\\" + sres + "_" + period + "_" + os.path.basename(model) + "_" + var + "_" + str(month) + "_" + str(tile) + "_1.tif"
			
			if int(tile) < 10:
				asc = dirbase + "\\" + os.path.basename(model) + "\\" + period + "\\" + var + "_asciis\\" + sres + "_" + period + "_" + os.path.basename(model) + "_" + var + "_" + str(month) + "_00" + str(tile) + "_1.asc"
				tif = dirbase + "\\" + os.path.basename(model) + "\\" + period + "\\" + var + "_tiffs\\" + sres + "_" + period + "_" + os.path.basename(model) + "_" + var + "_" + str(month) + "_0" + str(tile) + "_1.tif"
			elif int(tile) < 100:
				asc = dirbase + "\\" + os.path.basename(model) + "\\" + period + "\\" + var + "_asciis\\" + sres + "_" + period + "_" + os.path.basename(model) + "_" + var + "_" + str(month) + "_00" + str(tile) + "_1.asc"
				tif = dirbase + "\\" + os.path.basename(model) + "\\" + period + "\\" + var + "_tiffs\\" + sres + "_" + period + "_" + os.path.basename(model) + "_" + var + "_" + str(month) + "_0" + str(tile) + "_1.tif"
			# print asc
			if os.path.exists(asc):
				print asc
				print ascmod
				os.rename(asc, ascmod)
				os.rename(tif, tifmod)
		
print "\t ..done!!"