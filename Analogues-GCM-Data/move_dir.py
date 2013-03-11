# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob, shutil
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python move_dir.py D:\Analogues_GCM_data\ExtractWorld_tiles D:\Analogues_GCM_data\ExtractWorld_tiles_tiff"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear window
os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

# Get a list of grids in the workspace of each folder
print "\t ..listing asciis into " + dirbase
varlist = "bio", "dtr", "prec", "tmean"
modellist = sorted(glob.glob(dirbase + "\\SRES_B1\\Global_2_5min\\*"))
for model in modellist[0:4]:
	print model

	for var in varlist:
		indir = model + "\\2020_2049\\" + var + "_tiff"
		outdir = dirout + "\\SRES_B1\\Global_2_5min\\" + os.path.basename(model) + "\\2020_2049\\" + var + "_tiffs"
		print " .. moving " + indir
		if not os.path.exists(outdir):
			shutil.move(indir, outdir)

	os.system("7za a -mmt2 " + model + ".zip " + model)
	os.system("rmdir /s /q " + model)
	
	os.system("7za a -mmt2 " + dirout + "\\SRES_B1\\Global_2_5min\\" + os.path.basename(model) + ".zip " + dirout + "\\SRES_B1\\Global_2_5min\\" + os.path.basename(model))
	os.system("rmdir /s /q " + dirout + "\\SRES_B1\\Global_2_5min\\" + os.path.basename(model))
		
		
print "\t ..done!!"