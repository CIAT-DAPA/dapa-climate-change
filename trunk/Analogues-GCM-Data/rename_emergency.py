# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import os, sys, glob, shutil
# gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python rename_emergency.py F:\Analogues_GCM_data\TilesByCountry\SRES_A1B\eth_30s F:\Analogues_GCM_data\ExtractByCountry_asciis\SRES_A1B\eth_30s"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]

# Check out Spatial Analyst extension license
# gp.CheckOutExtension("Spatial")

# Clear window
os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

# Get a list of grids in the workspace of each folder
varlist = "bio", "dtr", "prec", "tmean"
# typelist = "asciis", "tiffs"

modellist = sorted(glob.glob(dirbase + "\\*"))
for model in modellist:
	if not os.path.basename(model) == "current":
	
		# for type in typelist:
		print model

		for var in varlist:
			
			shutil.move(model + "\\2020_2049\\" + var + "_asciis", dirout + "\\" + os.path.basename(model) + "\\2020_2049\\" + var + "_asciis")
			# asclist = sorted(glob.glob(model + "\\2020_2049\\" + var + "_" + type + "\\*"))

				# # Lopping around the asciis
				# for asc in asclist:
					# print model,os.path.basename(asc)
					# os.rename(asc, model + "\\2020_2049\\" + var + "_" + type + "\\a1b_2020_2049_" + os.path.basename(asc))
	
print "\t ..done!!"