# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import os, sys, glob, shutil
# gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 010-rename_emergency.py D:\cenavarro\Analogues_GCM_data\TilesByCountry\a1b\pan_30s\current\1960_1990\bio_tif"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
# dirout = sys.argv[2]

# Check out Spatial Analyst extension license
# gp.CheckOutExtension("Spatial")

# Clear window
os.system('cls')

print "\n~~~~ ASCII TO GRID ~~~~\n"

# Get a list of grids in the workspace of each folder
varlist = "bio", "dtr", "prec", "tmean"
# typelist = "asciis", "tiffs"

# modellist = sorted(glob.glob(dirbase + "\\*"))
for var in varlist:
	tifdir = dirbase + "\\" + var + "_tif"
	tiflist = sorted(glob.glob(tifdir + "\\*"))
	
	for tif in tiflist:
		print os.path.basename(tif).replace("a1b", "baseline").replace("2020_2049", "1960_1990")
		os.rename(tif, tifdir + "\\" + os.path.basename(tif).replace("a1b", "baseline").replace("2020_2049", "1960_1990"))
	
	
# countrylist = "eth", "tza", "ken", "uga" 
# for country in countrylist:
	# modellist = sorted(glob.glob(dirbase + "\\" + country + "_30s\\*"))
	
	# for model in modellist:
		# # if not os.path.basename(model) == "current":
		
			# # # for type in typelist:
			# # print country,model

			# # for var in varlist:
				# # os.rename(model + "\\2020_2049\\" + var + "_tiffs", model + "\\2020_2049\\" + var + "_tif")
				# # tiflist = sorted(glob.glob(model + "\\2020_2049\\" + var + "_tif\\*"))
				# # for tif in tiflist:
					# # if var == "bio":
						# # print country,model,tif[:-6] + ".tif"
						# # os.rename(tif, tif[:-6] + ".tif")
					# # else:
						# # print country,model,os.path.dirname(tif) + "\\" + os.path.basename(tif)[14:]
						# # os.rename(tif, os.path.dirname(tif) + "\\" + os.path.basename(tif)[14:])
						
		# if os.path.basename(model) == "current":
		
			# # for type in typelist:
			# print country,model

			# for var in varlist:
				# shutil.move(model + "\\" + var + "_tiffs", dirout + "\\" + country + "_30s\\current\\1960_1990\\" + var + "_tif")
				# tiflist = sorted(glob.glob(dirout + "\\" + country + "_30s\\current\\1960_1990\\" + var + "_tif\\*"))
				# for tif in tiflist:
					# if var == "bio":
						# print country,model,os.path.dirname(tif) + "\\baseline_1960_1990_" + os.path.basename(tif)[:-6] + ".tif"
						# os.rename(tif, os.path.dirname(tif) + "\\baseline_1960_1990_" + os.path.basename(tif)[:-6] + ".tif")
					# else:
						# print country,model,os.path.dirname(tif) + "\\baseline_1960_1990_" + os.path.basename(tif)
						# os.rename(tif, os.path.dirname(tif) + "\\baseline_1960_1990_" + os.path.basename(tif))
				
				# # shutil.move(model + "\\2020_2049\\" + var + "_tiffs", dirout + "\\" + os.path.basename(model) + "\\2020_2049\\" + var + "_asciis")
				# # asclist = sorted(glob.glob(model + "\\2020_2049\\" + var + "_" + type + "\\*"))

					# # # Lopping around the asciis
					# # for asc in asclist:
						# # print model,os.path.basename(asc)
						# # os.rename(asc, model + "\\2020_2049\\" + var + "_" + type + "\\a1b_2020_2049_" + os.path.basename(asc))
		
# print "\t ..done!!"