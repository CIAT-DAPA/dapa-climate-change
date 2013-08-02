# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 23-7-2012
# Purpose: Create tile from GCM data
# ----------------------------------------------------------------------------------

import arcpy, os, sys, string, glob
from arcpy import env

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python tiles_world.py G:\jtarapues\Global G:\jtarapues\tile_global 2_5min SRES_a1b"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
resolution = sys.argv[3]
sres = sys.argv[4]

os.system('cls')
period = "2020_2049"


print "~~~~~~~~~~~~~~~~~~~"
print "  TILES COUNTRIES  "
print "~~~~~~~~~~~~~~~~~~~"

if not sres == "Baseline":
	modellist = sorted(os.listdir(dirbase + "\\" + sres + "\\downscaled\\Global_" + str(resolution)))
		
	for model in modellist:
		print model ########
		if not os.path.exists(dirout + "\\" + sres + "_" + model + "_tiles_world_done.txt"):
			arcpy.env.workspace = dirbase + "\\" + sres + "\\downscaled\\Global_" + str(resolution) + "\\" + model + "\\" + str(period)
			print arcpy.env.workspace #####
			diroutGrids = dirout + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + str(period)

			print "\nProcessing",sres,model,period,"\n"
			rasterList = arcpy.ListRasters("bio*", "GRID")
			for raster in rasterList:
				print raster
				
				if os.path.basename(raster).split("_")[0] == "bio": # or os.path.basename(raster).split("_")[0] == "prec" or os.path.basename(raster).split("_")[0] == "tmean" or os.path.basename(raster).split("_")[0] == "dtr":
					
					diroutGridsVar = diroutGrids + "\\" + os.path.basename(raster).split("_")[0]
					if not os.path.exists(diroutGridsVar):
						os.system('mkdir ' + diroutGridsVar)
					
					trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
					for trashfile in trashList:
						os.remove(trashfile)
							
					if not arcpy.Exists(diroutGridsVar + "\\" + raster + "_161"):
						
						print "\tspliting .. ",raster
						
						rasterdeleteList = sorted(glob.glob(diroutGridsVar + "\\" + os.path.basename(raster) + "_*"))
						for rasterdelete in rasterdeleteList:
							arcpy.Delete_management(rasterdelete)
						
						arcpy.SplitRaster_management(raster, diroutGridsVar, raster + "_", "NUMBER_OF_TILES", "GRID", "#", "18 9 ", "#", "0", "PIXELS", "#", "#")
						print "\t" + raster,"tiled"					
						
						trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
						for trashfile in trashList:
							os.remove(trashfile)
						
					else:
						print "\t" + raster,"tiled"
				
			#Create check file
			checkTXT = open(dirout + "\\" + sres + "_" + model + "_tiles_world_done.txt", "w")
			checkTXT.close()
			print "\nGlobal_2.5min",sres,model," Processed!"
		else:
			print "\nGlobal_2.5min",sres,model," Processed!"

				
			
			
			
else:
	model = "current"
	if not os.path.exists(dirout + "\\" + sres + "_" + model + "_tiles_world_done.txt"):
			arcpy.env.workspace = dirbase + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model
			diroutGrids = dirout + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model

			print "\nProcessing",sres,model,period,"\n"
			rasterList = arcpy.ListRasters("*", "GRID")
			for raster in rasterList:
				
				if os.path.basename(raster).split("_")[0] == "bio" or os.path.basename(raster).split("_")[0] == "prec" or os.path.basename(raster).split("_")[0] == "tmean" or os.path.basename(raster).split("_")[0] == "dtr":
					
					diroutGridsVar = diroutGrids + "\\" + os.path.basename(raster).split("_")[0]
					if not os.path.exists(diroutGridsVar):
						os.system('mkdir ' + diroutGridsVar)
					
					trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
					for trashfile in trashList:
						os.remove(trashfile)
							
					if not arcpy.Exists(diroutGridsVar + "\\" + raster + "_161"):
						
						print "\tspliting .. ",raster
						
						rasterdeleteList = sorted(glob.glob(diroutGridsVar + "\\" + os.path.basename(raster) + "_*"))
						for rasterdelete in rasterdeleteList:
							arcpy.Delete_management(rasterdelete)
						
						arcpy.SplitRaster_management(raster, diroutGridsVar, raster + "_", "NUMBER_OF_TILES", "GRID", "#", "18 9 ", "#", "0", "PIXELS", "#", "#")
						print "\t" + raster,"tiled"					
						
						trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
						for trashfile in trashList:
							os.remove(trashfile)
						
					else:
						print "\t" + raster,"tiled"
				
			#Create check file
			checkTXT = open(dirout + "\\" + sres + "_" + model + "_tiles_world_done.txt", "w")
			checkTXT.close()
			print "\nGlobal_2.5min",model," Processed!"
	else:
		print "\nGlobal_2.5min",model," Processed!"


		

print "\n Process done!"
