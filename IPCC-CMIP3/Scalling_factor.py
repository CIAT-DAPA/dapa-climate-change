# -----------------------------------------------------------
# Author: Carlos Navarro
# Date: April 13th 2011
# Pourpose: Make a raster layers from GCM downscaled datasets
# -----------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Scalling_factor.py E:\workspace\amkn\base-data\climate\current\_layers\_grids E:\workspace\amkn\base-data\climate\current\_layers\_grids_corected"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
# scenario = sys.argv[3]
# resolution = sys.argv[3]
# type = sys.argv[5]

#Clear screen
os.system('cls')
gp.toolbox="management"
gp.CheckOutExtension("Spatial")
print "~~~~~~~~~~~~~~~~~~~"
print "    Make Layers    " 
print "~~~~~~~~~~~~~~~~~~~"

###Future
sreslist = "sres_a1b", "sres_a2", "sres_b1" 
for sres in sreslist: 
	modellist = sorted(os.listdir(dirbase + "\\" + sres))
	for model in modellist:
		period = "2020_2049"
		# for period in sorted(periodDc):

		gp.workspace = dirbase + "\\" + sres + "\\" + model + "\\" + period
		diroutraster = dirout + "\\" + sres + "\\" + model + "\\" + period
		if not os.path.exists(diroutraster):
			os.system('mkdir ' + diroutraster)
		print "\n---> Processing: " + sres + " " + model + " " + period + "\n"
			
		#Get a list of raster into the workspace
		rasters = sorted(gp.ListRasters("", "GRID"))
		for raster in rasters:
				if os.path.basename(raster) == "bio_1" or os.path.basename(raster) == "bio_2" or os.path.basename(raster) == "bio_5" or os.path.basename(raster) == "bio_6" or os.path.basename(raster) == "bio_7" or os.path.basename(raster) == "bio_8" or os.path.basename(raster) == "bio_9" or os.path.basename(raster) == "bio_10" or os.path.basename(raster) == "bio_11" or os.path.basename(raster).split("_")[0] == "tmax" or os.path.basename(raster).split("_")[0] == "tmin" or os.path.basename(raster).split("_")[0] == "tmean":
					InExpression = raster + " * 0.1"
					print raster
					gp.SingleOutputMapAlgebra_sa(InExpression, diroutraster + "\\" + raster)    
				elif os.path.basename(raster) == "bio_4" or :
					print raster
					gp.SingleOutputMapAlgebra_sa(InExpression, diroutraster + "\\" + raster)  
					InExpression = raster + " * 0.001"
				else:
					print raster
					gp.CopyRaster_management(raster, diroutraster + "\\" + raster)  

# ####Current
# modellist = sorted(os.listdir(dirbase))
# for model in modellist:
	# period = "2020_2049"
	# # for period in sorted(periodDc):

	# gp.workspace = dirbase + "\\" + model
	# diroutraster = dirout + "\\" + model
	# if not os.path.exists(diroutraster):
		# os.system('mkdir ' + diroutraster)
	# print "\n---> Processing: " + model 
		
	# #Get a list of raster into the workspace
	# rasters = sorted(gp.ListRasters("", "GRID"))
	# for raster in rasters:
			# if os.path.basename(raster) == "bio_1" or os.path.basename(raster) == "bio_2" or os.path.basename(raster) == "bio_5" or os.path.basename(raster) == "bio_6" or os.path.basename(raster) == "bio_7" or os.path.basename(raster) == "bio_8" or os.path.basename(raster) == "bio_9" or os.path.basename(raster) == "bio_10" or os.path.basename(raster) == "bio_11" or os.path.basename(raster) == "tm*":
				# InExpression = raster + " * 0.1"
				# print raster
				# gp.SingleOutputMapAlgebra_sa(InExpression, diroutraster + "\\" + raster)    
			# elif os.path.basename(raster) == "bio_4":
				# print raster
				# gp.SingleOutputMapAlgebra_sa(InExpression, diroutraster + "\\" + raster)  
				# InExpression = raster + " * 0.001"
			# else:
				# print raster
				# gp.CopyRaster_management(raster, diroutraster + "\\" + raster)  
					
# print "Process done!"	