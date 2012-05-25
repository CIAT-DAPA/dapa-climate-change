# -----------------------------------------------------------
# Author: Carlos Navarro
# Date: April 13th 2011
# Pourpose: Make a raster layers from GCM downscaled datasets
# -----------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python GCMLayers.py E:\workspace\amkn\base-data\climate\future-mean\_layers\_grids E:\workspace\amkn\base-data\climate\future-models\_layers E:\workspace\amkn\base-data\climate\layers_symbolic"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
dirlyr = sys.argv[3]
# scenario = sys.argv[3]
# resolution = sys.argv[3]
# type = sys.argv[5]

#Clear screen
os.system('cls')
gp.toolbox="management"

print "~~~~~~~~~~~~~~~~~~~"
print "    Make Layers    " 
print "~~~~~~~~~~~~~~~~~~~"


# # #Layers from sres
# # #Get lists of scenarios and models
# sreslist = "sres_a1b", "sres_a2", "sres_b1" 
# periodDc = {"2010_2039": "2020s", "2020_2049": "2030s", "2030_2059": "2040s", "2040_2069": "2050s", "2050_2079": "2060s", "2060_2089": "2070s", "2070_2099": "2080s"}
variableDc = {"bio_1" : "STD Annual Mean Temperature", "bio_2" : "STD Mean Diurnal Range", "bio_3" : "STD Isothermality", "bio_4" : "STD Temperature Seasonality", "bio_5" : "STD Max Temperature of Warmest Month", "bio_6" : "STD Min Temperature of Coldest Month", "bio_7" : "STD Temperature Annual Range", "bio_8" : "STD Mean Temperature of Wettest Quarter", "bio_9" : "STD Mean Temperature of Driest Quarter", "bio_10" : "STD Mean Temperature of Warmest Quarter", "bio_11" : "STD Mean Temperature of Coldest Quarter", "bio_12" : "STD Annual Precipitation", "bio_13" : "STD Precipitation of Wettest Month", "bio_14" : "STD Precipitation of Driest Month", "bio_15" : "STD Precipitation Seasonality", "bio_16" : "STD Precipitation of Wettest Quarter", "bio_17" : "STD Precipitation of Driest Quarter", "bio_18" : "STD Precipitation of Warmest Quarter", "bio_19" : "STD Precipitation of Coldest Quarter", "prec_1" : "STD Precipitation in January", "prec_2" : "STD Precipitation in February", "prec_3" : "STD Precipitation in March", "prec_4" : "STD Precipitation in April", "prec_5" : "STD Precipitation in May", "prec_6" : "STD Precipitation in June", "prec_7" : "STD Precipitation in July", "prec_8" : "STD Precipitation in August", "prec_9" : "STD Precipitation in September", "prec_10" : "STD Precipitation in October", "prec_11" : "STD Precipitation in November", "prec_12" : "STD Precipitation in December", "tmean_1" : "STD Mean Temperature in January", "tmean_2" : "STD Mean Temperature in February", "tmean_3" : "STD Mean Temperature in March", "tmean_4" : "STD Mean Temperature in April", "tmean_5" : "STD Mean Temperature in May", "tmean_6" : "STD Mean Temperature in June", "tmean_7" : "STD Mean Temperature in July", "tmean_8" : "STD Mean Temperature in August", "tmean_9" : "STD Mean Temperature in September", "tmean_10" : "STD Mean Temperature in October", "tmean_11" : "STD Mean Temperature in November", "tmean_12" : "STD Mean Temperature in December", "tmax_1" : "STD Max Temperature in January", "tmax_2" : "STD Max Temperature in February", "tmax_3" : "STD Max Temperature in March", "tmax_4" : "STD Max Temperature in April", "tmax_5" : "STD Max Temperature in May", "tmax_6" : "STD Max Temperature in June", "tmax_7" : "STD Max Temperature in July", "tmax_8" : "STD Max Temperature in August", "tmax_9" : "STD Max Temperature in September", "tmax_10" : "STD Max Temperature in October", "tmax_11" : "STD Max Temperature in November", "tmax_12" : "STD Max Temperature in December", "tmin_1" : "STD Min Temperature in January", "tmin_2" : "STD Min Temperature in February", "tmin_3" : "STD Min Temperature in March", "tmin_4" : "STD Min Temperature in April", "tmin_5" : "STD Min Temperature in May", "tmin_6" : "STD Min Temperature in June", "tmin_7" : "STD Min Temperature in July", "tmin_8" : "STD Min Temperature in August", "tmin_9" : "STD Min Temperature in September", "tmin_10" : "STD Min Temperature in October", "tmin_11" : "STD Min Temperature in November", "tmin_12" : "STD Min Temperature in December", "cons_mths" : "STD Consecutive Months"}

# for sres in sreslist: 
	# modellist = sorted(os.listdir(dirbase + "\\" + sres))
	# for model in modellist:
		# if os.path.basename(model) == "multimodel_std":
			# period = "2020_2049"
			# # for period in sorted(periodDc):

			# gp.workspace = dirbase + "\\" + sres + "\\" + model + "\\" + period
			# print "\n---> Processing: " + sres + " " + model + " " + period + "\n"
			# diroutlayer = dirout 
			# if not os.path.exists(diroutlayer):
				# os.system('mkdir ' + diroutlayer)
				
			# #Get a list of raster into the workspace
			# rasters = sorted(gp.ListRasters("", "GRID"))
			# for raster in rasters:
				# TmpLyr = sres + "-" + os.path.basename(model) + "-" + str(periodDc[period]) + "-" + raster
				# NameInLyr = str(variableDc[raster])
				# if not os.path.exists(diroutlayer + "\\" + TmpLyr  + ".lyr"):
					# print "  Making layer of " + TmpLyr 
					# gp.MakeRasterLayer_management(raster, NameInLyr)
					# sym = dirlyr + "\\" + os.path.basename(raster) + ".lyr"
					# gp.applysymbologyfromlayer(NameInLyr, sym)
					# FinalLyr = diroutlayer + "\\" + TmpLyr  + ".lyr"
					# gp.savetolayerfile(NameInLyr, FinalLyr)
				# else:
					# print "  Existing Layer " + TmpLyr 
# print "Process done!!!"    

inlayer = dirout + "\\sres_a1b-bccr_bcm2_0-2030s-prec_1.lyr"
outlayer = dirout + "\\_sres_a1b-bccr_bcm2_0-2030s-prec_1_mod.lyr"
sym = dirlyr + "\\prec_1.lyr"
gp.applysymbologyfromlayer(inlayer, sym)
gp.savetolayerfile(inlayer, outlayer)


# # #Layers for current
# modellist = sorted(os.listdir(dirbase))
# for model in modellist:
	# gp.workspace = dirbase + "\\" + os.path.basename(model)
	# diroutlayer = dirout
	# if not os.path.exists(diroutlayer):
		# os.system('mkdir ' + diroutlayer)

	# #Get a list of raster into the workspace
	# rasters = sorted(gp.ListRasters("", "GRID"))
	# for raster in rasters:
		# # if str(len(os.path.basename(raster).split("_")[1])) == "2":
		# TmpLyr = "current-" + os.path.basename(model) + "-" + raster
		# NameInLyr = str(variableDc[raster])
		# if not os.path.exists(diroutlayer + "\\" + TmpLyr  + ".lyr"):
			# print "  Making layer of " + TmpLyr 
			# gp.MakeRasterLayer_management(raster, NameInLyr)
			# sym = dirlyr + "\\" + os.path.basename(raster) + ".lyr"
			# gp.applysymbologyfromlayer(NameInLyr, sym)
			# FinalLyr = diroutlayer + "\\" + TmpLyr  + ".lyr"
			# gp.savetolayerfile(NameInLyr, FinalLyr)

			# # del TmpLyr
			# # del OutLyr
# print "Process done!!!"    




# ####Mosaic
# gp.toolbox="management"
# modellist = sorted(os.listdir(dirbase))

# for model in modellist:
	# print model
	# gp.workspace = dirbase + "\\" + os.path.basename(model)
	# rasters = sorted(gp.ListRasters("prec_*", "GRID"))
	# for raster in rasters:
		# print raster
		# diroutraster = dirlyr + "\\tmp"
		# gp.CopyRaster_management(gp.workspace + "\\" + raster, diroutraster + "\\" + raster)
		# gp.delete_management(raster)
		# gp.extent = diroutraster + "\\" + raster
		# gp.SnapRaster = diroutraster + "\\" + raster
		# gp.MosaicToNewRaster_management(diroutraster + "\\" + raster + ";" + dirlyr + "\\mosaic\\mosaic", gp.workspace, raster)
		# gp.delete_management(diroutraster + "\\" + raster)