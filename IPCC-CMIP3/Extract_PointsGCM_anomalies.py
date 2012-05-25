# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_PointsGCM_anomalies.py L:\climate_change\IPCC_CMIP3 A2 D:\Workspace\Mexico_SMO\Estaciones_Mexico\Estaciones_PCP.shp D:\Workspace\Mexico_SMO\Estaciones_Mexico\extract"
	print "   Syntax	: <Extract_MaskGCM.py>, <dirbase>, <scenario>, <mask>, <dirout>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   scenario	: A1B, A2 or B1"
	print "	  mask		: shape with full path and extension"
	print "   dirout	: Out folder"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
scenario = sys.argv[2]
mask = sys.argv[3]
dirout = sys.argv[4]

# resolution = sys.argv[5]
# type = sys.argv[6]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

#Get lists 
periodlist = "2010_2039" , "2040_2069", # "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = "bccr_bcm2_0", "cccma_cgcm3_1_t47", "cnrm_cm3", "csiro_mk3_5", "gfdl_cm2_0", "gfdl_cm2_1", "ingv_echam4", "miroc3_2_medres", "miub_echo_g", "mpi_echam5", "mri_cgcm2_3_2a", "ukmo_hadcm3", "ukmo_hadgem1"
# modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\anomalies\\")
# variablelist = "tmin", "tmax", "prec"
print "Available models: " + str(modellist)
variable = "tmean"

for model in modellist:
	for period in periodlist:
	
		print "\n---> Processing: " + "SRES_" + scenario + " " + model + " " + period + "\n"
		diroutpoints = dirout + "\\_extract_" + "SRES_" + scenario 
		if not os.path.exists(diroutpoints):
			os.system('mkdir ' + diroutpoints)	
		
		if not os.path.exists(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt"):
			# for variable in variablelist:
			
			for month in range (1, 12 + 1, 1):
				
				gp.workspace = dirbase + "\\SRES_" + scenario + "\\anomalies\\" + model + "\\" + period
				raster = gp.workspace + "\\" + variable + "_" + str(month) #sorted(gp.ListRasters(variable + "*", "GRID"))
				
				# for raster in rasters:
				#OutRaster = diroutraster + "\\" + raster
				#gp.clip_management(raster,"-100 -60 -30 23 ",OutRaster)
				InPointsFC = mask 
				OutPointsFC = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + os.path.basename(raster) + ".dbf"

				if not os.path.exists(OutPointsFC):
					print "\tExtracting .. " + os.path.basename(raster)
					#Check out Spatial Analyst extension license
					gp.CheckOutExtension("Spatial")

					#Process: Cell Statistics...
					gp.Sample_sa(raster, InPointsFC, OutPointsFC, "")
				else:
					print "\t" + os.path.basename(raster) + " extracted"
			
			# print "\n"
			# for variable in variablelist:
			for month in range (1, 12 + 1, 1):
				dbf = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + variable + "_" + str(month) + ".dbf"

				if not os.path.basename(dbf)[-11:] == "tmean_1.dbf":
					print "\tJoining .. " + os.path.basename(dbf)
					InData = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_tmean_1.dbf"
					fields = os.path.basename(dbf)[:-4].split("_")[-2:]
					gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
					os.remove(dbf)
			
			xmlList = sorted(glob.glob(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "*.xml"))
			for xml in xmlList:
				os.remove(xml)
			
			checkTXT = open(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt", "w")
			checkTXT.close()
print "done!!!" 