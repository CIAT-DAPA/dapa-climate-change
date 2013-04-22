# -----------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Claculate zonal statistics for diseggregated, interpolated, anomalies or downscaled surfaces
# -----------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: ZonalStatisticsGCM_CMIP3.py <dirbase> <dirout> <scenario> <resolution> <mask>"
	print "	  - ie: python CutGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled D:\Workspace a1b 30s D:\Workspace\mask\mask"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder for the statistics"
	print "   sres		: IPCC Emission Escenario"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   mask		: shapefile with full path and extension"
	print "   switch	: Set YES to convert outputs in ESRI-Ascii files"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
sres = sys.argv[3]
resolution = sys.argv[4]
mask = sys.argv[5]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Calculate Statistics GCM " + " " + str(resolution) + " " + sres
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"


#Get lists of sress and models
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase  + "\\sres_" + sres + "\\Global_" + str(resolution)))
print "\nAvailable models: " + str(modellist)

# Looping around periods
for period in periodlist:

	# for model in modellist:
	for model in modellist:
		
		# Set workspace by each model
		gp.workspace = dirbase + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		
		# Define out table file (dbf extension)
		outSta = dirout + "\\" + sres + "-" + model + "-" + period + ".dbf"
		
		if not gp.Exists(outSta):
			
			print "\n\t Processing: ", sres, period, model + "\n"
			
			# Define and create output directory
			if not os.path.exists(dirout):
				os.system('mkdir ' + dirout)

			#Get a list of raster in workspace
			rasters = sorted(gp.ListRasters("*", "GRID"))
			for raster in rasters:
				
				if not os.path.basename(raster).split("_")[0] == "tmean":
					
					# Zonal statistical function
					outDbf = dirout + "\\" + sres + "-" + model + "-" + period + "-" + os.path.basename(raster) + ".dbf"
					if gp.Exists(outDbf):
						os.remove(outDbf)
						gp.ZonalStatisticsAsTable_sa(mask, "FID", raster, outDbf, "DATA")
						print "\t", raster, period, model, "stats calculated"
					else:
						gp.ZonalStatisticsAsTable_sa(mask, "FID", raster, outDbf, "DATA")
						print "\t", raster, period, model, "stats calculated"
							
			# Join dbfs files extracted
			print "\n\t .. Joining outputs"
			dbfList = sorted(glob.glob(dirout + "\\" + sres + "-" + model + "-" + period + "*.dbf"))
			for dbf in dbfList:
				inData = dirout + "\\" + sres + "-" + model + "-" + period + "-bio_1.dbf"
				if not os.path.basename(dbf)[-9:] == "bio_1.dbf":
					gp.joinfield (InData, "FID", dbf, "FID", "MEAN")
					gp.joinfield (InData, "FID", dbf, "FID", "STD")
					gp.joinfield (InData, "FID", dbf, "FID", "SUM")
					os.remove(dbf)

			# Rename join file
			os.rename(dirout + "\\" + sres + "-" + model + "-" + period + "-bio_1.dbf", outSta)
			
			# Delete Trash
			xmlList = sorted(glob.glob(dirout + "\\*.xml"))
			for xml in xmlList:
				os.remove(xml)
			
			print "\n\t ", sres, period, model, "stats calcs done"
		
		else:
			print "\n\t ", sres, period, model, "stats calcs done"
			
print "\n \t Process done!!"  


		
print "done!!!" 