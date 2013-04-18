# ----------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Cut by mask, anomalies, disaggregated, interpolated or downscaled surfaces
# -----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: CutGCM_CMIP3.py <dirbase> <dirout> <scenario> <resolution> <period> <mask> <switch>"
	print "	  - ie: python CutGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled D:\Workspace a1b 30s 2010_2039 D:\Workspace\mask\mask YES"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder of averaged data"
	print "   sres		: IPCC Emission Escenario"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   period	: Future 30yr interval"	
	print "   mask		: This grid limites calculated to a specific region"
	print "   switch	: Set YES to convert outputs in ESRI-Ascii files"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
sres = sys.argv[3]
res = sys.argv[4]
period = sys.argv[5]
mask = sys.argv[6]
switch = sys.argv[7]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Cut GCM " + " " + str(resolution) + " " + sres
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
		
		# Define check file
		checkFile = dirout + "\\sres_" + sres + "\\" + model + "-" + period + "-extract-done.txt"
		if not os.path.exists(checkFile):
			
			print "\n Processing: ", sres, period, model + "\n"
			
			# Define and create output directory
			diroutGrd = dirout + "\\sres_" + sres + "\\Global" + model + "\\" + period
			if not os.path.exists(diroutGrd):
				os.system('mkdir ' + diroutGrd)

			#Get a list of raster in workspace
			rasters = sorted(gp.ListRasters("*", "GRID"))
			for raster in rasters:
				
				# Define out ESRI-Grid
				outGrd = diroutGrd + "\\" + raster
				
				if not gp.Exists(OutRaster):
					
					# Cut function
					gp.ExtractByMask_sa(gp.workspace + "\\" + raster, mask, outGrd)
					print "\t", raster, period, model "cutted"
					
					# Convert outputs to ESRI-Asciis
					diroutAsc = dirout + "\\sres_" + sres + "\\Global" + model + "\\" + period + "\\_asciis"
					outAsc = diroutAsc + "\\" + os.path.basename(raster) + ".asc"
					
					if switch == "YES" and not os.path.exists(outAsc):
						
						# Create ascii output directory 
						if not os.path.exists(diroutAsc):
							os.system('mkdir ' + diroutAsc)
						
						# Raster to ascii function
						gp.RasterToASCII_conversion(raster, outAsc)
						
						# Compress ESRI-asciis files
						inZip = diroutAsc + "\\" + os.path.basename(raster).split("_")[0] + "_asc.zip"
						os.system('7za a ' + inZip + " " + outAsc)
						os.remove(outAsc)
						gp.delete_management(raster)
						print "\t", os.path.basename(raster), period, model "grd2asc converted"

						# Remove trash files
						pjrList = sorted(glob.glob(diroutAsc + "\\*.pjr"))
						for pjr in pjrList:
							os.remove(pjr)
				
			checkFile = open(checkFile, "w")
			checkFile.close()
			print "\n " + sres + " " + period + " " + model + " processed\n"
		else:
			print "\n " + sres + " " + period + " " + model + " processed\n"

print "\n \t Process done!!"  