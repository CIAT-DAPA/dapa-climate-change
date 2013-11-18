# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: 11/2013
# Purpose: Grid to zip CMIP5
# ----------------------------------------------------------------------------------


# python G:\_scripts\dapa-climate-change\IPCC-CMIP5\07-ZipGrids_process.py T:\data\gcm\cmip5\downscaled S:\data\portals\ccafs_climate\download_data\files\data\ipcc_5ar_ciat_downscaled rcp26 10min

import arcgisscripting, os, sys, string, shutil
gp = arcgisscripting.create(9.3)

rootdir = sys.argv[1]
outputdir = sys.argv[2]
rcp = sys.argv[3]
resol = sys.argv[4]


# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")
gp.toolbox="management"

tmp = "G:\jetarapues\cmip5_process"

if rcp == "rcp26": 
	rcpmod = "rcp2_6"
elif rcp == "rcp45": 
	rcpmod = "rcp4_5"
elif rcp == "rcp60": 
	rcpmod = "rcp6_0"
else:
	rcpmod = "rcp8_5"

periodlist = ["2020_2049", "2040_2069", "2060_2089", "2070_2099"]
vartype = ["bio", "cons", "prec", "tmax", "tmean", "tmin"]	

modellist = sorted(os.listdir(rootdir + "\\" + rcp + "\\global_" +resol))
for period in periodlist:
	for model in modellist:

		if period == "2020_2049": 
			tsmod = "2030s"
		elif period == "2040_2069": 
			tsmod = "2050s"
		elif period == "2060_2089":
			tsmod = "2070s"
		else:
			tsmod = "2080s"	
			
		checkFile = outputdir+ "\\" +rcpmod+ "\\" +tsmod+ "\\" +model+ "\\" +resol + "_grd2zip_done.txt"
		if not os.path.exists(checkFile):
			gp.workspace = rootdir + "\\" + rcp + "\\global_" +resol + "\\" + model + "\\r1i1p1\\" + period
			outmoddir = outputdir+ "\\" +rcpmod+ "\\" +tsmod+ "\\" +model+ "\\" +resol
			print "==== Processing",rcp, resol, model, period+" ("+tsmod+")\n"
			rasters = sorted(gp.ListRasters("*", "GRID"))
			for raster in rasters:
				outgrid = tmp+ "\\" +rcpmod+ "\\" +tsmod+ "\\" +model+ "\\" +resol+ "\\" +raster.split("_")[0]
				inzip = outmoddir+ "\\" +model+"_"+rcpmod+"_"+tsmod+"_"+raster.split("_")[0]+"_"+resol+"_r1i1p1_no_tile_grd.zip"
				if not os.path.exists(inzip):
					if not os.path.exists(outgrid):
						os.system('mkdir ' + outgrid)
					if not os.path.exists(outgrid+ "\\" +raster):	
						gp.CopyRaster_management(raster,outgrid+ "\\" +raster)	
					print "Copied ", outgrid+ "\\" +raster	
			for raster in vartype:
				outgrid = tmp+ "\\" +rcpmod+ "\\" +tsmod+ "\\" +model+ "\\" +resol+ "\\" +raster
				inzip = outmoddir+ "\\" +model+"_"+rcpmod+"_"+tsmod+"_"+raster+"_"+resol+"_r1i1p1_no_tile_grd.zip"
				if not os.path.exists(inzip):			
					os.system('7za a -mmt=4 ' + inzip + " " + outgrid)
					shutil.rmtree(outgrid)
				print "\n " + model,rcpmod,tsmod,raster,resol + " processed\n"					
			checkFile = open(checkFile, "w")
			checkFile.close()				
		else:
			print "\n " + rcp, resol, model, period+" ("+tsmod+")" + " processed\n"			