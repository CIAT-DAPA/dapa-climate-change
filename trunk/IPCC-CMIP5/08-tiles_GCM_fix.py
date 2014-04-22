# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: March 30th, 2011
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcpy, os, sys, string, glob, shutil
from arcpy import env

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python 08-tiles_GCM_fix.py T:\gcm\cmip5\ipcc_5ar_ciat_tiled T:\gcm\cmip5\downscaled G:\jetarapues\cmip5_process\cmip5_tiles_process T:\gcm\cmip5\ipcc_5ar_ciat_tiled rcp26 30s"
	print "   Syntax	: <Tiles_GCM.py>, <dirbase>, <dirtemp>, <scenario>, <dirout>, <scenario>, <res>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   dirtemp 	: Where is made calculations"
	print "   dirout	: Out folder"
	print "   scenario  : A1B, A2 or B1"
	print "   res: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: Disaggregated, Interpolated or Downscaled"	
	sys.exit(1)

dirTiles = sys.argv[1]
dirBase = sys.argv[2]
dirTmp = sys.argv[3]
dirOut = sys.argv[4]
rcp = sys.argv[5]
res = sys.argv[6]


os.system('cls')
# gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "    SPLIT IN TILES     "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

if rcp=='rcp26':
	modellist = ['bcc_csm1_1', 'bcc_csm1_1_m', 'bnu_esm', 'cccma_canesm2', 'cesm1_cam5', 'csiro_mk3_6_0', 'fio_esm', 'gfdl_cm3', 'gfdl_esm2g', 'gfdl_esm2m', 'giss_e2_h', 'giss_e2_r', 'ipsl_cm5a_lr', 'ipsl_cm5a_mr', 'lasg_fgoals_g2', 'miroc_esm', 'miroc_esm_chem', 'miroc_miroc5', 'mohc_hadgem2_es', 'mpi_esm_lr', 'mpi_esm_mr', 'mri_cgcm3', 'ncar_ccsm4', 'ncc_noresm1_m', 'nimr_hadgem2_ao']

ens= 'r1i1p1'

rcpDc = {"rcp26": "rcp2_6", "rcp45": "rcp4_5", "rcp60": "rcp6_0", "rcp85": "rcp8_5"}
periodDc = {"2030s":"2020_2049", "2050s":"2040_2069", "2070s":"2060_2089", "2080s":"2070_2099"}
periodList = ["2030s", "2050s", "2070s","2080s"]
variablelist = ["bio","tmin","tmax","tmean","prec","cons"]

zoneDc = {"0":"c1", "1":"c2", "2":"c3", "3":"c4", "4":"c5", "5":"c6", "6":"b1", "7":"b2", "8":"b3", "9":"b4", "10":"b5", "11":"b6", "12":"a1", "13":"a2", "14":"a3", "15":"a4", "16":"a5", "17":"a6"}

latDc = ["a", "b", "c"]
lonDc = ["1", "2", "3", "4", "5", "6"]


check = []

for period in periodList:
	for model in sorted(modellist):
		fileList = dirTiles + "\\" + rcpDc[rcp]+ "\\" +period+ "\\" +model + "\\" + res
		checkFile = dirTiles + "\\check_"+rcpDc[rcp] + ".txt"
		# if os.path.exists(checkFile):
		for var in variablelist:
			for lat in sorted(latDc):
				for lon in sorted(lonDc): 
					inZip = fileList + "\\" + model + "_" + rcpDc[rcp] + "_" + period + "_" + var + "_" + res + "_r1i1p1_" + lat + lon + "_asc.zip"
					if not os.path.exists(inZip):
						# print inZip
						check.append(model + "-" + rcpDc[rcp] + "-" + period + "-" + var)
						
###################################################################################################					
	
			
print '----------- Fix check -----------------------\n'
print list(set(check))
print '\n-------------------------------------\n'

for item in  list(set(check)):
	model = item.split('-')[0]
	period = item.split('-')[2]
	var = item.split('-')[3]

	dirProc = dirTmp + "\\" + rcp + "_" + model + "_" + period
	dirgrids = dirTmp + "\\" + rcp + "_" + model + "_" + period + "\grids"
	dirCopy = dirOut + "\\" + rcpDc[rcp] + "\\" + period + "\\" +  model + "\\" + res	
	
	checkfile = dirCopy + "_tiles_done1.txt"
	if not os.path.exists(checkfile):	
		
		if not os.path.exists(dirProc):
			os.system('mkdir ' + dirProc)
		if not os.path.exists(dirgrids):
			os.system('mkdir ' + dirgrids)
			
		print "\n\tprocessing", model,period,var + "\n"

		##### Get a list of raster in workspace
		arcpy.env.workspace = dirBase + "\\" + rcp + "\\global_" + str(res) + "\\" + model + "\\" + ens + "\\" + periodDc[period]

		rasterList = sorted(arcpy.ListRasters(var+"*", "GRID"))
		for raster in rasterList:
			if not arcpy.Exists(dirgrids+"\\"+raster+"_17"):
				arcpy.SplitRaster_management(raster, dirgrids, raster + "_", "NUMBER_OF_TILES",  "GRID", "#", "6 3", "#", "0", "DEGREES", "#", "#")
				print "\t ", os.path.basename(raster), " splited"# + "_"+str(i)
			else:
				print "\t ", os.path.basename(raster), " splited"		
		
		#########Get a list of raster in processing dir
		arcpy.env.workspace = dirgrids		
		rasterList = sorted(arcpy.ListRasters("", "GRID"))
		for raster in rasterList:
			
			############# Convert to ESRI-Ascii
			var = os.path.basename(raster).split("_")[0]
			month = os.path.basename(raster).split("_")[1]
			zone = os.path.basename(raster).split("_")[-1]
				
			dirAsc = dirTmp + "\\" + rcp + "_" + model + "_" + period  + "\\ascii\\" + var + "_" + zoneDc[zone]
			if not os.path.exists(dirAsc):
				os.system('mkdir ' + dirAsc)
			
			outAsc = dirAsc + "\\" + var + "_" + month + ".asc"
			prjAsc = dirAsc + "\\" + var + "_" + month + ".prj"
			# print outAsc
			if not os.path.exists(outAsc): 
				try:
					##########os.system("gdal_translate -of AAIGrid -ot Int16 -quiet " + arcpy.env.workspace + "\\" + raster + " " + outAsc)
					arcpy.RasterToASCII_conversion(raster, outAsc)
					
				except:
					arcpy.CalculateStatistics_management(raster)
					arcpy.RasterToASCII_conversion(raster, outAsc)
				print "\t", os.path.basename(raster), " converted to ascii"
				# arcpy.Delete_management(raster)
			else:
				print "\t", os.path.basename(raster), " converted"
				
			if os.path.exists(prjAsc):
				os.system("del /s /q " + prjAsc)					
			
		#################### Compress by Zones
		print "\n .> Compress by Zones: ", rcp, model, str(res), ens, period, "\n"

		######### if os.path.exists(dirProc + "\\info"):
			###### os.system("rmdir /s /q " + dirProc + "\\info")
		if not os.path.exists(dirTmp + "\\" + rcp + "_" + model + "_" + period  + "\\ZipVarZonas"):
			os.system('mkdir ' + dirTmp + "\\" + rcp + "_" + model + "_" + period  + "\\ZipVarZonas")

			
		varZones = sorted(os.listdir(dirProc+ "\\ascii"))
		for varZone in varZones:
			######### if varZone.split("_")[0] == "cons":
				###########inZip = dirProc + "\\" + model + "_" + rcpDc[rcp] + "_" + periodDc[period] + "_" + varZone.split("_")[0] + "_" + res + "_" + ens + "_" + varZone.split("_")[1] + "_asc.zip"
			######## else:
			inZip = dirProc + "\\ZipVarZonas\\" + model + "_" + rcpDc[rcp] + "_" + period + "_" + varZone.split("_")[0] + "_" + res + "_" + ens + "_" + varZone.split("_")[1] + "_asc.zip"
			if not os.path.exists(inZip):
				os.system('7za a -tzip ' + inZip + " " + dirProc + "\\ascii\\" + varZone)
				
				os.system("rmdir /s /q " + dirProc + "\\" + varZone)
				
				print varZone, " compressed!"
			
			########## else:
				######### Copying to output dir
				if not os.path.exists(dirCopy + "\\"+os.path.basename(inZip) ):
					print "\n .> Copying to output dir: ", rcp, model, str(res), ens, period, "\n"
					os.system("robocopy " + dirProc + "\\ZipVarZonas" + " " + dirCopy + " "+os.path.basename(inZip)+ " /z /e /MOV")
		
		# os.system("rmdir /s /q " + dirTmp + "\\" + rcp + "_" + model + "_" + period  + "\\ascii")
		
		os.system("rmdir /s /q " + dirProc)
		# checkTxt = open(checkfile, "w")
		# checkTxt.close()
		
		print "\n .> Slit in tiles: ", rcp, model, str(res), ens, period, " done! \n"

	else:
		
		print "\n .> Slit in tiles: ", rcp, model, str(res), ens, period, " done! \n"
	
	
			
				
print "Process done!!!" 