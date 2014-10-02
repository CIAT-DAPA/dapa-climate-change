# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: September 13th, 2010
# Updated: July 28th, 2014
# Purpose: Purpose: Cut by mask, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob, shutil
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 12:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <Extract_MaskGCM.py> <dirbase> <dirout> <mask> <dataset> <sres> <resolution> <models> <periods> <variable> <ascii> <descfile>"
	print "   - ie: python CutGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3 D:\PRUEBAS_SCRIPTs D:\PRUEBAS_SCRIPTs\mask\mask_italia downscaled A2 2_5min bcc_csm1_1 2020_2049 prec YES YES"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   dirout	: Out folder"	
	print "	  mask		: Input mask data defining areas to extract (ESRI-grid file or shapefile)"
	print "   dataset	: The possibilities are: downscaled, and interpolations dataset"	
	print "   sres	    : IPCC Emission Escenario. The possibilities are a1b, a2, b1"
	print "   resolution: Units Resolution in arcminutes. The possibilities are 30s 2_5min 5min 10min"
	print "	  models	: Global Climate Models. If you want to choose some models, only separated with commas without spaces. Use 'ALL' to choose all available models"
	print "   periods	: Future 30yr periods. If you want to choose some periods, enter them separated by commas without spaces; e.g. 2020_2049,2040_2069,2060_2089,2070_2099. Use 'ALL' to process all the periods"
	print "	  variable  : The possibilities are: bio(bioclimatic), prec(precipitation), tmax(maximum temperature), tmin(minimum temperature), tmean(average temperature). If you want to choose some variables, only separated with commas without spaces; e.g. prec,tmax,tmin. UUse 'ALL' to select all the data in the workspace."
	print "   ascii	    : If YES convert outputs ESRI-Grid files to Ascii and remove ESRI-Grid files"
	print "   descfile  : If YES describe properties of outputs ESRI-Grid files"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
dataset = sys.argv[4]
sres = sys.argv[5]
resolution = sys.argv[6]
models = sys.argv[7]
periods = sys.argv[8]
variable = sys.argv[9]
ascii = sys.argv[10]
descfile = sys.argv[11]

# Clean screen
os.system('cls')
gp.CheckOutExtension("Spatial")

if not os.path.exists(dirout + "\\" + dataset + "\\" + sres):
	os.system('mkdir ' + dirout + "\\" + dataset + "\\" + sres)
	
if descfile == "YES":
	#create description file to Raster	
	describefile = dirout + "\\" + dataset + "\\" + sres + "\\Describe_Global_" + str(resolution)  + ".txt"
	outFile = open(describefile, "w")
	outFile.write("SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")
	

if dataset == 'downscaled':

	print "~~~~~~~~~~~~~~~~~~~~~~"
	print " EXTRACT BY MASK GCM  "
	print "~~~~~~~~~~~~~~~~~~~~~~"

	#Get lists of periods
	if periods == "ALL":
		periodlist = '2020_2049','2040_2069','2060_2089','2070_2099'
	else:
		periodlist = periods.split(",")
		
	#Get lists of models
	if models == "ALL":
		modellist = sorted(os.listdir(dirbase + "\\"  + dataset + "\\" + sres + "\\Global_" + str(resolution) ))
	else: 
		modellist = models.split(",")
	#Get lists of data
	if variable == "ALL":
		variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
	else:
		variablelist = variable.split(",")
		
	gp.AddMessage("Models: " + str(modellist))
	gp.AddMessage( "Periods: " + str(periodlist) )	
	gp.AddMessage( "Variables: " + str(variablelist))		
	
		
	for model in modellist:
		# Looping around periods
		for period in periodlist:
		
			gp.workspace = dirbase + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period
			print gp.workspace
			if os.path.exists(gp.workspace) and not os.path.exists(dirout + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "_extract_" + period + "_done.txt"):
				gp.AddMessage( "\n---> Processing: "  + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period + "\n" )
				diroutraster = dirout + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period
				diroutascii =  dirout + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period

				if not os.path.exists(diroutraster):
					os.system('mkdir ' + diroutraster)
					
				if variable == "ALL":
					var = "*"
				else:	
					var = variable + "*"
					
				for variable in variablelist:
					if variable == "bio":
						num = 19
					else:
						num = 12
					for month in range (1, num + 1, 1):
						if variable == "cons_mths":
							raster = gp.workspace + "\\" + variable
						else:
							raster = gp.workspace + "\\" + variable + "_" + str(month)

						OutRaster = diroutraster + "\\" + os.path.basename(raster)
						
						if not gp.Exists(OutRaster):
						
							# function ExtractByMask_sa
							gp.ExtractByMask_sa(raster, mask, OutRaster)
							
							gp.AddMessage( "    Extracted " + os.path.basename(raster) )
							
							if not os.path.exists(diroutascii):
								os.system('mkdir ' + diroutascii)
								
							#create description file to Raster						
							if descfile == "YES":	
									
								MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
								MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
								MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
								STD = gp.GetRasterProperties_management(OutRaster, "STD")
								CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
								outFile = open(describefile, "a")
								outFile.write(sres + "\t" + model + "\t" + period + "\t" + os.path.basename(raster) + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

				if ascii == "YES":
					for variable in variablelist:
						for month in range (1, 12 + 1, 1):
							if variable == "cons_mths":
								raster = diroutraster + "\\" + variable
							else:
								raster = diroutraster + "\\" + variable + "_" + str(month)	
							if os.path.exists(raster):								
								OutAscii = diroutascii + "\\" + os.path.basename(raster) + ".asc"
								gp.AddMessage( "\n    Converting to ascii " + os.path.basename(raster) )
								gp.RasterToASCII_conversion(raster, OutAscii)
								
								# Compress ESRI-asciis files
								InZip = diroutascii + "\\" + os.path.basename(raster).split("_")[0] + "_asc.zip"
								os.system('7za a ' + InZip + " " + OutAscii)
								os.remove(OutAscii)
								if os.path.exists(OutAscii[:-3]+"prj"):
									os.remove(OutAscii[:-3]+"prj")
								gp.delete_management(raster)

					if os.path.exists(diroutraster + '\\info'):
						shutil.rmtree(diroutraster + '\\info')
					if os.path.exists(diroutraster + '\\log'):
						os.remove(diroutraster + '\\log')
	
				print " Done!!"
				checkTXT = open(dirout + "\\" + dataset + "\\" + sres + "\\Global_" + str(resolution) + "\\" + model + "_extract_" + period + "_done.txt", "w")
				checkTXT.close()
				
			else:
				gp.AddMessage( "The model " + model + " " + period + " is already processed" )
				print "Processing the next period \n"

else:

	print "~~~~~~~~~~~~~~~~~~~~~~"
	print " EXTRACT BY MASK GCM  "
	print "~~~~~~~~~~~~~~~~~~~~~~"

	#Get lists of periods
	if periods == "ALL":
		periodlist = '2010_2039', '2020_2049', '2030_2059', '2040_2069', '2050_2079', '2060_2089', '2070_2099'
	else:
		periodlist = periods.split(",") # ej: "2010_2039", "2050_2079", "2060_2089", "2030_2059"
	#Get lists of models
	if models == "ALL":
		modellist = sorted(os.listdir(dirbase + "\\"  + dataset + "\\" + sres))
	else: 
		modellist = models.split(",")

	#Get lists of data
	if variable == "ALL":
		variablelist = ["prec","tmin","tmax"]
	else:
		variablelist = variable.split(",")
		
	gp.AddMessage("Models: " + str(modellist))
	gp.AddMessage( "Periods: " + str(periodlist) )	
	gp.AddMessage( "Variables: " + str(variablelist))			
		
	# for model in modellist:
	for model in modellist:
		for period in periodlist:
		
			gp.workspace = dirbase + "\\" + dataset + "\\" + sres  + "\\" + model + "\\r1i1p1\\" + period

			if os.path.exists(gp.workspace) and not os.path.exists(dirout + "\\" + dataset + "\\" + sres + "\\" + model + "_extract_" + period + "_done.txt"):
				gp.AddMessage( "\n---> Processing: "  + dataset + "\\" + sres  + "\\" + model + "\\r1i1p1\\" + period + "\n" )
				diroutraster = dirout + "\\" + dataset + "\\" + sres  + "\\" + model + "\\r1i1p1\\" + period
				diroutascii =  dirout + "\\" + dataset + "\\" + sres  + "\\" + model + "\\r1i1p1\\" + period

				if not os.path.exists(diroutraster):
					os.system('mkdir ' + diroutraster)
				
				#Get a list of raster in workspace
				for variable in variablelist:
					for month in range (1, 12 + 1, 1):
						if variable == "cons_mths":
							raster = gp.workspace + "\\" + variable
						else:
							raster = gp.workspace + "\\" + variable + "_" + str(month)

						OutRaster = diroutraster + "\\" + os.path.basename(raster)

						if not gp.Exists(OutRaster):
							# function ExtractByMask_sa
							gp.ExtractByMask_sa(raster, mask, OutRaster)
							gp.AddMessage( "    Extracted " + os.path.basename(raster) )
							
							#Create output folder
							if not os.path.exists(diroutascii):
								os.system('mkdir ' + diroutascii)
							
							if descfile == "YES":
								#create description file to Raster
								MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
								MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
								MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
								STD = gp.GetRasterProperties_management(OutRaster, "STD")
								CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
								outFile = open(describefile, "a")
								outFile.write(sres + "\t" + model + "\t" + period + "\t" + os.path.basename(raster) + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")
							
				# Raster to ascii function
				if ascii == "YES":
					for variable in variablelist:
						for month in range (1, 12 + 1, 1):
							if variable == "cons_mths":
								raster = diroutraster + "\\" + variable
							else:
								raster = diroutraster + "\\" + variable + "_" + str(month)		
							if os.path.exists(raster):								
								OutAscii = diroutascii + "\\" + os.path.basename(raster) + ".asc"
								gp.AddMessage( "\n    Converting to ascii " + os.path.basename(raster) )
								gp.RasterToASCII_conversion(raster, OutAscii)
								
								# Compress ESRI-asciis files
								InZip = diroutascii + "\\" + os.path.basename(raster).split("_")[0] + "_asc.zip"
								os.system('7za a ' + InZip + " " + OutAscii)
								os.remove(OutAscii)
								if os.path.exists(OutAscii[:-3]+"prj"):
									os.remove(OutAscii[:-3]+"prj")
								gp.delete_management(raster)
						
					# Remove trash files
					if os.path.exists(diroutraster + '\\info'):
						shutil.rmtree(diroutraster + '\\info')
					if os.path.exists(diroutraster + '\\log'):
						os.remove(diroutraster + '\\log')				
						
				print " Done!!"
				
				checkTXT = open(dirout + "\\" + dataset + "\\" + sres + "\\" + model + "_extract_" + period + "_done.txt", "w")
				checkTXT.close()
				
			else:
				print "The model " + model + " " + period + " is already processed"
				print "Processing the next period \n"
			
gp.AddMessage("\n \t ====> DONE!! <====")  