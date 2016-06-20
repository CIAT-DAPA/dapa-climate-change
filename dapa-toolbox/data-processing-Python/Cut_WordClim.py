# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: September 13th, 2010
# Purpose: Purpose: Cut by mask worldclim data and Extraction by mask of points
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob, shutil
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <Extract_MaskGCM.py>, <dirbase>, <dirout>, <mask>, <resolution>, <variable>, <ascii>, <describe>"
	print "   - ie: python python Cut_WordClim.py \\dapadfs\data_cluster_4\observed\gridded_products\worldclim D:\mask D:\Workspace\Output 30s prec NO NO"
	print "  dirbase	: Root folder where are storaged the datasets"
	print "  dirout	: Is the folder where the ESRI-Grid files are created"
	print "	 mask		: Input mask data defining areas to extract (ESRI-grid file or shapefile)."	
	print "	 Resolution	: Is a numeric value indicating the resolution of the input files in arc-minutes. The possibilities are 30s, 2_5 min, 5min, 10min"	
	print "	 variable : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace. "
	print "	 ascii	: Convert outputs ESRI-Grid files to Ascii"
	print "	 describe	: Create description file to outputs Rasters"
	
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
resolution = sys.argv[4]
variable = sys.argv[5]
ascii = sys.argv[6]
describe = sys.argv[7]

# Clean screen
os.system('cls')

gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"


gp.workspace = dirbase + "\\"  + "\\Global_" + str(resolution)

# Get a list of grids in the workspace
if variable == "ALL":
	variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")


# if variable == "ALL":
	# variablelist = sorted(gp.ListRasters("*", "GRID"))
# else:	
	# variablelist = sorted(gp.ListRasters(variable + "*", "GRID"))

diroutraster = dirout + "\\worldclim\\Global_" + str(resolution)

if not os.path.exists(diroutraster):
	os.system('mkdir ' + diroutraster)
if not os.path.exists(diroutraster):
	os.system('mkdir ' + diroutraster)	

if not os.path.exists(diroutraster +"\\Global_" +resolution + "_wordclim-done.txt"):

	if os.path.splitext(mask)[1] == ".shp":
		params = gp.GetParameterInfo()
		desc = gp.Describe(mask)
		if desc.ShapeType == "Point":	
			###extracts data defined by points
			print "\n---> Processing: \n"
			if not os.path.exists(diroutraster):
				os.system('mkdir ' + diroutraster)	
			# for variable in variablelist:
			for variable in variablelist:
				for month in range (1, 12 + 1, 1):	
					if variable == "cons_mths":
						raster = gp.workspace + "\\" + variable
					else:
						raster = gp.workspace + "\\" + variable + "_" + str(month)				
					# raster = gp.workspace + "\\" + variable 
					OutPointsFC = diroutraster + "\\wordclim-" + os.path.basename(raster) + ".dbf"
					if not os.path.exists(OutPointsFC):
						gp.AddMessage("\tExtracting .. " + os.path.basename(raster))
						#Check out Spatial Analyst extension license
						gp.CheckOutExtension("Spatial")
						gp.Sample_sa(os.path.basename(raster), mask, OutPointsFC, "")
						
					else:
						print "\t" + os.path.basename(raster) + " extracted"	
					
					if not os.path.basename(raster) + ".dbf" == variable.split("_")[0] + "_1.dbf":
						if os.path.basename(raster) != 'cons_mths':
							InData = diroutraster + "\\wordclim-" + variable.split("_")[0] + "_1.dbf"
							gp.AddMessage( "--->Joining .. " + os.path.basename(raster)  + "\n" )
							gp.joinfield (InData, "OID", OutPointsFC, "OID", os.path.basename(raster) )
							os.remove(OutPointsFC)
						
				# Remove trash files
				xmlList = sorted(glob.glob(diroutraster + "\\wordclim-*.xml"))
				for xml in xmlList:
					os.remove(xml)
				os.system( "del " + diroutraster + "\log")	
			
		else:
			gp.AddMessage("The mask should be shapefile points")
	# extracts data defined by an area
	else:
		# for raster in variablelist:
		for variable in variablelist:
			for month in range (1, 12 + 1, 1):
				if variable == "cons_mths":
					raster = gp.workspace + "\\" + variable
				else:
					raster = gp.workspace + "\\" + variable + "_" + str(month)	
					
				gp.AddMessage("    Extracting " + os.path.basename(raster))
				# raster = gp.workspace + "\\" + variable 
				OutRaster = diroutraster + "\\" + os.path.basename(raster)
				
				if not gp.Exists(OutRaster):
					# function cut by mask	
					gp.ExtractByMask_sa(raster, mask, OutRaster)

					if not os.path.exists(diroutraster):
						os.system('mkdir ' + diroutraster)
					
					if describe == "YES":	
						#create description file to Raster
						describefile = dirout + "\\Describefile_Global_" + str(resolution)  + "_wordclim.txt"
						if os.path.isfile(describefile):
							outFile = open(describefile, "a")
						else:
							outFile = open(describefile, "w")

						outFile.write("GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")
					
						MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
						MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
						MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
						STD = gp.GetRasterProperties_management(OutRaster, "STD")
						CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
						outFile = open(describefile, "a")
						outFile.write(raster + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

						
		# Raster to ascii function
		if ascii == "YES":
			gp.AddMessage( "==> Converting to ascii" )
			gp.workspace = diroutraster
			rasters1 = sorted(gp.ListRasters("*", "GRID"))
			for raster in rasters1:
				OutAscii = diroutraster + "\\" + os.path.basename(raster) + ".asc"
				gp.AddMessage( "    Converting " + OutAscii )
				gp.RasterToASCII_conversion(raster, OutAscii)
				
				# Compress ESRI-asciis files
				InZip = diroutraster + "\\" + os.path.basename(raster).split("_")[0] + "_asc.zip"
				os.system('7za a ' + InZip + " " + OutAscii)
				os.remove(OutAscii)
				gp.delete_management(raster)
				# os.remove(OutAscii[:-4]+".prj")
				#gp.delete_management(raster)

			# Remove trash files
			pjrList = sorted(glob.glob(diroutraster + "\\*.pjr"))
			for pjr in pjrList:
				os.remove(pjr)
			shutil.rmtree(diroutraster + '\\info')
			logList = sorted(glob.glob(diroutraster + "\\log"))
			for log in logList:
				os.remove(log)					
			

else:
	checkTXT = open(diroutraster +"\\Global_" +resolution + "_wordclim-done.txt", "w")
	checkTXT.close()
		

			
gp.AddMessage("\n \t ====> DONE!! <====")  