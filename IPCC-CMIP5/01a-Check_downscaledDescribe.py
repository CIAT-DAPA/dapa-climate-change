# ---------------------------------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: January 20th, 2015
# Purpose: Describe properties of Grids Downscaled, Disaggregated, anomalies or Interpolated cmip5 datasets
# ---------------------------------------------------------------------------------------------------------

# python D:\jetarapues\_scripts\01a-Check_downscaledDescribe.py T:\gcm\cmip5 rcp26 30s D:\jetarapues\check_downscaled prec,bio,cons_mths downscaled 0 35
# python D:\jetarapues\_scripts\01a-Check_Interpolation.py T:\gcm\cmip5 rcp45 30s D:\jetarapues\_scripts\forCheckRasters D:\jetarapues\_scripts\forCheckRasters\zonesCheck.shp ID prec downscaled 0 35
# python D:\jetarapues\_scripts\01a-Check_Interpolation.py T:\gcm\cmip5 rcp60 30s D:\jetarapues\_scripts\forCheckRasters D:\jetarapues\_scripts\forCheckRasters\zonesCheck.shp ID prec downscaled 0 35
# python D:\jetarapues\_scripts\01a-Check_Interpolation.py T:\gcm\cmip5 rcp85 30s D:\jetarapues\_scripts\forCheckRasters D:\jetarapues\_scripts\forCheckRasters\zonesCheck.shp ID prec downscaled 0 35


# Import system modules
import arcpy, sys, os, string, glob
from arcpy import env
from arcpy.sa import *


#Syntax 
if len(sys.argv) < 9:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax: 10-describe-gcm-cmip5.py <dirbase> <rcp> <resolution>"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
rcp = sys.argv[2]
res = sys.argv[3]
dirout= sys.argv[4]
variable= sys.argv[5]
type= sys.argv[6] # ensemble
mod1= sys.argv[7]
mod2= sys.argv[8]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

#Get lists of models and periods
periodlist = ["2020_2049", "2040_2069", "2060_2089", "2070_2099"]

if(type=="ensemble"):
	modellist = ["ensemble"]
	outdir= dirout + '\\ensemble\\'+ rcp
if(type=="downscaled"):
	modellist = sorted(os.listdir(dirbase + "\\downscaled\\" + rcp + "\\global_" + res))
	outdir= dirout
if(type=="interpolations"):
	modellist = sorted(os.listdir(dirbase + "\\interpolations\\" + rcp))
	outdir= dirout + '\\'+ rcp
	
checkFile=dirout + "\\" + rcp+ "_check.txt"

	
print "\nAvailable models: " + str(modellist)
	
if not os.path.exists(outdir):
	os.system('mkdir ' + outdir)	
			
if variable == "ALL":
	if(type=="interpolations"):
		variablelist = ["prec","tmin","tmax"]
	else:
		variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")

descfileMerge = outdir + '\\describe_'+ rcp +'.txt' 

if not os.path.isfile(descfileMerge):
	outFile = open(descfileMerge, "w")
	outFile.write("MODELS" + "\t" + "PERIODS" + "\t" + "VARIABLES" 
	+ "\tSIZE\t" + "TOP_ALL" + "\t" + "LEF_ALL" + "\t" + "RIG_ALL" + "\t" + "BOT_ALL" + "\t" + "CEX_ALL" + "\t" + "CEY_ALL" + "\t" + "ROW_ALL" + "\t" + "COLUMN_ALL" + "\t" + "MEAN_ALL" + "\t" + "STD_ALL\tMIN_ALL\tMAX_ALL" +  "\n")
	outFile.close()	
	
for model in modellist[int(mod1):int(mod2)]:

	for period in periodlist:
	
		print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		print "     Describe " + " " + rcp + " "  + str(model) + " "  + str(period) +" countModel:"+str(mod1)
		print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
		
		#Set workspace
		if(type=="ensemble"):
			# env.workspace = dirbase + "\\ensemble\\" + rcp + "\\global_" + res + "\\" + period
			workspace= dirbase + "\\downscaled\\ensemble\\" + rcp + "\\global_" + res + "\\" + period
		if(type=="downscaled"): 
			# env.workspace = dirbase + "\\" + rcp + "\\global_" + res + "\\" + model + "\\r1i1p1\\" + period			
			workspace = dirbase + "\\downscaled\\" + rcp + "\\global_" + res + "\\" + model + "\\r1i1p1\\" + period			
		if(type=="interpolations"): 
			workspace = dirbase + "\\interpolations\\" + rcp + "\\" + model + "\\r1i1p1\\" + period		
		
		
		# rasters = sorted(arcpy.ListRasters("*", "GRID"))
		# for raster in rasters:
		
		if variable == "ALL":
			var = "*"
		else:	
			var = variable + "*"
			
		for variable in variablelist:
			if variable == "bio":
				num = 19
			elif variable == "cons_mths":
				num = 1
			else:
				num = 12
			for month in range (1, num + 1, 1):
				if variable == "cons_mths":
					raster= variable
					dirRaster= workspace+"\\"+ raster
				else:
					raster = variable + "_" + str(month)		
					dirRaster= workspace+"\\"+raster
					
				if arcpy.Exists(dirRaster):
					descfile = outdir +"\\"+ rcp + "_" + model + "_" +period + "_" +raster +"_describe.txt"
					if not os.path.exists(descfile):
						# try:
						######### extent=my_raster.extent
						my_raster = arcpy.Raster(dirRaster)
						size=my_raster.uncompressedSize		
						column = my_raster.width
						ROWC = my_raster.height
						EXTENT = my_raster.extent
						LEF = str(EXTENT).split(" ")[0]
						BOT = str(EXTENT).split(" ")[1]
						RIG = str(EXTENT).split(" ")[2]
						TOP = str(EXTENT).split(" ")[3]
						
						MIN = my_raster.minimum
						MAX = my_raster.maximum
						STD = my_raster.standardDeviation
						CEX = my_raster.meanCellHeight
						CEY = my_raster.meanCellWidth
						MEAN = my_raster.mean
					
						outFile = open(descfileMerge, "a")
						outFile.write(model + "\t" +period + "\t" +raster+ "\t" + str(size) + "\t" + str(TOP) + "\t" + str(LEF) + "\t" + str(RIG) + "\t" + str(BOT) + "\t" + str(CEX) + "\t" + str(CEY) + "\t" + str(ROWC) + "\t" + str(column) + "\t" + str(MEAN) + "\t" + str(STD)  + "\t" + str(MIN)  + "\t" + str(MAX) + "\n")
						print raster,'done!'
						# except:
							# print "Error file:",descfile
							# outFile = open(descfileMerge, "a")
							# outFile.write(rcp + "\t" + model + "\t" + period + "\t" + raster + "\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\n")
							# outFile.close()	
					else:
						print "...Exist: "+rcp + "_" + model + "_" +period + "_" +raster +"_describe.dbf"
						###### rows = arcpy.SearchCursor(descfile, "", "", "ID; AREA; MIN; MAX; MEAN; STD; MODELS; PERIODS; VARIABLES", "NAME_1 A") 
							

				else:
					print " No exist:",dirRaster
					checkTXT = open(checkFile, "a")
					checkTXT.write("No existe: "+dirRaster+"\n")
				
## Merge tables DBF
# ncList = sorted(glob.glob(outdir + "\\*.dbf"))
# arcpy.Merge_management(';'.join(ncList), outdir + '\\ZonalStatistics_'+ rcp +'_merge.dbf')
# print "\n\t Merge done"
	
print "\n Process done!!"  