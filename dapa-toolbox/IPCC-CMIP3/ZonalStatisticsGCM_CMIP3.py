# -----------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: September 13th, 2010
# Purpose: Claculate zonal statistics for diseggregated, interpolated, anomalies or downscaled surfaces
# -----------------------------------------------------------------------------------------------------
# python D:\\_scripts\dapa\IPCC-CMIP3\\ZonalStatisticsGCM_CMIP3.py \\dapadfs\\data_cluster_4\\gcm\\cmip3 D:\\PRUEBAS_SCRIPTs\\pr1 D:\PRUEBAS_SCRIPTs\\mask\\mask_recuadro.shp downscaled b1 10min bccr_bcm2_0 2020_2049 prec
import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

# Syntax
if len(sys.argv) < 10:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <ZonalStatisticsGCM_CMIP3.py> <dirbase> <dirout> <mask> <dataset> <sres> <resolution> <models> periods> <wildcard>"
	print "	  - ie: python ZonalStatisticsGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3 D:\Workspace\output D:\Workspace\polygon.shp downscaled b1 10min bccr_bcm2_0 2020_2049 prec"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder for the statistics"
	print "   mask		: shapefile with full path and extension"	
	print "   dataset	: The possibilities are: Downscaled, Disaggregated, interpolations, and anomalies dataset"	
	print "   sres		: IPCC Emission Escenario. The possibilities are a1b, a2, b1"
	print "   resolution: Units Resolution in arcminutes. The possibilities are 30s 2_5min 5min 10min"
	print "	  models	: Global Climate Models. If you want to choose some models, only separated with commas without spaces. Use 'ALL' to choose all available models"
	print "   periods	: Future 30yr periods. If you want to choose some periods, enter them separated by commas without spaces. E.g.: 2010_2039,2020_2049,2030_2059. Use 'ALL' to process all the periods"
	print "	  wildcard  : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
dataset = sys.argv[4]
sres = sys.argv[5]
resolution = sys.argv[6]
models = sys.argv[7]
periods = sys.argv[8]
wildcard = sys.argv[9]

#### Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Calculate Statistics GCM " + " " + str(resolution) + " " + sres
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"


##### Get lists of sress and models
if models == "ALL":
	modellist = sorted(os.listdir(dirbase + "\\"  + dataset + "\\SRES_" + scenario + "\\Global_" + str(resolution) ))
else: 
	modellist = models.split(",")

#### #Get lists of sress and models
if periods == "ALL":
	periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
else:	
	periodlist = periods.split(",")
	
#Get lists of data
if wildcard == "ALL":
	variablelist = ["bio","prec","tmin","tmax","tmean" ]
else:
	variablelist = wildcard.split(",")

gp.AddMessage("Models: " + str(modellist))
gp.AddMessage( "Periods: " + str(periodlist))
gp.AddMessage( "Variables: " + str(variablelist))

if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)	

# Looping around periods
for period in periodlist:
	# for model in modellist:
	for model in modellist:
		# Get a list of grids in the workspace
		print "\t ..listing grids into " + dirbase + '\\'  + dataset
		
		# Lopping around the grids
		for variable in variablelist:
			for month in range (1, 12 + 1, 1):
				#Set output
				outTable = dirout + "\\" + sres + "-" + model + "-" + period + "-" + variable + "_" + str(month) + ".dbf"
				inValueRaster = dirbase + '\\'  + dataset + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\\" + variable + "_" + str(month)
				# Zonal Statistical as table function
				if not os.path.exists(outTable):	
					gp.AddMessage( "==>Processing: " + os.path.basename(outTable)[:-4] )
					gp.ZonalStatisticsAsTable_sa(mask, "FID", inValueRaster, outTable, "DATA")
					# add colummna models
					gp.addfield (outTable, "MODEL", "text", "", "", "30")
					gp.addfield (outTable, "VARIABLE", "text", "", "", "30")
					result = gp.GetCount_management(outTable)
					count = int(result.GetOutput(0))
					rows = gp.UpdateCursor(outTable, "FID_ =0")
					for row in rows:
						row.MODEL = model
						row.VARIABLE = variable + "_" + str(month)
						rows.updateRow(row)
					del rows	
				
for period in periodlist:
	for model in modellist:
	
		gp.workspace = dirbase + '\\'  + dataset + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + period 	
		
		# Get a list of grids in the workspace
		print "\t ..listing grids into " + gp.workspace
		if wildcard == "ALL":
			rasters = sorted(gp.ListRasters("*", "GRID"))
		else:	
			rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))	
		
		for raster in rasters:
			outTable = dirout + "\\" + sres + "-" + model + "-" + period + "-" + os.path.basename(raster) + ".dbf"
			outFile = open(dirout + '\\'+ sres + "-" + period + "-" + 'list_merge.txt', 'a') 
			outFile.write(outTable + ';' )
			outFile.close()
			l=open(dirout + '\\'+ sres + "-" + period + "-" + 'list_merge.txt')
			lines = [i for i in l.readlines()]
			gp.OverwriteOutput = 1
			gp.merge_management(lines[0], dirout + '\\'+ sres + "-" + period + "-" + 'ZonalStatisticsAsTable.dbf')
			
# Delete file dbf
for period in periodlist:
	for model in modellist:	
		if wildcard == "ALL":
			rasters = sorted(gp.ListRasters("*", "GRID"))
		else:	
			rasters = sorted(gp.ListRasters(wildcard + "*", "GRID"))	
	
		for raster in rasters:
			outTable = dirout + "\\" + sres + "-" + model + "-" + period + "-" + os.path.basename(raster) + ".dbf"			
			l.close()
			# Remove trash files
			os.remove(outTable)
			os.remove(outTable+'.xml')
			
# Remove trash files
for period in periodlist:		
	os.remove(dirout + '\\'+ sres + "-" + period + "-" + 'ZonalStatisticsAsTable.dbf.xml')
	os.remove(dirout + '\\'+ sres + "-" + period + "-" + 'list_merge.txt')
		
gp.AddMessage("\n \t ====> DONE!! <====")  