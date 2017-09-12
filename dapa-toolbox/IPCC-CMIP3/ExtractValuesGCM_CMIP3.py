# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: September 13th, 2010
# Purpose: Extraction by mask of points, downscaled, diseggregated or interpolated GCM data.
# Note: If process is interrupted, you must be erase the last processed period.
#		If you choose interpolation dataset, and requires tmean variable, it is necessary to include tmax and tmin.
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob, shutil

gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 10:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <Extract_MaskGCM.py> <dirbase> <dirout> <mask> <dataset> <sres> <resolution> <models> <periods> <variable>"
	print "   - ie:	python ExtractValuesGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3 D:\Workspace\output D:\Workspace\Pluviometros.shp downscaled A2 5min bccr_bcm2_0,cccma_cgcm3_1_t47 2020_2049,2040_2069 prec"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   dirout	: Is the folder where the tables (Dbf) are created"	
	print "	  mask		: shape with full path and extension"
	print "   dataset	: The possibilities are: Downscaled, Disaggregated, interpolations, and anomalies dataset"
	print "   sres		: IPCC Emission Escenario. The possibilities are a1b, a2, b1"
	print "   resolution: Units Resolution in arcminutes. The possibilities are 30s 2_5min 5min 10min"
	print "	  models	: Global Climate Models. If you want to choose some models, only separated with commas without spaces. Use 'ALL' to choose all available models"
	print "   periods	: Future 30yr periods. If you want to choose some periods, enter them separated by commas without spaces. E.g.: 2010_2039,2020_2049,2030_2059. Use 'ALL' to process all the periods"
	print "   variable	: Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write 'PREC'. Use 'ALL' to convert all data in the workspace. The possibilities are: prec, tmin, tmax, tmean"
	
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
variable= sys.argv[9]

# Clear screen
os.system('cls')

if dataset == 'downscaled' or dataset == 'disaggregated': #or 'ensemble':
	#Get lists of periods
	if periods == "ALL":
		periodlist = '2010_2039', '2020_2049', '2030_2059', '2040_2069', '2050_2079', '2060_2089', '2070_2099'
	
	else:
		periodlist = periods.split(",") 

	#Get lists of models
	if models == "ALL":
		modellist = sorted(os.listdir(dirbase + "\\" + dataset + "\\SRES_" + sres + "\\Global_" + str(resolution) ))

	else: 
		modellist = models.split(",")

	#Get lists of data
	if variable == "ALL":
		variablelist = ["bio","prec","tmin","tmax","tmean" ]
	else:
		variablelist = variable.split(",")

	gp.AddMessage("Models: " + str(modellist))
	gp.AddMessage( "Periods: " + str(periodlist) )		
	gp.AddMessage( "Variables: " + str(variablelist))		
		
	for model in modellist:
		for period in periodlist:
		
			print "\n---> Processing: " + dataset + " " + "SRES_" + sres + " " + model + " " + period + "\n"
			# Define outputs
			diroutpoints = dirout + '\\'+ dataset
			if not os.path.exists(diroutpoints):
				os.system('mkdir ' + diroutpoints)	
			
			if not os.path.exists(diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-done.txt"):
			
				for variable in variablelist:
				
					for month in range (1, 12 + 1, 1):
						
						if dataset == 'ensemble':
							gp.workspace = dirbase + "\\" + "\\downscaled\\"+ dataset + "\\SRES_" + sres + "\\Global_" + str(resolution)+ "\\" + period
						else:
							gp.workspace = dirbase + "\\" + dataset + "\\SRES_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
							
						raster = gp.workspace + "\\" + variable + "_" + str(month)
						
						InPointsFC = mask 
						OutPointsFC = diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-" + os.path.basename(raster) + ".dbf"
						
						if not os.path.exists(OutPointsFC):

							gp.AddMessage( "\tExtracting .. " + os.path.basename(raster) )
							#Check out Spatial Analyst extension license
							gp.CheckOutExtension("Spatial")

							gp.Sample_sa(raster, InPointsFC, OutPointsFC, "")
							
						else:
							print "\t" + os.path.basename(raster) + " extracted"
					
					for month in range (1, 12 + 1, 1):
						dbf = diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-" + variable + "_" + str(month) + ".dbf"
						
						if not variable + "-" + str(month) + ".dbf" == variable + "-1.dbf":
						
							InData = diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-" + variable + "_1.dbf"
			
							print "\n--->Joining .. " + os.path.basename(dbf) + "\n"
							gp.joinfield (InData, "OID", dbf, "OID", variable + "_" + str(month))
							print "\n--->remove file: " + os.path.basename(dbf) + "\n"
							os.remove(dbf)

					# Add column model to table tmean
					InData = diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-" + variable + "_1.dbf"
					print "\n--->adicionando columna models: " + os.path.basename(InData) + "\n"
					gp.addfield (InData, "models", "text", "", "", "30")
					result = gp.GetCount_management(InData)
					count = int(result.GetOutput(0))
					for line in range(0, count): 
						rows = gp.UpdateCursor(InData, "OID ="+ str(line))
						for row in rows:
							print "...actualizando columna models: ", model
							row.models = model
							rows.updateRow(row)
						del rows

				xmlList = sorted(glob.glob(diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "*.xml"))
				for xml in xmlList:
					os.remove(xml)
				
				checkTXT = open(diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-done.txt", "w")
				checkTXT.close()

	# Merge tables DBF
	print "\n...processing MERGE dbf\n"
	os.chdir(diroutpoints)
	dbfList = glob.glob("*.dbf")

	for dbf in dbfList:
		if os.path.exists(dirout + '\\'+ dataset+'\\'+dbf.split("-")[3] +"_" + dbf.split("-")[4].split(".")[0] +'_proc.txt'):
			os.remove(dirout + '\\'+ dataset+'\\'+dbf.split("-")[3] +"_" + dbf.split("-")[4].split(".")[0] +'_proc.txt')

	for dbf in dbfList:
		outFile = open(dirout + '\\'+ dataset+'\\'+dbf.split("-")[3] +"_" + dbf.split("-")[4].split(".")[0] +'_proc.txt', 'a') 
		outFile.write(diroutpoints + "\\" + dbf + ';' )
		outFile.close()
		

	for period in periodlist:
		for variable in variablelist:
			l=open(dirout + '\\'+ dataset+'\\'+ period + "_" + variable +'_1_proc.txt')
			lines = [i for i in l.readlines()]
			gp.OverwriteOutput = 1
			gp.merge_management(lines[0], dirout + '\\'+ dataset+'\\'+ period + "_" + variable +'_DONE.dbf')
	
	l.close()
	# Remove trash files
	os.system( "del " + diroutpoints + "\log")
	os.system( "del " + diroutpoints + "\*.txt")
	os.system( "del " + diroutpoints + "\*.xml")
	os.system( "del " + diroutpoints + "\SRES-*")

else: ###============= ANOMALIES Y INTERPOLATION

	print "~~~~~~~~~~~~~~~~~~~~~~"
	print " EXTRACT BY MASK GCM  "
	print "~~~~~~~~~~~~~~~~~~~~~~"
	#Get lists of periods
	if periods == "ALL":
		periodlist = '2010_2039', '2020_2049', '2030_2059', '2040_2069', '2050_2079', '2060_2089', '2070_2099'
	
	else:
		periodlist = periods.split(",") 
		print "Available periods: " + str(periodlist)	
	#Get lists of models
	if models == "ALL":
		modellist = sorted(os.listdir(dirbase + "\\"  + dataset + "\\SRES_" + sres))
		print "Available models: " + str(modellist)
	else: 

		modellist = models.split(",")
		print "Available models: " + str(modellist)	
	#Get lists of data	
	if variable == "ALL":
		variable = "prec,tmin,tmax,tmean"
		variab = variable.split(",")
		variablelist = [ v for v in variab if not v.startswith('tmean') ]
		print "Available variables: " + str(variab)	
		
	else:
		variab = variable.split(",")
		variablelist = [ v for v in variab if not v.startswith('tmean') ]
		print "Available variables: " + str(variab)	
	# Looping around models and periods
	for model in modellist:
		for period in periodlist:
		
			print "\n---> Processing: " + "SRES_" + sres + " " + model + " " + period + "\n"
			diroutpoints = dirout + '\\'+ dataset
			if not os.path.exists(diroutpoints):
				os.system('mkdir ' + diroutpoints)	
			
			if not os.path.exists(diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-done.txt"):
				for variable in variablelist:
				
					for month in range (1, 12 + 1, 1):
						#Create output directories
						gp.workspace = dirbase + "\\" + dataset + "\\SRES_" + sres + "\\" + model + "\\" + period
						raster = gp.workspace + "\\" + variable + "_" + str(month)
						
						InPointsFC = mask 
						OutPointsFC = diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-" + os.path.basename(raster) + ".dbf"
						
						if not os.path.exists(OutPointsFC):

							gp.AddMessage( "\tExtracting .. " + os.path.basename(raster) )
							
							#Check out Spatial Analyst extension license
							gp.CheckOutExtension("Spatial")

							gp.Sample_sa(raster, InPointsFC, OutPointsFC, "")
							
						else:
							print "\t" + os.path.basename(raster) + " extracted"
					
					for month in range (1, 12 + 1, 1):
						#Create output directories
						dbf = diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-" + variable + "_" + str(month) + ".dbf"
						
						if not variable + "-" + str(month) + ".dbf" == variable + "-1.dbf":
						
							InData = diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-" + variable + "_1.dbf"
			
							print "\n--->Joining .. " + os.path.basename(dbf) + "\n"
							gp.joinfield (InData, "OID", dbf, "OID", variable + "_" + str(month))
							print "\n--->remove file: " + os.path.basename(dbf) + "\n"
							os.remove(dbf)

					# Add column model
					InData = diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-" + variable + "_1.dbf"
					print "\n--->adicionando columna models: " + os.path.basename(InData) + "\n"
					gp.addfield (InData, "models", "text", "", "", "30")
					result = gp.GetCount_management(InData)
					count = int(result.GetOutput(0))
					for line in range(0, count): 
						rows = gp.UpdateCursor(InData, "OID ="+ str(line)) #####str(line)
						for row in rows:
							print "...actualizando columna models: ", model
							row.models = model
							rows.updateRow(row)
						del rows

				xmlList = sorted(glob.glob(diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "*.xml"))
				for xml in xmlList:
					os.remove(xml)
				
				checkTXT = open(diroutpoints + "\\SRES-" + sres + "-" + model + "-" + period + "-done.txt", "w")
				checkTXT.close()

	# Merge tables DBF
	print "\n...processing MERGE dbf\n"
	os.chdir(diroutpoints)
	dbfList = glob.glob("*.dbf")

	for dbf in dbfList:
		if os.path.exists(dirout+'\\'+ dataset+'\\'+dbf.split("-")[3] +"_" + dbf.split("-")[4].split(".")[0] +'_proc.txt'):
			os.remove(dirout+'\\'+ dataset+'\\'+dbf.split("-")[3] +"_" + dbf.split("-")[4].split(".")[0] +'_proc.txt')

	for dbf in dbfList:
		outFile = open(dirout+'\\'+ dataset+'\\'+dbf.split("-")[3] +"_" + dbf.split("-")[4].split(".")[0] +'_proc.txt', 'a') 
		outFile.write(diroutpoints + "\\" + dbf + ';' )
		outFile.close()
		

	for period in periodlist:
		for variable in variablelist:
			l=open(dirout+'\\'+ dataset+'\\'+ period + "_" + variable +'_1_proc.txt')
			lines = [i for i in l.readlines()]
			gp.OverwriteOutput = 1
			gp.merge_management(lines[0], dirout+'\\'+ dataset+'\\'+ period + "_" + variable +'_1_proc.dbf')
		
		for variable in variable.split(","):
			if variable == 'tmean':
				if os.path.exists(dirout+'\\'+ dataset+'\\'+ period + '_tmax_1_proc.dbf') and os.path.exists(dirout+'\\'+ dataset+'\\'+ period + '_tmin_1_proc.dbf'):
					# Creates and copy field to table tmean
					gp.OverwriteOutput = 1 
					gp.workspace = dirout+'\\'+ dataset
					tabla_input = period + '_tmax_1_proc.dbf'
					tabla_output = period + '_tmean_1_proc.dbf'
					gp.copyrows(tabla_input, tabla_output)
					gp.deletefield (tabla_output, 'models')

	for variable in variable.split(","):
		if variable == 'tmean':
		
			if os.path.exists(dirout+'\\'+ dataset+'\\'+ period + '_tmean_1_proc.dbf'):	
				# Join tmin and tmax to tmean
				print "---Join tmin and tmax "
				os.chdir(dirout+'\\'+ dataset)
				dbfList = glob.glob("*.dbf")
				for dbf in dbfList:
					# if not os.path.exists(dirout+'\\'+ dataset + "\\" + dbf[0:9] + "_tmin_1_proc.dbf"):	
					join_table = dirout +'\\'+ dataset + "\\" + dbf[0:9] + "_tmin_1_proc.dbf"
					if dbf[0:9] + "_" + dbf.split("_")[2] +'_1_proc.dbf' == dbf[0:9] + "_tmean_1_proc.dbf":
						InData = dirout +'\\'+ dataset + "\\" + dbf
						print dbf
						for month in range (1, 12 + 1, 1):
							gp.joinfield (InData, "OID", join_table, "OID",  "tmin_" + str(month))
					
						for month_f in range (1, 12 + 1, 1):
					
							field_name = "tmean_" + str(month_f)
							
							gp.addfield (InData, field_name, "DOUBLE", "10", "1", "10")
							
						result = gp.GetCount_management(dbfList[0])
						count = int(result.GetOutput(0))

						for month_f in range (1, 12 + 1, 1):

							field = "tmean_" + str(month_f)
							tmax_f = "tmax_" + str(month_f)
							tmin_f = "tmin_" + str(month_f)

							for line in range(0, count):

								rows = gp.UpdateCursor(InData, "OID ="+ str(line))
								row = rows.Next()
								while row:
									Length = row.GetValue(field)
									tmin = row.GetValue(tmin_f)
									tmax = row.GetValue(tmax_f)
									# Calculate operation tmean
									row.SetValue(field, (float(tmax) + tmin )/2 )
									rows.UpdateRow(row)
									row = rows.Next()
									
							gp.deletefield (InData, tmax_f + ';' + tmin_f )				
							
					   # Join column model to tmin
						gp.joinfield (InData, "OID", join_table, "OID",  "models" )
	
	l.close()
	# Remove trash files
	os.system( "del " + diroutpoints + "\log")
	os.system( "del " + diroutpoints + "\*.txt")
	os.system( "del " + diroutpoints + "\*.xml")
	os.system( "del " + diroutpoints + "\SRES-*")
		
gp.AddMessage("\n \t ====> DONE!! <====")  