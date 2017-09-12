#--------------------------------------------------
# Description: Extract CRU by points or by mask
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Purpouse: Extract data by points or by mask from CRU-TS3.1 grids. 
# Date: 18/03/2011
#--------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)
# python D:\jetarapues\Request\Request_cenavarro\ExtractWordclim.py S:\observed\gridded_products\worldclim\Global_30s D:\jetarapues\Request\Request_cenavarro D:\jetarapues\Request\Request_cenavarro\mask\points.shp ALL 1


os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python ExtractCRU_v3_1.py <dirbase> <dirout> <mask> <startyear> <endyear> <variable> <mode>"
	print "   - ex: python ExtractCRU_v3_1.py \\dapadfs\data_cluster_4\observed\gridded_products\cru-ts-v3-1\monthly_grids D:\Workspace\Output D:\Workspace\mask 2005 2009 ALL 1"
	print " dirbase	: Is the folder where your ESRI-Grid CRU data are located"
	print " dirout	: Is the folder where the output files are created (ESRI-Grid files or DBF files)"
	print " mask	: Is the folder where your ESRI-Grid CRU data are located"	
	print " startyear	: Start year of extraction"
	print " endyear	: End year of extraction"
	print " mode	:"
	print " variable: Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write 'PREC'. Use 'ALL' to convert all data in the workspace. The possibilities are: tmn(daily minimun temperature), tmx(daily maxmium temperature), pre(precipitation), tmp(daily mean temperature)"
	print		"1. Extract by points and groups by variables"
	print		"2. Extract by mask"
	print		"3. Extract by points and group for MarkSim "
	
	
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
# startyear = int(sys.argv[4])
# endyear = int(sys.argv[5])
variable= sys.argv[4]
mode = sys.argv[5]


#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Create output folder
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)	

	
# Set variables
if variable == "ALL":
	variablelist = ["prec", "tmin", "tmax", "tmean"]
else:
	variablelist = variable.split(",")	
	


print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     EXTRACT CRU v3.1      "
print "~~~~~~~~~~~~~~~~~~~~~~~~~\n"

if mode == "1":
	
	print "\n~~~~ Extract by points and groups by variables ~~~~\n"
	
	# for year in range (startyear, endyear + 1, 1):
		
	for variable in variablelist:
		for month in range (1, 12 + 1, 1):
			
			# Set workspace
			gp.workspace = dirbase
			
			# Define inputs and outputs
			InRaster = variable + "_" + str(month)		
			OutPoints = dirout + "\\" + variable + "_" + str(month) + ".dbf"
			
			if not os.path.exists(OutPoints): 	
				print OutPoints
				gp.Sample_sa(InRaster, mask, OutPoints, "")
				
				
				
				################ if str(month)  == "10" or str(month) == "11" or str(month) == "12":
				# field = variable+"_"+str(month)
				# gp.AddField(OutPoints,field,"DOUBLE", "10", "1", "10")
				
				# result = gp.GetCount_management(mask)
				# count = int(result.GetOutput(0))
				####### if str(month)  == "10" or str(month) == "11" or str(month) == "12":
				# for line in range(0, count):
					# rows = gp.UpdateCursor(OutPoints, "OID ="+ str(line))
					# row = rows.Next()
					# while row:
						# campo = row.GetValue(variable+"_"+str(month)[:-1])
						# row.SetValue(field, campo)
						# rows.UpdateRow(row)
						# row = rows.Next()
				# gp.deletefield (OutPoints, variable+"_"+str(month)[:-1])		
			gp.AddMessage( "\t"+ " " + InRaster+ " " +  "extracted" )
	
	# Join dbfs files extracted
	# print "\n .. Joining outputs"
	
		for month in range (1, 12 + 1, 1):
			dbf = dirout + "\\" + variable + "_" + str(month) + ".dbf"
			if not os.path.basename(dbf) == variable + "_1.dbf":
				InData = dirout + "\\" + variable  + "_1.dbf"
				# if str(month)  == "10" or str(month) == "11" or str(month) == "12":
					# gp.JoinField(InData, "OID", dbf, "OID", variable+str(month))
				# else:
				gp.JoinField(InData, "OID", dbf, "OID", variable  + "_" + str(month))
				gp.AddMessage( "\t" + dbf + " Joined")	
				#os.remove(dbf)
	# for variable in variablelist:
		# os.rename(dirout + "\\" + variable+ "_1.dbf", dirout + "\\" +variable + "_extracts.dbf")	
	# xmlList = sorted(glob.glob(dirout + "\\*.xml"))
	# for xml in xmlList:
		# os.remove(xml)

if mode == "2":

	print "\n~~~~ Extract by mask ~~~~\n"
	
	for year in range (startyear, endyear + 1, 1):
		
		for month in range (1, 12 + 1, 1):
		
			for variable in variablelist:
				
				# Set workspace
				gp.workspace = dirbase + "\\" + variable
				
				# Define inputs and outputs
				InRaster = variable + "_" + str(year) + "_" + str(month)
				OutRaster = dirout + "\\" + variable + "_" + str(year) + "_" + str(month)				
				
				#Check out Spatial Analyst extension license
				gp.CheckOutExtension("Spatial")

				# Extract by mask process
				gp.ExtractByMask_sa(gp.workspace + "\\" + InRaster, mask, OutRaster)
				gp.AddMessage( "\t"+ " " +  InRaster+ " " +  "extracted" )
				
if mode == "3":

	print "\n~~~~ Extract by points and group for marksim ~~~~\n"
		
	for year in range (startyear, endyear + 1, 1):
		
		for month in range (1, 12 + 1, 1):
			
			for variable in variablelist:
				
				# Set workspace
				gp.workspace = dirbase + "\\" + variable
				
				# Define inputs and outputs
				InRaster = variable + "_" + str(year) + "_" + str(month)		
				OutPoints = dirout + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
				
				if not os.path.exists(OutPoints): 	
					# Sample function
					gp.Sample_sa(InRaster, mask, OutPoints, "")
					if str(month)  == "10" or str(month) == "11" or str(month) == "12":
						field = variable+"_"+str(year)+str(month)
						gp.AddField(OutPoints,field,"DOUBLE", "10", "1", "10")
						
						result = gp.GetCount_management(mask)
						count = int(result.GetOutput(0))
						if str(month)  == "10" or str(month) == "11" or str(month) == "12":
							for line in range(0, count):
								rows = gp.UpdateCursor(OutPoints, "OID ="+ str(line))
								row = rows.Next()
								while row:
									campo = row.GetValue(variable+"_"+str(year)+"_"+str(month)[:-1])
									row.SetValue(field, campo)
									rows.UpdateRow(row)
									row = rows.Next()
							gp.deletefield (OutPoints, variable+"_"+str(year)+"_"+str(month)[:-1])		
				gp.AddMessage( "\t"+ " " + InRaster+ " " +  "extracted" )
				
		# Join dbfs files extracted		
		print "\n .. Joining outputs"
		
		for month in range (1, 12 + 1, 1):
			for variable in variablelist:
				dbf = dirout + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
				if not os.path.basename(dbf) == variablelist[0] + "_" +  str(year) + "_1.dbf":
					InData = dirout + "\\" + variablelist[0]   + "_" + str(year) + "_1.dbf"
					if str(month)  == "10" or str(month) == "11" or str(month) == "12":
						gp.JoinField(InData, "OID", dbf, "OID", variable+"_"+str(year)+str(month))
					else:
						gp.JoinField(InData, "OID", dbf, "OID", variable + "_" + str(year) + "_" + str(month))
					gp.AddMessage( "\t" + dbf + " Joined")		
					os.remove(dbf)
		os.rename(dirout + "\\" + variablelist[0]   + "_" + str(year) + "_1.dbf", dirout + "\\extract_" + str(year) + ".dbf")	
	# Remove trash files	
	xmlList = sorted(glob.glob(dirout + "\\*.xml"))
	for xml in xmlList:
		os.remove(xml)

gp.AddMessage("\n \t ====> DONE!! <====")
