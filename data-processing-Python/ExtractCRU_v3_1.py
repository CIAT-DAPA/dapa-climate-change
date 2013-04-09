#--------------------------------------------------
# Description: Extract CRU by points or by mask
# Author: Carlos Navarro
# Date: 18/03/2011
#--------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~"
print "   EXTRACT VALUES   "
print "~~~~~~~~~~~~~~~~~~~~"
print "\n"

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ExtractCRU_v3_1.py \\dapadfs\data_cluster_4\observed\gridded_products\cru-ts-v3-1\monthly_grids D:\Workspace\_mask\mask D:\Workspace 2005 2009 1"
	print " 	Syntaxis python ExtractCRU_v3_1.py <dirbase> <mask> <dirout> <start year> <end year> <mode>"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
mask = sys.argv[2]
dirout = sys.argv[3]
inityear = int(sys.argv[4])
finalyear = int(sys.argv[5])
mode = sys.argv[6]

# Modes: 	1. Extract by points and groups by variables
# 			2. Extract by mask
#			3. Extract by points and group for marksim

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Create output folder
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)	

# Set variables
variablelist = "tmn", "tmx", "pre", "tmp"

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     EXTRACT CRU v3.1      "
print "~~~~~~~~~~~~~~~~~~~~~~~~~\n"

if mode == "1":
	
	print "\n~~~~ Extract by points and groups by variables ~~~~\n"
	for year in range (inityear, finalyear + 1, 1):
		
		for variable in variablelist:
			
			for month in range (1, 12 + 1, 1):
				
				# Set workspace
				gp.workspace = dirbase + "\\" + variable
				
				# Define inputs and outputs
				InRaster = variable + "_" + str(year) + "_" + str(month)		
				OutPoints = dirout + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
				
				# Sample Process
				gp.Sample_sa(InRaster, mask, OutPoints, "")
				print InRaster, "extracted"
		
	# Join dbfs files extracted
	print "\n .. Joining outputs"
	for variable in variablelist:
		
		dbflist = sorted(glob.glob(dirout + "\\" + variable + "*.dbf"))
		for dbf in dbflist:
			
			if not os.path.basename(dbf) == variable + "_" + str(inityear) + "_1.dbf":
				
				InData = dirout + "\\" + variable + "_" + str(inityear) + "_1.dbf"
				fields = os.path.basename(dbf)[:-4].split("_")
				if fields[2] == "10" or fields[2] == "11" or fields[2] == "12":
					gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2][:-1])
				else:
					gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2])
				
				os.remove(dbf)
	
		os.rename(dirout + "\\" + variable + "_" + str(inityear) + "_1.dbf", dirout + "\\" + variable + "_extracts.dbf")
		
	xmlList = sorted(glob.glob(dirout + "\\*.xml"))
	for xml in xmlList:
		os.remove(xml)

if mode == "2":

	print "\n~~~~ Extract by mask ~~~~\n"
	
	for year in range (inityear, finalyear + 1, 1):
		
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
				print InRaster, "extracted"
				
if mode == "3":

	print "\n~~~~ Extract by points and group for marksim ~~~~\n"
		
	for year in range (inityear, finalyear + 1, 1):
		
		for month in range (1, 12 + 1, 1):
			
			for variable in variablelist:

				# Set workspace
				gp.workspace = dirbase + "\\" + variable
				
				# Define inputs and outputs
				InRaster = variable + "_" + str(year) + "_" + str(month)		
				OutPoints = dirout + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
				
				#Process: Cell Statistics...
				print InRaster, mask, OutPoints
				gp.Sample_sa(InRaster, mask, OutPoints, "")

		# Join dbfs files extracted
		print "\n .. Joining outputs"
		
		for month in range (1, 12 + 1, 1):
			
			for variable in variablelist:
				
				if not variable == "tmp":
					
					dbf = dirout + "\\" + variable + "_" + str(year) + "_" + str(month) + ".dbf"
					if not os.path.basename(dbf) == "tmn_" + str(year) + "_1.dbf":
						
						InData = dirout + "\\tmn_" + str(year) + "_1.dbf"
						fields = os.path.basename(dbf)[:-4].split("_")
						if fields[2] == "10" or fields[2] == "11" or fields[2] == "12":
							gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2][:-1])
						else:
							gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1] + "_" + fields[2])
						
						os.remove(dbf)
	
		os.rename(dirout + "\\tmn_" + str(year) + "_1.dbf", dirout + "\\extract_" + str(year) + ".dbf")
		
	xmlList = sorted(glob.glob(dirout + "\\*.xml"))
	for xml in xmlList:
		os.remove(xml)
		
print "\n\t Process done"
print "\n"
