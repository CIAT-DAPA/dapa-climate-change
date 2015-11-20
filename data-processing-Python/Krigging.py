#----------------------------------------------------------------------------------------
# Description: Interpolate surfaces from a table with many columns
# Author: Carlos Navarro
# Date: February 13th, 2015
#----------------------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "   INTERPOLATE SURFACES   "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python Krigging.py <dirbase> <diroutfile> <points> <wildcard>"
	print "   - ex: python Krigging.py D:\CIAT\Workspace\jsalinas D:\CIAT\Workspace\jsalinas\plots"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Create output folder
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

# Set workspace
gp.workspace = dirbase 

yearlist = "2003-2004", "2005-2006", "2007-2010", "2011-2013"

for years in yearlist:
	
	dates = dirbase + "\\dates_" + years + ".txt" 
	inputFeatureDataset = dirbase + "\\" + years + ".shp" 
	
	listfile = open(dates)
	for line in listfile:
		
		#Date
		day = line.split("\t")[1][:-1]
			
		#Output file
		outputRaster = dirout + "\\" + day
		
		gp.Extent = "-76.385 3.493 -76.334 3.512"
		
		# Define the semivariogram 
		Model = "Spherical "         #(Remember the space at the end)
		LagSize = "0.000052 "          #(Remember the space at the end)
		Semivariogram = Model + LagSize 

		# Set the attribute field
		attributeName = day

		# Check out Spatial Analyst extension license
		gp.CheckOutExtension("Spatial")
				
		if not gp.Exists(outputRaster):
		
			try:
								
				print "\n\n Krigging over " + day
				
				# Process: Kriging...
				gp.Kriging_sa(inputFeatureDataset, attributeName, outputRaster, Semivariogram, "0.0001", "VARIABLE 12")
			
			except:
				# If an error occurred while running a tool, then print the messages
				print gp.GetMessages()
print "Process done!!"
