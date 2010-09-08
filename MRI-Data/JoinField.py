# ----------------------------------------------------------------------------------------------
# Description : Join in a single database (.dbf) the extracted data by months of the MRI values,
# The output data is a anual shapefile with 12 columns corresponding to each month, 
# with sintaxis "var_YYYY_MM".
# Author : Carlos Navarro
#------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Join_Field.py <dirbase> <inityear> <finalyear> <outdir>"
	print "   - ie: python JoinField.py E:\MRI_Analysis\Extracted\tmean_monthly 1979 2003 tmean"
	sys.exit(1)

dirbase =sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
variable = sys.argv[4]

os.system('cls')

gp.workspace = dirbase 

print "~~~~~~~~~~~~~~~~~~~~"
print "     JOIN FIELDS    "
print "~~~~~~~~~~~~~~~~~~~~"

for year in range(inityear, finalyear + 1, 1):

	for month in range (2, 12 + 1, 1):

		if month < 10:
			InFeature = variable + "_" + str(year) + "0" + str(month) + ".shp"
			InData = dirbase + "\\" + variable + "_" + str(year) + "01.shp"
			print "---> Joining " + variable + "_" + str(year)
			gp.joinfield (InData, "STATION_ID", InFeature, "STATION_ID", "RASTERVALU")
			print InFeature + " Joined!"

		else:
			InFeature = variable + "_" + str(year) + str(month) + ".shp"
			InData = dirbase + "\\" + variable + "_" + str(year) + "01.shp"
			print "---> Joining " + variable + "_" + str(year)
			gp.joinfield (InData, "STATION_ID", InFeature, "STATION_ID", "RASTERVALU")
			print InFeature + " Joined!"

print "Done!!"
