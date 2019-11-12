# ---------------------------------------------------------
# Author: Carlos Navarro
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python ZonalStatistical.py <srtm> <shpdir> <dirout>"
	print "   - ex: python 03-ZonalStatistical_subnational.py D:\Workspace\csa_profiles D:\Workspace\csa_profiles"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
admdir = sys.argv[2]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Clear screen
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     ZONAL STATS           "
print "~~~~~~~~~~~~~~~~~~~~~~~~~\n"

# countrylist = "mwi"
countrylist = "sin", "zzz"
for country in countrylist:

	admshp = admdir + "\\" + country + "\\" + country + "_adm\\" + country + "1.shp"
	dissshp = dirbase + "\\" + country + "\\" + country + "1_diss.shp"

	bio1 = dirbase + "\\" + country + "\\anomalies\\rcp45\\Global_30s\\2040_2069\\bio_1" 
	bio12 = dirbase + "\\" + country + "\\anomalies\\rcp45\\Global_30s\\2040_2069\\bio_12"

	outTable_bio_1 = dirbase + "\\" + country + "\\bio1_stats.dbf"
	outTable_bio_12 = dirbase + "\\" + country + "\\bio12_stats.dbf"
		
	if not gp.Exists(outTable_bio_12):

		print admshp, dissshp
		gp.Dissolve_management(admshp, dissshp, "REGION")
		
		print "\t", country, "stats calcs"
		gp.ZonalStatisticsAsTable_sa(dirbase + "\\" + country + "\\" + country + "1_diss.shp", "REGION", bio1, outTable_bio_1, "DATA")
		gp.ZonalStatisticsAsTable_sa(dirbase + "\\" + country + "\\" + country + "1_diss.shp", "REGION", bio12, outTable_bio_12, "DATA")
		

	gp.joinfield (dissshp, "REGION", outTable_bio_1, "REGION", "MEAN")
	gp.joinfield (dissshp, "REGION", outTable_bio_12, "REGION", "MEAN")
	print "\t", country, "stats joined "

print "\n\t Process done!!"