# ---------------------------------------------------------
# Author: Carlos Navarro
# ---------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print " Syntaxis python ZonalStatistical.py <srtm> <shpdir> <dirout>"
	print "   - ex: python ZonalStatistical.py D:\CIAT\climate_change\csa_profiles D:\CIAT\_tools\AdminBoundaries\SHP_files"
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

countrylist = "tza", "npl", "zmb", "moz", "pak", "btn", "bgd", "ben", "gmb", "civ", "phl"

for country in countrylist:

	admshp = admdir + "\\" + country + "_adm\\" + country + "1.shp"
	dissshp = dirbase + "\\" + country + "\\" + country + "1_diss.shp"

	bio1 = dirbase + "\\" + country + "\\anomalies\\rcp45\\Global_30s\\2040_2069\\bio_1" 
	bio12 = dirbase + "\\" + country + "\\anomalies\\rcp45\\Global_30s\\2040_2069\\bio_12"

	outTable_bio_1 = dirbase + "\\" + country + "\\bio1_stats.dbf"
	outTable_bio_12 = dirbase + "\\" + country + "\\bio12_stats.dbf"
		
	if not gp.Exists(outTable_bio_12):
	
		print admshp, dissshp
		gp.Dissolve_management(admshp, dissshp, "ZONES")
		
		print "\t", country, "stats calcs"
		gp.ZonalStatisticsAsTable_sa(dirbase + "\\" + country + "\\" + country + "1_diss.shp", "ZONES", bio1, outTable_bio_1, "DATA")
		gp.ZonalStatisticsAsTable_sa(dirbase + "\\" + country + "\\" + country + "1_diss.shp", "ZONES", bio12, outTable_bio_12, "DATA")
		
	
	gp.joinfield (dissshp, "ZONES", outTable_bio_1, "ZONES", "MEAN")
	gp.joinfield (dissshp, "ZONES", outTable_bio_12, "ZONES", "MEAN")
	print "\t", country, "stats joined "

print "\n\t Process done!!"