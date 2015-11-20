# -----------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Claculate zonal statistics for diseggregated, interpolated, anomalies or downscaled surfaces
# -----------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: ZonalStatisticsGCM.py <dirbase> <dirout> <scenario> <resolution> <mask>"
	print "	  - ie: python ZonalStatisticsGCM.py E:\CIAT\Projects\ecu-hidroelectrica\03_future_climate_data\cmip5\anomalies_resampled_sf D:\CIAT\Projects\ecu-hidroelectrica\04-Performance-gcm\analyses D:\CIAT\Projects\ecu-hidroelectrica\04-Performance-gcm\analyses\masks\division_cuenca.shp"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
# resolution = sys.argv[3]
mask = sys.argv[3]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "    Calculate Statistics GCM " 
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"


#Get lists of sress and models
rcplist = "rcp26", "rcp45", "rcp85"
periodlist = "2020_2049", "2040_2069", "2070_2099"

# Looping scenarios
for rcp in rcplist:

	modellist = sorted(os.listdir(dirbase  + "\\" + rcp))

	# for model in modellist:
	for model in modellist:
		
		# Looping around periods
		for period in periodlist:

			# Set workspace by each model
			gp.workspace = dirbase + "\\" + rcp + "\\" + model + "\\" + period
		
			# Define out table file (dbf extension)
			outSta = dirout + "\\" + rcp + "-" + period + ".dbf"
			
			if not gp.Exists(outSta):
				
				print "\n\t Processing: ", rcp, period, model + "\n"
				
				# Define and create output directory
				if not os.path.exists(dirout):
					os.system('mkdir ' + dirout)

				#Get a list of raster in workspace
				rasters = {"tmax_ann.asc":"ann_tmax", "tmin_ann.asc":"ann_tmin", "prec_ann.asc":"ann_prec"}
				
				for raster in rasters:
					
					# Zonal statistical function
					outDbf = dirout + "\\" + rcp + "-" + period + "-" + model + "-" + os.path.basename(rasters [raster]) + ".dbf"
					if gp.Exists(outDbf):
						os.remove(outDbf)
						gp.ZonalStatisticsAsTable_sa(mask, "FID", gp.workspace + "\\" + raster, outDbf, "DATA")
						print "\t", raster, period, "stats calculated"
					else:
						gp.ZonalStatisticsAsTable_sa(mask, "FID", gp.workspace + "\\" + raster, outDbf, "DATA")
						print "\t", raster, period, "stats calculated"
				
					if raster == "bio_12_per.asc":
						gp.deletefield (outDbf, "VARIETY; MAJORITY; MINORITY; MEDIAN")
				
					# add colummna models
					gp.addfield (outDbf, "RCP", "text", "", "", "30")
					gp.addfield (outDbf, "PERIOD", "text", "", "", "30")
					gp.addfield (outDbf, "MODEL", "text", "", "", "30")
					gp.addfield (outDbf, "VARIABLE", "text", "", "", "30")
					result = gp.GetCount_management(outDbf)
					count = int(result.GetOutput(0))
					print count
					rows = gp.UpdateCursor(outDbf) #, "FID_ =0")
					for row in rows:
						row.RCP = rcp
						row.PERIOD = period
						row.MODEL = model
						row.VARIABLE = rasters [raster]
						rows.updateRow(row)
					del rows
				

# Join dbfs files extracted
print "\n\t .. Joining outputs"
dbfList = sorted(glob.glob(dirout + "\\" + "*.dbf"))
gp.merge_management(dbfList, dirout + '\\'+ 'statistics.dbf')

for f in dbfList:
	os.remove(f)
	
# # Rename join file
# os.rename(dirout + "\\" + sres + "-" + model + "-" + period + "-bio_1.dbf", outSta)

# Delete Trash
xmlList = sorted(glob.glob(dirout + "\\*.xml"))
for xml in xmlList:
	os.remove(xml)

			
print "\n \t Process done!!"  
