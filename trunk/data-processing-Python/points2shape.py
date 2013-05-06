# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: April 23th, 2013
# ----------------------------------------------------------------------------------

import arcpy, os, sys, string
from arcpy import env

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python points2shape.py D:\CIAT\Portals\CCAFS_Climate\Tiles\cordex"
	sys.exit(1)

dirout = sys.argv[1]
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~"
print "    SHAPE IN TILES     "
print "~~~~~~~~~~~~~~~~~~~~~~~~"

### Global (Downscaled, disaggregated)
# latDc = {"A": 30, "B": -30, "C": -90}
# lonDc = {"1": -180, "2": -120, "3": -60, "4": 0, "5": 60, "6": 120}

# list = ""

# for lat in latDc:
	
	# for lon in lonDc:
		
		# print str(lat) + str(lon)
		
		# outTxt = dirout + "\\" + str(lat) + str(lon) + ".txt"
		# outLayer = dirout + "\\" + str(lat) + str(lon) + ".lyr"
		# outPtsShp = dirout + "\\" + str(lat) + str(lon) + "_pts.shp"
		# outLinShp = dirout + "\\" + str(lat) + str(lon) + "_lin.shp"
		# outPolShp = dirout + "\\" + str(lat) + str(lon) + "_pol.shp"
		# outGlbShp = dirout + "\\global_tiled.shp"
		
		# if not os.path.exists(outPolShp):
			
			# wFile = open(outTxt, "w")
			# wFile.write("pol" + "\t" + "lat" + "\t" + "lon" + "\n")
			# wFile.write("1\t" + str(latDc [lat]) + "\t" + str(lonDc [lon]) + "\n")
			# wFile.write("2\t" + str(int(latDc [lat])+60) + "\t" + str(lonDc [lon]) + "\n")
			# wFile.write("3\t" + str(int(latDc [lat])+60) + "\t" + str(int(lonDc [lon])+60) + "\n")
			# wFile.write("4\t" + str(latDc [lat]) + "\t" + str(int(lonDc [lon])+60) + "\n")
			# wFile.close()
			
			# arcpy.MakeXYEventLayer_management(outTxt, "lon", "lat", outLayer, "", "")
			# arcpy.CopyFeatures_management(outLayer, outPtsShp)
			# arcpy.PointsToLine_management(outPtsShp, outLinShp, "", "pol", "CLOSE")
			# arcpy.FeatureToPolygon_management(outLinShp, outPolShp, "", "", "")

		# list = list + ';' + outPolShp 

# LIST = "\"" + list[1:] + "\""
# arcpy.Merge_management(LIST, outGlbShp, "")
# print "Process done!!!" 


# ### PRECIS
# outTxt = dirout + "\\precis_lat.txt"
# outLayer = dirout + "\\precis_lat.lyr"
# outPtsShp = dirout + "\\precis_lat_pts.shp"
# outLinShp = dirout + "\\precis_lat_lin.shp"
# outPolShp = dirout + "\\precis_lat.shp"

# if not os.path.exists(outPolShp):
	
	# wFile = open(outTxt, "w")
	# wFile.write("pol" + "\t" + "lat" + "\t" + "lon" + "\n")
	# wFile.write("1\t21.4\t-95.5\n")
	# wFile.write("2\t21.4\t-33.9\n")
	# wFile.write("3\t-34.1\t-33.9\n")	
	# wFile.write("4\t-34.1\t-95.5\n")
	# wFile.close()
	
	# arcpy.MakeXYEventLayer_management(outTxt, "lon", "lat", outLayer, "", "")
	# arcpy.CopyFeatures_management(outLayer, outPtsShp)
	# arcpy.PointsToLine_management(outPtsShp, outLinShp, "", "pol", "CLOSE")
	# arcpy.FeatureToPolygon_management(outLinShp, outPolShp, "", "", "")

# print "Process done!!!" 


### CORDEX
corDic = {"South America": "1\t15.4\t-94.12\n2\t15.4\t-26.64\n3\t-57.61\t-26.64\n4\t-57.61\t-94.12\n",
			"CentralAmerica": "1\t34.83\t-118.89\n2\t34.83\t-26.6\n3\t-14.42\t-26.6\n4\t-14.42\t-118.89\n",
			"NorthAmerica": "1\t75.88\t-141.68\n2\t75.88\t-52.32\n3\t19.12\t-52.32\n4\t19.12\t-141.68\n",
			"Europe": "1\t71.84\t-21.77\n2\t71.84\t44.77\n3\t27.34\t44.77\n4\t27.34\t-21.77\n",
			"Africa": "1\t42.24\t-24.64\n2\t42.24\t60.28\n3\t-45.76\t60.28\n4\t-45.76\t-24.64\n",
			"SouthAsia": "1\t45.07\t23.48\n2\t45.07\t110.47\n3\t-11.66\t110.47\n4\t-11.66\t23.48\n",
			"EastAsia": "1\t58.56\t72.18\n2\t58.56\t165.48\n3\t-14.47\t165.48\n4\t-14.47\t72.18\n",
			"CentralAsia": "1\t69.37\t42.41\n2\t69.37\t119.82\n3\t25.83\t119.82\n4\t25.83\t42.41\n",
			"Australasia": "1\t3.87\t101.41\n2\t3.87\t180\n3\t-52.36\t180\n4\t-52.36\t101.41\n",
			"Mediterranean": "1\t56.66\t-11.88\n2\t56.66\t43.41\n3\t29.39\t43.41\n4\t29.39\t-11.88\n",
			"Mena": "1\t45\t-27\n2\t45\t70\n3\t-7\t70\n4\t-7\t-27\n"}
			# "Antarctica": "1\t-63.4\t100.47\n2\t-63.4\t60.02\n3\t-68.36\t60.02\n4\t-68.36\t100.47\n",
			# "Arctic": "1\t62.56\t-86.91\n2\t62.56\t87.92\n3\t59.14\t87.92\n4\t59.14\t-86.91\n",

list = ""
for cordexDom in corDic:

	outTxt = dirout + "\\" + cordexDom + ".txt"
	outLayer = dirout + "\\" + cordexDom + ".lyr"
	outPtsShp = dirout + "\\" + cordexDom + "_pts.shp"
	outLinShp = dirout + "\\" + cordexDom + "_lin.shp"
	outPolShp = dirout + "\\" + cordexDom + ".shp"
	outGlbShp = dirout + "\\cordex.shp"
	
	if not os.path.exists(outPolShp):
		
		print cordexDom
		
		wFile = open(outTxt, "w")
		wFile.write("pol" + "\t" + "lat" + "\t" + "lon" + "\n")
		wFile.write(str(corDic [cordexDom]))
		wFile.close()
		
		arcpy.MakeXYEventLayer_management(outTxt, "lon", "lat", outLayer, "", "")
		arcpy.CopyFeatures_management(outLayer, outPtsShp)
		arcpy.PointsToLine_management(outPtsShp, outLinShp, "", "pol", "CLOSE")
		arcpy.FeatureToPolygon_management(outLinShp, outPolShp, "", "", "")
		arcpy.Delete_management(outLinShp)
		arcpy.Delete_management(outPtsShp)
		list = list + ';' + outPolShp 

LIST = "\"" + list[1:] + "\""
arcpy.Merge_management(LIST, outGlbShp, "")
print "Process done!!!" 

print "Process done!!!" 

