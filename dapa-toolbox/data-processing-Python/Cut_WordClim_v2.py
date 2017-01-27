# Name: ExtractByMask_Ex_02.py
# Description: Extracts the cells of a raster that correspond with the areas
#    defined by a mask.
# Requirements: Spatial Analyst Extension

# python D:\dapa-climate-change\dapa-toolbox\data-processing-Python\Cut_WordClim_v2.py D:\Request\Request_jramirez\Cauca\wcl

# Import system modules
import arcpy, os, sys
from arcpy import env
from arcpy.sa import *

diroutraster = sys.argv[1]
variable = sys.argv[2]

if variable == "ALL":
	variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")

# Set environment settings
env.workspace = "S:/observed/gridded_products/worldclim/Global_30s_v2"
inMaskData = "D:/Request/Request_jramirez/Cauca/mask_cauca"
			
for variable in variablelist:
	for month in range (1, 12 + 1, 1):
		if variable == "cons_mths":
			raster = env.workspace + "\\" + variable+".tif"
		else:
			raster = env.workspace + "\\" + variable + "_" + str(month)+".tif"
					
			# Set local variables
			inRaster = raster

			OutRaster = diroutraster + "\\" + os.path.basename(raster)
				
			if not arcpy.Exists(OutRaster):
				# Check out the ArcGIS Spatial Analyst extension license
				arcpy.CheckOutExtension("Spatial")

				# Execute ExtractByMask
				outExtractByMask = ExtractByMask(inRaster, inMaskData)

				# Save the output 
				outExtractByMask.save(OutRaster)
