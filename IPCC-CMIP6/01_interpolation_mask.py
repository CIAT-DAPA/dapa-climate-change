import arcpy, sys, os, datetime
from arcpy import env  
from arcpy.sa import *

# python X:\ADAA\CMIP6\_scripts\01_interpolation_mask.py tasmin_12_A3.shp D:/TEMP/interpo.tif "-180 -60 0 0"
shp = sys.argv[1]
outR= sys.argv[2]
box= sys.argv[3]
clipAnom1km=sys.argv[4]

now = datetime.datetime.now() 
# print box.split(",")
# sys.exit(0)

#procesos en paralelo 
arcpy.env.parallelProcessingFactor = "70%"

# Set environment settings
env.workspace = os.path.dirname(shp)

# Set local variables
inPointFeatures = shp
zField = "VALUE"
outRaster = outR
cellSize = 0.00833333376842104
splineType = "REGULARIZED"
weight = 0.5
number_points=8

# Check out the ArcGIS 3D Analyst extension license
arcpy.CheckOutExtension("3D")

# Execute Spline
if not os.path.exists(outRaster):
	arcpy.Spline_3d(inPointFeatures, zField, outRaster, cellSize, 
					splineType, weight,number_points);

# Corta al extent
print "Processing clip..\n"
if not os.path.exists(clipAnom1km):
	arcpy.Clip_management(outR," ".join(box.split(",")),clipAnom1km,  "#", "#", "NONE","MAINTAIN_EXTENT");
	
	
	
	
later = datetime.datetime.now() 
elapsed = later - now  
print elapsed  
# Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
# The following inputs are layers or table views: "tmin.tif"
# arcpy.Clip_management(in_raster="tmin.tif", rectangle="-180 -60.000003 0 0", out_raster="D:/TEMP/2020_2049/tmin_clip.tif", in_template_dataset="", 
# nodata_value="-3.400000e+038", clipping_geometry="NONE", maintain_clipping_extent="MAINTAIN_EXTENT")

