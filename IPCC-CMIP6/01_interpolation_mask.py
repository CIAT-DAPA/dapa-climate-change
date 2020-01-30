import arcpy, sys, os
from arcpy import env  
from arcpy.sa import *

# python X:\ADAA\CMIP6\_scripts\01_interpolation_mask.py tasmin_12_A3.shp D:/TEMP/interpo.tif
shp = sys.argv[1]
outR= sys.argv[2]
# box =

arcpy.env.parallelProcessingFactor = "70%"

# Import system modules
import arcpy, sys, os
from arcpy import env

# Set environment settings
env.workspace = "D:/TEMP/2020_2049"

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



arcpy.Clip_management(outR, "-180 -60 0 0","C:/temp/tmin_clip2.tif",  "#", "#", "NONE","MAINTAIN_EXTENT");

# Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
# The following inputs are layers or table views: "tmin.tif"
# arcpy.Clip_management(in_raster="tmin.tif", rectangle="-180 -60.000003 0 0", out_raster="D:/TEMP/2020_2049/tmin_clip.tif", in_template_dataset="", 
# nodata_value="-3.400000e+038", clipping_geometry="NONE", maintain_clipping_extent="MAINTAIN_EXTENT")

