# Name: RasterToPolygon_Ex_02.py
# Description: Converts a raster dataset to polygon features.
# Requirements: None

# Import system modules
import arcpy
from arcpy import env

# Set environment settings
env.workspace = "D:/CIAT/Projects/ecu-hidroelectrica/02_baseline/_region"

# Set local variables
inRaster = "mask"
outPolygons = "D:/CIAT/Projects/ecu-hidroelectrica/04-Performance-gcm/Administrative_boundaries/ECU_adm/ECU_0.shp"
field = "VALUE"

# Execute RasterToPolygon
arcpy.RasterToPolygon_conversion(inRaster, outPolygons, "NO_SIMPLIFY", field)
