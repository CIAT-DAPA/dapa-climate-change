# Name: Select_Example2.py
# Description: Select roads of Class 4 from major roads tin the gnatcatcher habitat study area

# Import system modules
import arcpy
from arcpy import env

# Set workspace
env.workspace = "D:\Workspace\msc_gis_thesis\00_maps"

# Set local variables
in_features = "gadm36_0_camexca.shp"
ctr_list = "BHS", "BLZ", "CRI", "CUB", "DOM", "GTM", "HND", "HTI", "JAM", "MEX", "NIC", "PAN", "PRI", "SLV"  

for ctr in ctr_list:

    out_feature_class = "\\by_country\\", ctr, "_adm0.shp"
    where_clause = "GID_0 = ", ctr 

    # Execute Select
    arcpy.Select_analysis(in_features, out_feature_class, where_clause)