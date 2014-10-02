import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "D:\Request\Request_miguel\ensemble\rcp45\global_30s\2020_2049\col_adm\anomalies"

# Set local variables
# inRaster1 = Raster("D:\Request\Request_miguel\ensemble\rcp45\global_30s\2020_2049\col_adm\bio_1")
# inRaster2 = Raster("S:\observed\gridded_products\worldclim\Global_30s\bio_1")

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

# Execute Minus
# outMinus = inRaster1 - inRaster2

# Save the output 
# outMinus.save("D:\Request\Request_miguel\ensemble\rcp45\global_30s\2020_2049\col_adm\anomalies\bio_1")


# arcpy.RasterCalculator_sa("/\D:/Request/Request_miguel/ensemble/rcp45/global_30s/2020_2049/col_adm/bio_1/\ - /\S:/observed/gridded_products/worldclim/Global_30s/bio_1/\","D:/Request/Request_miguel/ensemble/rcp45/global_30s/2020_2049/col_adm/anomalies/bio1")
arcpy.RasterCalculator_sa("D:/Request/Request_miguel/ensemble/rcp45/global_30s/2020_2049/col_adm/bio_1 - S:/observed/gridded_products/worldclim/Global_30s/bio_1","D:/Request/Request_miguel/ensemble/rcp45/global_30s/2020_2049/col_adm/anomalies/bio1")












