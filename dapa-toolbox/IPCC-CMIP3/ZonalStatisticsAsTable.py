# python D:\_scripts\dapa\IPCC-CMIP3\ZonalStatisticsAsTable.py


# import arcpy
# from arcpy import env
# from arcpy.sa import *

# env.workspace = "D:\PRUEBAS_SCRIPTs\grids"

# inZoneData = "D:\PRUEBAS_SCRIPTs\mask\mask_recuadro.shp"
# zoneField = "FID"
# inValueRaster = "prec_1"
# outTable = "D:\PRUEBAS_SCRIPTs\pr1\prec_1.dbf"

# arcpy.CheckOutExtension("Spatial")

# outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster,outTable, "DATA")


#==================================================== VERSION 9.3 ======================================================================


import arcgisscripting

gp = arcgisscripting.create(9.3)


inZoneData = "D:\PRUEBAS_SCRIPTs\mask\mask_recuadro.shp"
inValueRaster = "Z:\\gcm\\cmip3\\downscaled\\sres_a2\\Global_5min\\csiro_mk3_5\\2020_2049\\prec_1"
print inValueRaster
outTable = "D:\PRUEBAS_SCRIPTs\pr1\prec_1.dbf"

gp.CheckOutExtension("Spatial")

gp.ZonalStatisticsAsTable_sa(inZoneData, "FID", inValueRaster, outTable, "DATA")

