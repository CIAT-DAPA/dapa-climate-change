import arcpy, os

mxd = arcpy.mapping.MapDocument(r"C:\Workspace\jramirez\ccafs_climate\Multimodel_Global_10min.mxd")

for lyr in arcpy.mapping.ListLayers(mxd):
	a = lyr.name
	# b = a.split("-")[1].split("_")[1]
	# c = lyr.dataSource[28:]
	# d = lyr.dataSource
	print a + "\n"
	# print c
	# print r"M:\Workspace\jramirez\ccafs_climate" + c, "RASTER_WORKSPACE", os.path.basename(d) 
    # if b == "A1B":	
		# lyr.findAndReplaceWorkspacePath(r"M:\climate_change\IPCC_CMIP3\SRES_A1B", r"M:\Workspace\jramirez\ccafs_climate\SRES_A1B", "FALSE")
	# lyr.replaceDataSource(r"M:\Workspace\jramirez\ccafs_climate" + c, "RASTER_WORKSPACE", os.path.basename(d) ,"FALSE")

# (r"M:\climate_change\IPCC_CMIP3\SRES_A1B\downscaled\Global_10min\Multimodel_Mean\2020_2049", \downscaled\Global_10min\Multimodel_Mean\2020_2049", "FALSE")

        # lyr.replaceDataSource("C:/Rafiq_GIS/PYTHONTESTING/2011/2011_CountyComm/Data/Orange/OR_ADA_Dist2.shp")
    # if lyr.name == "OR_Rts_Dist1.shp":
        # lyr.replaceDataSource("C:/Rafiq_GIS/PYTHONTESTING/2011/2011_CountyComm/Data/Orange/OR_Rts_Dist2.shp")
    # if lyr.name == "CountyCommission_TriCounty":
        # lyr.definitionQuery = '"DISTRICT" = \'2\' AND "COUNTY" = \'ORANGE\''

##mxd.saveACopy(r"D:\climate_change\IPCC_CMIP3\_layers\Multimodel_Global_10min-Copy1.mxd", "9.3")

# M:\climate_change\IPCC_CMIP3\SRES_B1\downscaled\Global_10min
# M:\climate_change\IPCC_CMIP3\SRES_B1\downscaled\Global_10min\Multimodel_STD\2040_2069\cons_mths\cons_mths
# 
# lyr.replaceDataSource(r"Database Connections\Test_Carta_LIS.sde", "SDE_WORKSPACE", "Carta.LIS.StreetSegment")