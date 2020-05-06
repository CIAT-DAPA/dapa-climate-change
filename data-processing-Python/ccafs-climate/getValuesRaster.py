from osgeo import gdal,ogr
import struct, glob, sys, os#, numpy #-> se usa para band.ReadAsArray
# D:\cygwin\bin\python2.7.exe D:\Dropbox\_scripts\GDAL_Python\getValuesRaster.py S:\observed\gridded_products\chirps\daily\32bits\compress -76 4 1981 01
# F:/cygwin64/bin/python2.7.exe E:/Inetpub/Bc_DownScale/scripts/getValuesRaster.py S:/observed/gridded_products/chirps/daily/32bits/compress -76 4 1981 1982 01 12
dirbase = sys.argv[1]
lon = sys.argv[2]
lat = sys.argv[3]
year = sys.argv[4]
mon = sys.argv[5]
# day = sys.argv[6]

if int(mon) < 10:
	mon="0"+mon
# if int(day) < 10:
	# day="0"+day
	
mx,my=float(lon), float(lat)  #coord in map units # mx,my=-74.930451, 5.363346  #coord in map units

filetif = dirbase+"//"+"pr"+year+"-"+mon+".tif"
# for tif in os.listdir(dirbase):
	# if tif.endswith(".tif"):
		# if tif[2:6]==year and tif[6:8]==mon:
			# if os.path.exists(filetif):	
				# print filetif
				# src_filename = dirbase+"\\"+tif
				# src_ds=gdal.Open(src_filename) 
				# gt=src_ds.GetGeoTransform()
				# px = int((mx - gt[0]) / gt[1]) #x pixel
				# py = int((my - gt[3]) / gt[5]) #y pixel			############### print "[ RASTER BAND COUNT ]: ", src_ds.RasterCount
				# for band in range( src_ds.RasterCount ):
					# band += 1
					# srcband = src_ds.GetRasterBand(band)
					# structval=srcband.ReadRaster(px,py,1,1,buf_type=gdal.GDT_Float32) #Assumes 16 bit int aka 'short'
					# intval = struct.unpack('f' , structval) #use the 'short' format code (2 bytes) not int (4 bytes) #######intval=srcband.ReadAsArray(px,py,1,1) # otra forma para obtener el valor del raster ############ print tif[2:6],tif[6:8],band,intval[0]
					# print str(tif[2:6])+"-"+str(tif[6:8])+"-"+str(band),intval[0]

####### para datos diarios solos					
# filetif = dirbase+"//"+"chirps-v2.0."+year+"."+mon+"."+day+".tif"
# if os.path.exists(filetif):	
	# src_filename = filetif
	# src_ds=gdal.Open(src_filename) 
	# gt=src_ds.GetGeoTransform()
	# px = int((mx - gt[0]) / gt[1]) #x pixel
	# py = int((my - gt[3]) / gt[5]) #y pixel			############### print "[ RASTER BAND COUNT ]: ", src_ds.RasterCount
	# band = 1
	# srcband = src_ds.GetRasterBand(band)
	# structval=srcband.ReadRaster(px,py,1,1,buf_type=gdal.GDT_Float32) #Assumes 16 bit int aka 'short'
	# intval = struct.unpack('f' , structval) #use the 'short' format code (2 bytes) not int (4 bytes) #######intval=srcband.ReadAsArray(px,py,1,1) # otra forma para obtener el valor del raster ############ print tif[2:6],tif[6:8],band,intval[0]
	# print year+"-"+mon+"-"+str(band),intval[0]

# para multibandas v1
filetif = dirbase+"//"+"pr"+year+"-"+mon+".tif"
if os.path.exists(filetif):	
	src_filename = filetif
	src_ds=gdal.Open(src_filename) 
	gt=src_ds.GetGeoTransform()
	px = int((mx - gt[0]) / gt[1]) #x pixel
	py = int((my - gt[3]) / gt[5]) #y pixel			############### print "[ RASTER BAND COUNT ]: ", src_ds.RasterCount
	for band in range( src_ds.RasterCount ):
		band += 1
		srcband = src_ds.GetRasterBand(band)
		structval=srcband.ReadRaster(px,py,1,1,buf_type=gdal.GDT_Float32) #Assumes 16 bit int aka 'short'
		intval = struct.unpack('f' , structval) #use the 'short' format code (2 bytes) not int (4 bytes) #######intval=srcband.ReadAsArray(px,py,1,1) # otra forma para obtener el valor del raster ############ print tif[2:6],tif[6:8],band,intval[0]
		print year+"-"+mon+"-"+str(band),intval[0]


		