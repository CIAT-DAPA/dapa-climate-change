# Jaime Tarapues

from osgeo import gdal,ogr
from datetime import date, timedelta as td
import struct, glob, sys, os,time #, numpy #-> se usa para band.ReadAsArray
from joblib import Parallel, delayed
import multiprocessing
# https://blog.dominodatalab.com/simple-parallelization/	

# D:\cygwin\bin\python2.7.exe D:\Dropbox\_scripts\GDAL_Python\getValuesRaster_v2.py S:\observed\gridded_products\chirps\daily\32bits\compress -76 4 "1981-01,1981-02" chirps
# python /dapadfs/data_cluster_4/portals/ccafs_climate/scripts/getValuesRaster_v2.py /dapadfs/data_cluster_4/observed/gridded_products/chirps/daily/32bits/compress -76 4 "1981-01,1981-02" chirps
# D:\cygwin\bin\python2.7.exe D:\Dropbox\_scripts\GDAL_Python\getValuesRaster_v2.py S:/observed/gridded_products/chirp/daily/16bits -76 4 "2017-01,2017-02" chirp
# python /data/home/jetarapues/scripts/getValuesRaster_v2.py /mnt/data_cluster_4/observed/gridded_products/chirps/daily/32bits/compress -76 4 "1981-01,1981-02"
# /opt/miniconda2/bin/python2.7 getValuesRaster_v2.py /mnt/data_cluster_4/observed/gridded_products/chirps/daily/32bits/compress -76 4 "1981-01,1981-02"
# D:\cygwin\bin\python2.7.exe D:\Dropbox\_scripts\GDAL_Python\getValuesRaster_v2.py S:\observed\gridded_products\chirps\daily\32bits\compress -76 4 1981 1982 01 12
# F:/cygwin64/bin/python2.7.exe E:/Inetpub/Bc_DownScale/scripts/getValuesRaster.py S:/observed/gridded_products/chirps/daily/32bits/compress -76 4 1981 1982 01 12
dirbase = sys.argv[1]
lon = sys.argv[2]
lat = sys.argv[3]
dates = sys.argv[4]
type = sys.argv[5] # chirps / chirp

# yi = sys.argv[4]
# yf = sys.argv[5]
# mi = sys.argv[6]
# mf = sys.argv[7]

# filetif = dirbase+"//"+"pr"+year+"-"+mon+".tif"
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
# filetif = dirbase+"//"+"pr"+year+"-"+mon+".tif"
# if os.path.exists(filetif):	
	# src_filename = filetif
	# src_ds=gdal.Open(src_filename) 
	# gt=src_ds.GetGeoTransform()
	# px = int((mx - gt[0]) / gt[1]) #x pixel
	# py = int((my - gt[3]) / gt[5]) #y pixel			############### print "[ RASTER BAND COUNT ]: ", src_ds.RasterCount
	# for band in range( src_ds.RasterCount ):
		# band += 1
		# srcband = src_ds.GetRasterBand(band)
		# structval=srcband.ReadRaster(px,py,1,1,buf_type=gdal.GDT_Float32) #Assumes 16 bit int aka 'short'
		# intval = struct.unpack('f' , structval) #use the 'short' format code (2 bytes) not int (4 bytes) #######intval=srcband.ReadAsArray(px,py,1,1) # otra forma para obtener el valor del raster ############ print tif[2:6],tif[6:8],band,intval[0]
		# print year+"-"+mon+"-"+str(band),intval[0]

##################
# para multibandas v2 - modificar para que guarde en vector y no en R
#################

# d1 = date(int(yi), int(mi), 1)
# d2 = date(int(yf), int(mf), 1)
# delta = d2 - d1
# ldate=[]
# for i in range(delta.days + 1):
    # ldate.append((d1 + td(days=i)).strftime("%m-%Y"))
# ldate = sorted(list(set(ldate)))
mx,my=float(lon), float(lat)  #coord in map units # mx,my=-74.930451, 5.363346  #coord in map units
hyear=time.strftime("%Y")
hmon=time.strftime("%m")
def processInput(i):
	year=i.split("-")[0]
	mon=i.split("-")[1]
	if type == "chirp":
		dlen=len(i.split("-"))
		if dlen < 3:#int(year) < int(hyear)+1 and int(mon) < int(hmon):
			filetif = dirbase+"//"+"pr"+str(year)+"-"+str(mon)+".tif"
		else:
			day=i.split("-")[2]
			filetif = dirbase+"//"+"dchirp."+str(year)+"."+str(mon)+"."+str(day)+".tif"
	else:
		filetif = dirbase+"//"+"pr"+str(year)+"-"+str(mon)+".tif"
	if os.path.exists(filetif):
		src_filename = filetif
		src_ds=gdal.Open(src_filename) 
		gt=src_ds.GetGeoTransform()
		px = int((mx - gt[0]) / gt[1]) #x pixel
		py = int((my - gt[3]) / gt[5]) #y pixel			############### print "[ RASTER BAND COUNT ]: ", src_ds.RasterCount
		values=[]
		for band in range( src_ds.RasterCount ):
			band += 1
			srcband = src_ds.GetRasterBand(band)
			structval=srcband.ReadRaster(px,py,1,1,buf_type=gdal.GDT_Float32) #Assumes 16 bit int aka 'short'
			intval = struct.unpack('f' , structval) #use the 'short' format code (2 bytes) not int (4 bytes) #######intval=srcband.ReadAsArray(px,py,1,1) # otra forma para obtener el valor del raster ############ print tif[2:6],tif[6:8],band,intval[0]
			if src_ds.RasterCount == 1:
				values.append(year+"-"+mon+"-"+str(day)+"_"+str(int(intval[0])))
			else:
				values.append(year+"-"+mon+"-"+str(band)+"_"+str(int(intval[0])))
		return values 
	# else:
		# return "no"

num_cores = multiprocessing.cpu_count()
    
results = Parallel(n_jobs=num_cores)(delayed(processInput)(i) for i in dates.split(","))
# print results[0]		
print results		
# outFile = open("/dapadfs/data_cluster_4/portals/ccafs_climate/scripts/chirps.txt", "w")
# outFile.write(results[0])
# outFile.close()		

