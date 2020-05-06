# Jaime Tarapues

from osgeo import gdal,ogr
from datetime import date, timedelta as td
import struct, glob, sys, os #, numpy #-> se usa para band.ReadAsArray
from joblib import Parallel, delayed
import multiprocessing
# https://blog.dominodatalab.com/simple-parallelization/	

# D:\cygwin\bin\python2.7.exe D:\Dropbox\_scripts\GDAL_Python\getValuesCRUv4.py S:\observed\gridded_products\cru-ts-v4\proces_data -76 4 ALL
# python /data/home/jetarapues/scripts/getValuesWclv2.py /mnt/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2\Multi-banda -76 4 ALL

dirbase = sys.argv[1]
lon = sys.argv[2]
lat = sys.argv[3]
vars = sys.argv[4]
yi = int(sys.argv[5])
yf = int(sys.argv[6])

if vars == "ALL":
	varslist = ["prec","tmin","tmax"]
else:
	varslist = vars.split(",")

###### parell by var		
# mx,my=float(lon), float(lat)  #coord in map units # mx,my=-74.930451, 5.363346  #coord in map units
# def processInput(i):
	# values=[]
	# for year in range (yi, yf + 1, 1):	
		# filetif= dirbase +"//"+i+"//"+i+"_"+str(year)+ ".tif"
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
				# val=int(intval[0])/10
				# values.append(i+"_"+str(year)+"-"+str(band)+"_"+str(val))
	# return values 


# num_cores = multiprocessing.cpu_count()
# results = Parallel(n_jobs=num_cores)(delayed(processInput)(i) for i in varslist)
			
# print results		
		
		
		
####### parell by year	
fmttypes = {'Byte':'B', 'UInt16':'H', 'Int16':'h', 'UInt32':'I', 
            'Int32':'i', 'Float32':'f', 'Float64':'d'}		
mx,my=float(lon), float(lat)  #coord in map units # mx,my=-74.930451, 5.363346  #coord in map units
def processInput(i):
	values=[]
	for var in varslist:	
		filetif= dirbase +"//"+var+"//"+var+"_"+str(i)+ ".tif"
		if os.path.exists(filetif):	
			src_filename = filetif
			src_ds=gdal.Open(src_filename) 
			gt=src_ds.GetGeoTransform()
			px = int((mx - gt[0]) / gt[1]) #x pixel
			py = int((my - gt[3]) / gt[5]) #y pixel			############### print "[ RASTER BAND COUNT ]: ", src_ds.RasterCount
			
			for band in range( src_ds.RasterCount ):
				band += 1
				srcband = src_ds.GetRasterBand(band)
				structval = srcband.ReadRaster(px,py,1,1,buf_type=srcband.DataType )	
				bandtype = gdal.GetDataTypeName(srcband.DataType)
				intval = struct.unpack(fmttypes[bandtype] , structval)
				val=int(intval[0])*0.1
				values.append(var+"_"+str(i)+"-"+str(band)+"_"+str(val))
	return values 


num_cores = multiprocessing.cpu_count()
results = Parallel(n_jobs=num_cores)(delayed(processInput)(i) for i in range (yi, yf + 1, 1))
			
print results	


# mx,my=float(lon), float(lat)  #coord in map units # mx,my=-74.930451, 5.363346  #coord in map units
# i=205
# values=[]
# for var in varslist:	
	# filetif= dirbase +"//"+var+"//"+var+"_"+str(i)+ ".tif"
	# if os.path.exists(filetif):	
		# print filetif
		# src_filename = filetif
		# src_ds=gdal.Open(src_filename) 
		# gt=src_ds.GetGeoTransform()
		# px = int((mx - gt[0]) / gt[1]) #x pixel
		# py = int((my - gt[3]) / gt[5]) #y pixel			############### print "[ RASTER BAND COUNT ]: ", src_ds.RasterCount
		
		# for band in range( src_ds.RasterCount ):
			# band += 1
			# srcband = src_ds.GetRasterBand(band)
			# structval = srcband.ReadRaster(px,py,1,1,buf_type=srcband.DataType )	
			# bandtype = gdal.GetDataTypeName(srcband.DataType)
			# intval = struct.unpack(fmttypes[bandtype] , structval)
			# val=int(intval[0])
			# values.append(var+"_"+str(i)+"-"+str(band)+"_"+str(val))
		
# print values	