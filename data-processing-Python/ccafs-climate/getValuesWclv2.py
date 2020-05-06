# Jaime Tarapues

from osgeo import gdal,ogr
from datetime import date, timedelta as td
import struct, glob, sys, os #, numpy #-> se usa para band.ReadAsArray
from joblib import Parallel, delayed
import multiprocessing
# https://blog.dominodatalab.com/simple-parallelization/	

# D:\cygwin\bin\python2.7.exe D:\Dropbox\_scripts\GDAL_Python\getValuesRaster_wcl_v2.py S:\observed\gridded_products\worldclim\Global_30s_v2\Multi-banda -76 4 ALL
# python /data/home/jetarapues/scripts/getValuesWclv2.py /mnt/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2\Multi-banda -76 4 ALL

dirbase = sys.argv[1]
lon = sys.argv[2]
lat = sys.argv[3]
vars = sys.argv[4]

if vars == "ALL":
	varslist = ["prec","tmin","tmax"]
else:
	varslist = vars.split(",")

mx,my=float(lon), float(lat)  #coord in map units # mx,my=-74.930451, 5.363346  #coord in map units
def processInput(i):
	filetif = dirbase+"//"+i+"_16bit.tif"
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
			if i != "prec":
				val=intval[0]/10
			else:
				val=int(intval[0])
			values.append(i+","+str(band)+","+str(val))
		return values 


num_cores = multiprocessing.cpu_count()
    
results = Parallel(n_jobs=num_cores)(delayed(processInput)(i) for i in varslist)
			
print results		
		
