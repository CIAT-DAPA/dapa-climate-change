from osgeo import gdal,ogr
import struct, glob, sys, os#, numpy #-> se usa para band.ReadAsArray
# D:\cygwin\bin\python2.7.exe D:\Dropbox\_scripts\GDAL_Python\getValuesRaster.py S:\observed\gridded_products\chirps\daily\0_1deg -76 4 1981 01
# F:/cygwin64/bin/python2.7.exe E:/Inetpub/Bc_DownScale/scripts/getValuesRaster.py S:/observed/gridded_products/chirps/daily/32bits -76 4 1981 01
# python getValuesRaster.py /mnt/data_cluster_4/observed/gridded_products/chirps/daily/32bits -76 4 1981 01

# python getValuesRaster.py /mnt/data_cluster_4/observed/gridded_products/chirps/daily/32bits /home/cnavarro/gtm_extract 2016

dirbase = sys.argv[1]
dirout = sys.argv[2]
# lon = sys.argv[2]
# lat = sys.argv[3]
year = sys.argv[3]
# mon = sys.argv[5]
# day = sys.argv[6]


dic = {"jal1":"-90.123,14.632", "jal2":"-89.875,14.793", "jal3":"-89.792,14.628", "jut1":"-89.712,14.382", "jut2":"-90.166,13.878", "jut3":"-89.629,14.292",  "chi1":"-89.46,14.628", "chi2":"-89.29,14.795", "chi3":"-89.628,14.635"}

for site in sorted(dic):

	staFile = dirout + "/" + site + ".txt"
	if not os.path.isfile(staFile):
		wFile = open(staFile, "w")
		wFile.write("Site" + "\t" + "Lon" + "\t" + "Lat" + "\t" + "Date" + "\t" + "Value" + "\n")
		wFile.close()


	mx,my=float(dic[site].split(",")[0]), float(dic[site].split(",")[1])
	print site,mx,my

	for mon in range(5,10+1,1):

		mon = str(mon)
	
		for day in range(1,31+1,1):

			day = str(day)
			
			if int(mon) < 10:
				mon0="0"+mon
			else: 
				mon0=mon
			if int(day) < 10:
				day0="0"+day
			else:
				day0=day
				
			# mx,my=float(lon), float(lat)  #coord in map units # mx,my=-74.930451, 5.363346  #coord in map units

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
			
			
			filetif = dirbase+"//"+"chirps-v2.0."+year+"."+mon0+"."+day0+".tif"
			if os.path.exists(filetif):	
				src_filename = filetif
				src_ds=gdal.Open(src_filename) 
				gt=src_ds.GetGeoTransform()
				px = int((mx - gt[0]) / gt[1]) #x pixel
				py = int((my - gt[3]) / gt[5]) #y pixel			############### print "[ RASTER BAND COUNT ]: ", src_ds.RasterCount
				# for band in range( src_ds.RasterCount ):
				band = 1
				srcband = src_ds.GetRasterBand(band)
				structval=srcband.ReadRaster(px,py,1,1,buf_type=gdal.GDT_Float32) #Assumes 16 bit int aka 'short'
				intval = struct.unpack('f' , structval) #use the 'short' format code (2 bytes) not int (4 bytes) #######intval=srcband.ReadAsArray(px,py,1,1) # otra forma para obtener el valor del raster ############ print tif[2:6],tif[6:8],band,intval[0]
				print site, year+"-"+mon0+"-"+day0,intval[0]
				wFile = open(staFile, "a")
				wFile.write(site + "\t" + str(mx) + "\t" + str(my) + "\t" + str(year) + str(mon0) + str(day0) + "\t" + str(intval[0]) + "\n")
		
				