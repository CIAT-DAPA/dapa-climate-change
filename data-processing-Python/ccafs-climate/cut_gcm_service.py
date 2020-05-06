# Autor: Jaime Tarapues
# email: j.e.tarapues@cgiar.org
# Fecha: 2018
#

from osgeo import gdal,ogr
# import datetime
from datetime import datetime
#from datetime import date, timedelta as td
import struct, glob, sys, os , numpy as np #-> se usa para band.ReadAsArray
from joblib import Parallel, delayed
import multiprocessing
from rasterstats import zonal_stats
import smtplib
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText
import shutil
# https://blog.dominodatalab.com/simple-parallelization/	
# mask: xmin ymin xmax ymax

# D:\cygwin\bin\python2.7.exe D:\Dropbox\_scripts\GDAL_Python\getValuesCRUv4.py S:\observed\gridded_products\cru-ts-v4\proces_data -76 4 ALL

# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /home/jetarapues/Request/ jaime /mnt/data_cluster_4/admin_boundaries/shp_files/PAN_adm/PAN0.shp rcp26 30s ensemble 2020_2049 prec YES wcl jaime.tm8@gmail.com anom
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /home/jetarapues/Request/ jaime -81.5,7.5,-80.5,9 rcp26 30s ensemble 2020_2049 prec YES wcl jaime.tm8@gmail.com anom
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ jaime -81.5,7.5,-80.5,9 rcp26 30s ensemble 2020_2049 prec NO nowcl jaime.tm8@gmail.com noanom
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ jaime -81.5,7.5,-80.5,9 rcp26 30s bcc_csm1_1 2020_2049 prec NO nowcl jaime.tm8@gmail.com noanom
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /home/jetarapues/Request/ jaime -81.5,7.5,-80.5,9 rcp26 30s ensemble 2020_2049 prec NO nowcl jaime.tm8@gmail.com noanom

# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /home/jetarapues/Request/ Request_Ihamisu /home/jetarapues/Request/masks/Request_Isah/skt_lg2.shp ALL 30s bcc_csm1_1_m,bnu_esm ALL ALL NO wcl ih100@leicester.ac.uk noanom
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /home/jetarapues/Request/ Request_aines_HTI /mnt/data_cluster_4/admin_boundaries/shp_files/HTI_adm/HTI0.shp rcp85 30s ALL ALL ALL NO wcl A.Jines@cgiar.org noanom
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /home/jetarapues/Request/ Request_aines_GUY /mnt/data_cluster_4/admin_boundaries/shp_files/GUY_adm/GUY0.shp rcp85 30s ALL ALL ALL NO wcl A.Jines@cgiar.org noanom

# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ Request_jmutua /mnt/GCMPage/data/data_requests/Masks/aoi/aoi_rectangle.shp ALL 30s ALL 2020_2049,2040_2069 ALL YES wcl J.Y.Mutua@CGIAR.ORG noanom
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ Request_jmutua /mnt/GCMPage/data/data_requests/Masks/ecabren_aoi/ecabren_aoi.shp rcp26,rcp85 30s ALL 2020_2049,2040_2069 ALL YES wcl J.Y.Mutua@CGIAR.ORG noanom
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ Request_hugo /mnt/GCMPage/data/data_requests/Masks/honduras/Departamentos_Honduras.shp ALL 30s ensemble 2020_2049,2060_2089 ALL YES wcl h.a.dorado@cgiar.org noanom

# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ Request_anton /mnt/data_cluster_4/admin_boundaries/shp_files/JAM_adm/JAM0.shp ALL 30s ALL ALL ALL YES wcl anton.eitzinger@gmail.com noanom


# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ Request_brayan /mnt/data_cluster_4/admin_boundaries/shp_files/COL_adm/COL0.shp rcp26,rcp85 30s ensemble 2020_2049,2060_2089 ALL YES wcl jbvalenciag@unal.edu.co noanom


# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ Request_anton -78.4,17.7,-76.1,18.53 ALL 30s ALL ALL ALL YES wcl anton.eitzinger@gmail.com noanom

# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ Request_ben /mnt/data_cluster_4/admin_boundaries/shp_files/ETH_adm/ETH0.shp rcp85 30s ensemble 2040_2069 ALL YES wcl NO anom downscaled
# python2.7 /mnt/data_climatewizard/temp/cut_gcm_service.py /mnt/data_cluster_2/gcm/cmip5 /mnt/GCMPage/data/data_requests/ Request_ajines /mnt/data_cluster_4/admin_boundaries/shp_files/COL_adm/COL0.shp rcp45,rcp85 30s ensemble 2040_2069,2070_2099 ALL YES wcl NO anom downscaled



dirbase = sys.argv[1]
workspasce= sys.argv[2]
idirout = sys.argv[3]
mask = sys.argv[4] #  xmin,ymin,xmax,ymax
rcps = sys.argv[5]
resolution = sys.argv[6]
models = sys.argv[7]
periods = sys.argv[8]
variable = sys.argv[9]
descfile = sys.argv[10]
wcl = sys.argv[11]# cut worldclim: wcl|wcl2|nowcl
toaddr = sys.argv[12] # send email: name@email.com|NO
calcAnom= sys.argv[13] # option: anom|noanom
dataset= sys.argv[14] # option: downscaled|nodownscaled
# dataset = "downscaled"
# Clean screen
os.system('clear')

if wcl=="wcl2":
	wcl_dir="/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_"+resolution+'_v2'
else:
	wcl_dir="/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_"+resolution
CopyToFTP="/mnt/GCMPage/data/data_requests/"
FTP="ftp://ftp.ciat.cgiar.org/DAPA/projects/GCMPage/data/data_requests/"

compress="NO"
# FTP="http://maprooms.ciat.cgiar.org/CCAFS-Climate/downloads/custom/"

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

num_cores = multiprocessing.cpu_count()-2

#Get lists of periods
if rcps == "ALL":
	rcplist = 'rcp26','rcp45','rcp60','rcp85'
else:
	rcplist = rcps.split(",")
	
#Get lists of periods
if periods == "ALL":
	periodlist = '2020_2049','2040_2069','2060_2089','2070_2099'
else:
	periodlist = periods.split(",")
	
#Get lists of data
if variable == "ALL":
	variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")

mask = mask.split(",")	#	#xmin ymin xmax ymax
	
dateCurr=datetime.now().strftime('%Y-%m-%d-%H%M%S') 
folderFTP=dateCurr+"_"+idirout
# dirout=workspasce+folderFTP	
dirout=workspasce+idirout+"_"+datetime.now().strftime("%Y%m%d%H%M%S")	
requestFTP=CopyToFTP+folderFTP	


if os.path.exists(dirout):
	shutil.rmtree(dirout, ignore_errors=True)
	
if not os.path.exists(dirout):
	os.system('mkdir -p ' + dirout)

if os.path.exists(requestFTP):
	shutil.rmtree(requestFTP, ignore_errors=True)


ifile=workspasce+"/"+datetime.now().strftime("%Y%m%d%H%M%S")+"_order_status.st"
checkTXT = open(ifile,"a")
checkTXT.write("Starting...\n\n")
checkTXT.close()
			
if toaddr != "NO":				
	print '... Sending email order to: '+toaddr
	fromaddr = "ccafsclimate@gmail.com"
	ccaddr=[""] #    ####print ', '.join(ccaddr)  #FTP+folderFTP ,"C.E.Navarro@CGIAR.ORG" 
	msg = MIMEMultipart()
	msg['From'] = fromaddr
	msg['To'] = toaddr
	msg['CC'] = ', '.join(ccaddr) 
	msg['Subject'] = "CCAFS-Climate. Order submitted"
	body1 = "Dear CCAFS-Climate user, \n\nYour data request has been successfully submitted. Order Summary: \
	\nRCPs: "+",".join(rcplist)+"\nPeriods: "+",".join(periodlist)+"\nVariables: "+",".join(variablelist)+"\nModels: "+models+"\n"+"WorldClim" + " Global_"+resolution+' : '+wcl+"\nCalculate anomaly: "+calcAnom+"\nStatistics: "+descfile+ "\
	\n\nWhat's next?\
	\nMost orders only take a very short while to process, but larger orders do take more time and are affected by the number of orders in the data request queue.\n"
	html="\n\nIf you want to check the status of your order, please visit our order <a href='"+FTP+datetime.now().strftime("%Y%m%d%H%M%S")+"_order_status.st'>status page.</a> "
	body2 = "\nThis email is automatically created and distributed, so please do not reply to this email. If you have questions write us to the following emails: j.e.tarapues@cgiar.org, C.E.Navarro@cgiar.org.\n\nRegards,\nCCAFS-Climate team." #"".join(outlist)
	msg.attach(MIMEText(body1, 'plain'))
	msg.attach(MIMEText(html, 'html'))
	msg.attach(MIMEText(body2, 'plain'))
	server = smtplib.SMTP('smtp.gmail.com', 587)
	server.starttls()
	server.login(fromaddr, "downscaled")
	text = msg.as_string()
	toaddrs = [toaddr] + ccaddr
	server.sendmail(fromaddr, toaddrs, text)
	server.quit()		

## Cut WorldClim	
print "..Cut WorldClim \n"	

diroutwcl=dirout+"/worldclim"
diroutraster=dirout+"/wcl"
if wcl=="wcl" or wcl=="wcl2":
	os.system('mkdir -p ' + diroutraster)
	
	checkTXT = open(ifile,"a")
	checkTXT.write("*** PROCESSING WORLDCLIM... ***\n ")
	checkTXT.close()	
	def processInput(variable):
	# for variable in variablelist: 
		if variable == "bio":
			num = 19
		else:
			num = 12
		for month in range (1, num + 1, 1):#[1,12]:#
			if wcl == "wcl2":
				raster = wcl_dir + "/" + variable + "_" + str(month)+ '.tif'
				OutRaster = diroutraster + "/" + os.path.basename(raster)
			else:
				raster = wcl_dir + "/" + variable + "_" + str(month)
				OutRaster = diroutraster + "/" + os.path.basename(raster)+".tif"
			print raster,diroutwcl + "/worldclim.zip"
			if os.path.exists(raster) and not os.path.exists(diroutwcl + "/worldclim.zip"):
				
				if not os.path.exists(OutRaster):
					if len(mask)>1:
						os.system('gdalwarp -te '+mask[0]+' '+mask[1]+' '+mask[2]+' '+mask[3]+' '+ raster+' '+OutRaster)	
					else:
						os.system('gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES -cutline '+mask[0]+' -crop_to_cutline '+raster+' '+ OutRaster)
						
					checkTXT = open(ifile,"a")
					checkTXT.write("-> WCL "+os.path.basename(raster)+"...done!\n")
					checkTXT.close()						
		return "done" 			
	results = Parallel(n_jobs=num_cores)(delayed(processInput)(variable) for variable in variablelist)
	
	checkTXT = open(ifile,"a")
	checkTXT.write("*** WORLDCLIM...DONE! ***\n\n ")
	checkTXT.close()	

if dataset == "downscaled":

	## para ensemble
	# for rcp in rcplist:
	def processEnsemble(rcp):
		descRaster=[]	
		compressdir=dataset +'/ensemble/' + rcp
		for period in periodlist:
			dirmod=dataset +'/ensemble/' + rcp + "/Global_" + str(resolution)  + "/" + period
			workspace = dirbase + "/" + dirmod
			if os.path.exists(workspace):
				print "\n---> Processing: "  + dirmod+ "\n"
				diroutraster = dirout + "/" + dirmod
				if not os.path.exists(diroutraster):
					os.system('mkdir -p ' + diroutraster)
					
				for variable in variablelist:
					if variable == "bio":
						num = 19
					else:
						num = 12
					for month in range (1, num + 1, 1):#[1,12]:#
						if variable == "cons_mths":
							raster = workspace + "/" + variable #+ "_std"
						else:
							raster = workspace + "/" + variable + "_" + str(month) #+ "_std"

							 
						if os.path.exists(raster):
							OutRaster = diroutraster + "/" + os.path.basename(raster) + '.tif'
							if not os.path.exists(OutRaster):
								if len(mask)>1:
									# print 'gdalwarp -te '+mask[0]+' '+mask[1]+' '+mask[2]+' '+mask[3]+' '+ raster+' '+OutRaster
									os.system('gdalwarp -te '+mask[0]+' '+mask[1]+' '+mask[2]+' '+mask[3]+' '+ raster+' '+OutRaster)	
								else:
									os.system('gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES -cutline '+mask[0]+' -crop_to_cutline '+raster+' '+ OutRaster)
								checkTXT = open(ifile,"a")
								checkTXT.write("-> Ensemble "+rcp+" "+period+" "+os.path.basename(raster)+"...done!\n")
								checkTXT.close()
		if compress=="YES":
			compressanom=dirout+'/'+compressdir + ".zip"
			if not os.path.exists(compressanom):
				print "..compress downscaled..\n"
				os.system('7za a ' + compressanom+ " " + dirout+'/'+compressdir)

		# return "done"
	checkens=len([x for x in models.split(",") if x == "ensemble"])	

	if checkens==1: # si es ensemble
		checkTXT = open(ifile,"a")
		checkTXT.write("*** PROCESSING ENSEMBLE... ***\n ")
		checkTXT.close()
		results = Parallel(n_jobs=num_cores)(delayed(processEnsemble)(rcp) for rcp in rcplist)
		checkTXT = open(ifile,"a")
		checkTXT.write("*** ENSEMBLE...DONE! ***\n\n")
		checkTXT.close()
	## Cut GCM		
	if checkens!=1 or len(models.split(","))>1:
		checkTXT = open(ifile,"a")
		checkTXT.write("*** PROCESSING GCMs... ***\n ")
		checkTXT.close()
	for rcp in rcplist:
		#Get lists of models
		if models == "ALL":
			modellist = sorted(os.listdir(dirbase + "/"  + dataset + "/" + rcp + "/Global_" + str(resolution) ))
			modellist.append('ensemble')
			## ensemble
			checkTXT = open(ifile,"a")
			checkTXT.write("*** PROCESSING ENSEMBLE... ***\n ")
			checkTXT.close()
			results = Parallel(n_jobs=num_cores)(delayed(processEnsemble)(rcp) for rcp in rcplist)
			checkTXT = open(ifile,"a")
			checkTXT.write("*** ENSEMBLE...DONE! ***\n\n")
			checkTXT.close()
			checkens=1		
		else: 
			modellist = models.split(",")
		# checkens=len([x for x in modellist if x == "ensemble"])	
		
		# if not os.path.exists(dirout + "/" + dataset + "/" + rcp):
			# os.system('mkdir -p ' + dirout + "/" + dataset + "/" + rcp)
		# for model in modellist:
		def processInput(model):
			descRaster=[]
			if model !='ensemble':	
				compressdir=dataset + "/" + rcp + "/Global_" + str(resolution) + "/" + model
				for period in periodlist:
						dirmod=dataset + "/" + rcp + "/Global_" + str(resolution) + "/" + model + "/r1i1p1/" + period
						
						
						workspace = dirbase + "/" + dirmod

						if os.path.exists(workspace):
							print "\n---> Processing: "  + dirmod+ "\n"
							diroutraster = dirout + "/" + dirmod
							if not os.path.exists(diroutraster):
								os.system('mkdir -p ' + diroutraster)
								
							for variable in variablelist:
								if variable == "bio":
									num = 19
								else:
									num = 12
								for month in range (1, num + 1, 1):#[1,12]:#
									if variable == "cons_mths":
										raster = workspace + "/" + variable
									else:
										raster = workspace + "/" + variable + "_" + str(month)
										
									if os.path.exists(raster):
										OutRaster = diroutraster + "/" + os.path.basename(raster) + '.tif'
										if not os.path.exists(OutRaster):
											if len(mask)>1:
												os.system('gdalwarp -te '+mask[0]+' '+mask[1]+' '+mask[2]+' '+mask[3]+' '+ raster+' '+OutRaster)	
											else:
												os.system('gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES -cutline '+mask[0]+' -crop_to_cutline '+raster+' '+ OutRaster)
											checkTXT = open(ifile,"a")
											checkTXT.write("-> GCMs "+rcp+" "+period+" "+model+" "+os.path.basename(raster)+"...done!\n")
											checkTXT.close()									
				if compress=="YES":
					compressanom=dirout+'/'+compressdir + ".zip"
					if not os.path.exists(compressanom):
						print "..compress downscaled..\n"
						os.system('7za a ' + compressanom+ " " + dirout+'/'+compressdir)
					
			return "done" 

		results = Parallel(n_jobs=num_cores)(delayed(processInput)(model) for model in modellist)


	if checkens!=1 or len(models.split(","))>1:	
		checkTXT = open(ifile,"a")
		checkTXT.write("*** GCMs...DONE! ***\n\n ")
		checkTXT.close()
	# sys.exit(0);			

		
	## calc anomalia

	if calcAnom=='anom':
		checkTXT = open(ifile,"a")
		checkTXT.write("*** PROCESSING CALC ANOMALY... ***\n ")
		checkTXT.close()
		for rcp in rcplist:
			if models == "ALL":
				modellist = sorted(os.listdir(dirbase + "/"  + dataset + "/" + rcp + "/Global_" + str(resolution) ))
			else: 
				modellist = models.split(",")
				
			def processInput(model):
				compressdir=dirout+'/anomalia/'+rcp + "/Global_" + str(resolution) + "/" + model
				for period in periodlist:
					dirmod=dirout+'/'+dataset + "/" + rcp + "/Global_" + str(resolution) + "/" + model + "/r1i1p1/" + period
					outanom=compressdir  + "/" + period

					if not os.path.exists(outanom):
						os.system('mkdir -p ' + outanom)			
							
					for variable in variablelist:
						if variable == "bio":
							num = 19
						else:
							num = 12
						for month in range (1, num + 1, 1):#[1,12]:#
							if variable == "cons_mths":
								raster = dirmod + "/" + variable+ '.tif'
							else:
								raster = dirmod + "/" + variable + "_" + str(month)+ '.tif'		
								rasterWCL=diroutraster + "/" +variable + "_" + str(month)+ '.tif'	
								
							if os.path.exists(raster) and os.path.exists(rasterWCL) :
								OutRaster = outanom + "/" + os.path.basename(raster)
								if not os.path.exists(OutRaster):			
									if variable=='prec':
										os.system('gdal_calc.py -A '+raster +' -B ' +rasterWCL +' --outfile='+OutRaster+ ' --calc="(((A-B)*1.0)/(B+0.0000001))*100"') # --type=Float32  #--co="COMPRESS=LZW"# to reduce output file size.
									else:
										os.system('gdal_calc.py -A '+raster +' -B ' +rasterWCL +' --outfile='+OutRaster+ ' --calc="A-B"')
										
									checkTXT = open(ifile,"a")
									checkTXT.write("-> ANOMALY "+rcp+" "+period+" "+model+" "+os.path.basename(raster)+"...done!\n")
									checkTXT.close()										
				
				if compress=="YES":
					compressanom=compressdir + ".zip"
					if not os.path.exists(compressanom):
						print "..compress anomalia..\n"
						os.system('7za a ' + compressanom+ " " + compressdir)
			
				return "done" 				
			results = Parallel(n_jobs=num_cores)(delayed(processInput)(model) for model in modellist)

		## For cal ensemble		
		# for rcp in rcplist:
		def processAnomEns(rcp):
			compressdir=dirout+'/anomalia/ensemble'
			for period in periodlist:
				dirmod=dirout+'/'+dataset+'/ensemble/' + rcp + "/Global_" + str(resolution)  + "/" + period
				outanom=dirout+'/anomalia/ensemble/' + rcp + "/Global_" + str(resolution)  + "/" + period			

				if not os.path.exists(outanom):
					os.system('mkdir -p ' + outanom)			
						
				for variable in variablelist:
					if variable == "bio":
						num = 19
					else:
						num = 12
					for month in range (1, num + 1, 1):#[1,12]:#
						if variable == "cons_mths":
							raster = dirmod + "/" + variable+ '.tif'
						else:
							raster = dirmod + "/" + variable + "_" + str(month)+ '.tif'		
							rasterWCL=diroutraster + "/" +variable + "_" + str(month)+ '.tif'	
							
						if os.path.exists(raster) and os.path.exists(rasterWCL) :
							OutRaster = outanom + "/" + os.path.basename(raster)
							if not os.path.exists(OutRaster):			
								if variable=='prec':
									os.system('gdal_calc.py -A '+raster +' -B ' +rasterWCL +' --outfile='+OutRaster+ ' --calc="(((A-B)*1.0)/(B+0.0000001))*100"') # --type=Float32  #--co="COMPRESS=LZW"# to reduce output file size.
								else:
									os.system('gdal_calc.py -A '+raster +' -B ' +rasterWCL +' --outfile='+OutRaster+ ' --calc="A-B"')
									
								checkTXT = open(ifile,"a")
								checkTXT.write("-> ANOMALY "+rcp+" "+period+" ensemble "+os.path.basename(raster)+"...done!\n")
								checkTXT.close()									
			
			if compress=="YES":
				compressanom=compressdir + ".zip"
				if not os.path.exists(compressanom):
					print "..compress anomalia..\n"
					os.system('7za a ' + compressanom+ " " + compressdir)
		
			return "done" 				
		if checkens==1: # si es ensemble
			results = Parallel(n_jobs=num_cores)(delayed(processAnomEns)(rcp) for rcp in rcplist)


	checkTXT = open(ifile,"a")
	checkTXT.write("*** PROCESSING INVENTORY... ***\n ")
	checkTXT.close()		
	print "..Inventory \n"		
	result = [os.path.join(dp, f) for dp, dn, filenames in os.walk(dirout) for f in filenames if os.path.splitext(f)[1] == '.tif']
	values=[]
	# for i in result:
	def processInput(i):
		# dataset=i.split("/")[-7]
		dataset=(i.split(dirout)[1]).split("/")[1]

		if dataset == "downscaled" and not "ensemble" in i.split("/"):
			rcp=i.split("/")[-6]
			model=i.split("/")[-4]
			period=i.split("/")[-2]
		elif dataset == "wcl":
			dataset="worldclim"
			model="baseline"
			rcp="wcl"
			period="1960-1990"
		elif dataset == "downscaled" and "ensemble" in i.split("/"):
			dataset="downscaled"
			rcp=i.split("/")[-4]
			model=i.split("/")[-5]	
			period=i.split("/")[-2]
		elif dataset == "anomalia" and "ensemble" in i.split("/"):
			dataset="anomalia"
			rcp=i.split("/")[-4]
			model=i.split("/")[-5]	
			period=i.split("/")[-2]		
		elif dataset == "anomalia":
			dataset="anomalia"
			rcp=i.split("/")[-5]
			model=i.split("/")[-3]	
			period=i.split("/")[-2]
			
		raster=os.path.basename(i)
		# print i
		if descfile=='YES':
			openRast = gdal.Open(i)
			band = openRast.GetRasterBand(1)
			stats2 = band.GetStatistics(True, True)
			#### rstats = ['min', 'max', 'median', 'mean','percentile_5','percentile_25','percentile_75','percentile_95']
			#### stats = zonal_stats(mask, raster,stats=rstats) # proceso lento
			### no funciona bien:
			nodata = band.GetNoDataValue()
			rasterArray  = band.ReadAsArray(0, 0, band.XSize, band.YSize)
			##### rasterArray = np.ma.masked_equal(rasterArray, nodata) # no funciona para percentile
			p25 = np.percentile(rasterArray[rasterArray!=nodata], 25) # return 25th percentile.
			p50 = np.percentile(rasterArray[rasterArray!=nodata], 50) # return 50th percentile.
			p75 = np.percentile(rasterArray[rasterArray!=nodata], 75) # return 75th percentile.								
			# values.append(dataset+'\t'+rcp+'\t'+model+'\t'+period+'\t'+raster+'\t'+str(stats2[0])+'\t'+str(stats2[1])+'\t'+str(stats2[2])+'\t'+str(stats2[3])+'\t'+str(p25)+'\t'+str(p50)+'\t'+str(p75))
			# print dataset+'\t'+rcp+'\t'+model+'\t'+period+'\t'+raster+'\t'+str(stats2[0])+'\t'+str(stats2[1])+'\t'+str(stats2[2])+'\t'+str(stats2[3])+'\t'+str(p25)+'\t'+str(p50)+'\t'+str(p75)
			return dataset+'\t'+rcp+'\t'+model+'\t'+period+'\t'+raster+'\t'+str(stats2[0])+'\t'+str(stats2[1])+'\t'+str(stats2[2])+'\t'+str(stats2[3])+'\t'+str(p25)+'\t'+str(p50)+'\t'+str(p75)
			##### remove unnecessary variables from memory.
			rasterArray = None;band = None;openRast=None		
		else:
			# print dataset+'\t'+rcp+'\t'+model+'\t'+period+'\t'+raster	
			return dataset+'\t'+rcp+'\t'+model+'\t'+period+'\t'+raster	
			
	describefile = dirout + "/inventory.txt"	
	outFile = open(describefile, "a")
	if descfile=='YES':
		outFile.write("DATASET" + "\t" +"SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "VAR" + "\t" + "MINIMUM" + "\t" + "MAXIMUM"+ "\t" + "MEAN" + "\t" + "STD"+ "\t" + "PERCENTILE_25"+ "\t" +"PERCENTILE_50" + "\t" + "PERCENTILE_75" + "\n")
	else:
		outFile.write("DATASET" + "\t" +"SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "VAR" + "\n")

	results = Parallel(n_jobs=num_cores)(delayed(processInput)(i) for i in result)
	# print results	
	outFile.write("\n".join(results))
	outFile.close()	
	print "..Done inventory \n"
	checkTXT = open(ifile,"a")
	checkTXT.write("*** INVENTORY DONE! ... ***\n ")
	checkTXT.close()
	#### compress NO 
	# outlist=[]
	# if os.path.exists(dirout+'/downscaled'):
		# if not os.path.exists(dirout+'/downscaled.zip'):
			# print "..compress "+dataset+"..\n"
			# os.system('7za a ' + dirout+'/downscaled.zip ' + dirout+'/downscaled')
			# shutil.rmtree(dirout+'/downscaled')
		# outlist.append(FTP+folderFTP+'/'+dataset + ".zip\n")
	# if os.path.exists(dirout+'/anomalia'):
		# if not os.path.exists(dirout+'/anomalia' + ".zip"):
			# print "..compress anomalia..\n"
			# os.system('7za a ' + dirout+'/anomalia' + ".zip " + dirout+'/anomalia')
			# shutil.rmtree(dirout+'/anomalia')
		# outlist.append(FTP+folderFTP+'/'+dirout+'/anomalia' + ".zip\n" )
		### delete files

	if os.path.exists(diroutraster):
		os.system('mkdir -p ' + diroutwcl)	
		if not os.path.exists(diroutwcl + "/worldclim.zip"):
			print "..compress wcl..\n"
			os.system('7za a ' + diroutwcl + "/worldclim.zip " + diroutraster)
			# shutil.rmtree(diroutraster)
		###### outlist.append(FTP+folderFTP+'/wcl.zip')

	#### Para eiminar archivos
	# for rcp in rcplist:
		# if models == "ALL":
			# modellist = sorted(os.listdir(dirbase + "/downscaled/" + rcp + "/Global_" + str(resolution) ))
			# modellist.append('ensemble')
		# else: 
			# modellist = models.split(",")
			
		# for model in modellist:
			# if model=='ensemble':
				# dirmod=dirout+'/downscaled/ensemble/' + rcp 
			# else:
				# dirmod=dirout+'/downscaled/' + rcp + "/Global_" + str(resolution) + "/" + model
				
			# outanom=dirout+'/anomalia/downscaled/'+model			

			# if os.path.exists(dirmod):
				# shutil.rmtree(dirmod)
			
			# if os.path.exists(outanom):
				# shutil.rmtree(outanom)
			
	if wcl=="wcl":
		wcl="YES"
	else:
		wcl="NO"
	if calcAnom=="anom":
		calcAnom="YES"
	else:
		calcAnom="NO"

	checkTXT = open(ifile,"a")
	checkTXT.write("*** Sending files ... ***\n ")
	checkTXT.close()
### send email automatic	
if toaddr != "NO":				
	print '... Coping to FTP: '+dirout+" "+requestFTP
	os.system("cp /home/jetarapues/Request/Readme.txt "+dirout)
	# if workspasce != CopyToFTP:
	os.system("rsync -zarv --remove-source-files --include='*/' --include='*.txt' --include='*.zip' --exclude='*' "+dirout+"/ "+requestFTP)
	print '... Sending email to: '+toaddr
	fromaddr = "ccafsclimate@gmail.com"
	ccaddr=["j.e.tarapues@cgiar.org"] #    ####print ', '.join(ccaddr)  #FTP+folderFTP ,"C.E.Navarro@CGIAR.ORG" 
	msg = MIMEMultipart()
	msg['From'] = fromaddr
	msg['To'] = toaddr
	msg['CC'] = ', '.join(ccaddr) 
	msg['Subject'] = "CCAFS-Climate. Order completed"
	body1 = "Dear CCAFS-Climate user, \n\nYou are receiving this email because a request of data was made from www.CCAFS-Climate.org portal and your data request is complete and ready to download. Order Summary: \
	\nRCPs: "+",".join(rcplist)+"\nPeriods: "+",".join(periodlist)+"\nVariables: "+",".join(variablelist)+"\nModels: "+models+"\n"+"WorldClim" + " Global_"+resolution+' : '+wcl+"\nCalculate anomaly: "+calcAnom+"\n"
	html="<a href='"+FTP+folderFTP+"'>Download Data</a>"
	body2 ="\nThis email is automatically created and distributed, so please do not reply to this email. If you have questions write us to the following emails: j.e.tarapues@cgiar.org, C.E.Navarro@cgiar.org.\n\nRegards,\nCCAFS-Climate team." #"".join(outlist)
	msg.attach(MIMEText(body1, 'plain'))
	msg.attach(MIMEText(html, 'html'))
	msg.attach(MIMEText(body2, 'plain'))
	server = smtplib.SMTP('smtp.gmail.com', 587)
	server.starttls()
	server.login(fromaddr, "downscaled")
	text = msg.as_string()
	toaddrs = [toaddr] + ccaddr
	server.sendmail(fromaddr, toaddrs, text)
	server.quit()		
	
	checkTXT = open(ifile,"a")
	checkTXT.write("See your email or download your data from here:\n"+FTP+folderFTP)
	checkTXT.close()
	
	if os.path.exists(dirout):
		shutil.rmtree(dirout, ignore_errors=True)	
# print "Done!!"
########## Clipping image with gdalwarp
# gdalwarp -te xmin ymin xmax ymax inputfile.tif outputfile.tif						