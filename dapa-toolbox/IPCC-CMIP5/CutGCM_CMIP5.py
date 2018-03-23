# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Modified: Jaime Tarapues
# Date: September 13th, 2010
# Updated: July 28th, 2014
# Purpose: Purpose: Cut by mask, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob, shutil
import smtplib
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText

gp = arcgisscripting.create(9.3)
# python D:\jetarapues\Request\Request_Jhon\CutGCM_CMIP5.py T:\gcm\cmip5 D:\jetarapues\Request\Request_Jhon D:\jetarapues\Request\Request_Jhon\aoi_tz\aoi_tz.shp ALL 30s bcc_csm1_1,bcc_csm1_1_m,cesm1_cam5,csiro_mk3_6_0,fio_esm,gfdl_cm3,gfdl_esm2g,gfdl_esm2m,giss_e2_r,ipsl_cm5a_lr,miroc_esm,miroc_esm_chem,miroc_miroc5,mohc_hadgem2_es,mri_cgcm3,ncar_ccsm4,ncc_noresm1_m,nimr_hadgem2_ao 2020_2049,2040_2069 prec,tmin,tmax,cons_mths,tmean NO NO NO J.Y.Mutua@CGIAR.ORG NO
#Syntax
if len(sys.argv) < 12:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <Extract_MaskGCM.py> <dirbase> <dirout> <mask> <dataset> <rcp> <resolution> <models> <periods> <variable> <ascii> <descfile>"
	print "   - ie: python CutGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3 D:\PRUEBAS_SCRIPTs D:\PRUEBAS_SCRIPTs\mask\mask_italia downscaled A2 2_5min bcc_csm1_1 2020_2049 prec YES YES"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   dirout	: Out folder"	
	print "	  mask		: Input mask data defining areas to extract (ESRI-grid file or shapefile)"
	print "   dataset	: The possibilities are: downscaled, and interpolations dataset"	
	print "   rcp	    : IPCC Emission Escenario. The possibilities are a1b, a2, b1"
	print "   resolution: Units Resolution in arcminutes. The possibilities are 30s 2_5min 5min 10min"
	print "	  models	: Global Climate Models. If you want to choose some models, only separated with commas without spaces. Use 'ALL' to choose all available models"
	print "   periods	: Future 30yr periods. If you want to choose some periods, enter them separated by commas without spaces; e.g. 2020_2049,2040_2069,2060_2089,2070_2099. Use 'ALL' to process all the periods"
	print "	  variable  : The possibilities are: bio(bioclimatic), prec(precipitation), tmax(maximum temperature), tmin(minimum temperature), tmean(average temperature). If you want to choose some variables, only separated with commas without spaces; e.g. prec,tmax,tmin. UUse 'ALL' to select all the data in the workspace."
	print "   ascii	    : If YES convert outputs ESRI-Grid files to Ascii and remove ESRI-Grid files"
	print "   descfile  : If YES describe properties of outputs ESRI-Grid files"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
rcps = sys.argv[4]
resolution = sys.argv[5]
models = sys.argv[6]
periods = sys.argv[7]
variable = sys.argv[8]
ascii = sys.argv[9]
descfile = sys.argv[10]
wcl = sys.argv[11]# cut worldclim: YES|NO
toaddr = sys.argv[12] # send email: name@email.com|NO
tif = sys.argv[13] # convert to tiff

# Clean screen
os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

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

dataset = "downscaled"
	
for rcp in rcplist:
	#Get lists of models
	if models == "ALL":
		modellist = sorted(os.listdir(dirbase + "\\"  + dataset + "\\" + rcp + "\\Global_" + str(resolution) ))
	else: 
		modellist = models.split(",")
		
	if not os.path.exists(dirout + "\\" + dataset + "\\" + rcp):
		os.system('mkdir ' + dirout + "\\" + dataset + "\\" + rcp)
		
	if descfile == "YES":
		#create description file to Raster	
		describefile = dirout + "\\" + dataset + "\\" + rcp + "\\Describe_Global_" + str(resolution)  + ".txt"
		outFile = open(describefile, "w")
		outFile.write("SCENARIO" + "\t" + "MODEL" + "\t" + "PERIOD" + "\t" + "GRID" + "\t" + "MINIMUM" + "\t" + "MAXIMUM" + "\t" + "MEAN" + "\t" + "STD" + "\t" + "CELLSIZE" + "\n")
		
		
	gp.AddMessage("Models: " + str(modellist))
	gp.AddMessage( "Periods: " + str(periodlist) )	
	gp.AddMessage( "Variables: " + str(variablelist))		

		
	for model in modellist:
		# Looping around periods
		for period in periodlist:
		
			gp.workspace = dirbase + "\\" + dataset + "\\" + rcp + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period
			print gp.workspace
			if os.path.exists(gp.workspace) and not os.path.exists(dirout + "\\" + dataset + "\\" + rcp + "\\Global_" + str(resolution) + "\\" + model + "_extract_" + period + "_done.txt"):
				gp.AddMessage( "\n---> Processing: "  + dataset + "\\" + rcp + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period + "\n" )
				diroutraster = dirout + "\\" + dataset + "\\" + rcp + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period
				diroutascii =  dirout + "\\" + dataset + "\\" + rcp + "\\Global_" + str(resolution) + "\\" + model + "\\r1i1p1\\" + period

				if not os.path.exists(diroutraster):
					os.system('mkdir ' + diroutraster)
					
				if variable == "ALL":
					var = "*"
				else:	
					var = variable + "*"
					
				for variable in variablelist:
					if variable == "bio":
						num = 19
					else:
						num = 12
					for month in range (1, num + 1, 1):
						if variable == "cons_mths":
							raster = gp.workspace + "\\" + variable
						else:
							raster = gp.workspace + "\\" + variable + "_" + str(month)

						OutRaster = diroutraster + "\\" + os.path.basename(raster)
						
						if not gp.Exists(OutRaster):
						
							# function ExtractByMask_sa
							gp.ExtractByMask_sa(raster, mask, OutRaster)
							
							gp.AddMessage( "    Extracted " + os.path.basename(raster) )
							
							if not os.path.exists(diroutascii):
								os.system('mkdir ' + diroutascii)
								
							#create description file to Raster						
							if descfile == "YES":	
									
								MIN = gp.GetRasterProperties_management(OutRaster, "MINIMUM")
								MAX = gp.GetRasterProperties_management(OutRaster, "MAXIMUM")
								MEA = gp.GetRasterProperties_management(OutRaster, "MEAN")
								STD = gp.GetRasterProperties_management(OutRaster, "STD")
								CEX = gp.GetRasterProperties_management(OutRaster, "CELLSIZEX")
								outFile = open(describefile, "a")
								outFile.write(rcp + "\t" + model + "\t" + period + "\t" + os.path.basename(raster) + "\t" + MIN.getoutput(0) + "\t" + MAX.getoutput(0) + "\t" + MEA.getoutput(0) + "\t" + STD.getoutput(0) + "\t" + CEX.getoutput(0) + "\n")

				if ascii == "YES":
					for variable in variablelist:
						for month in range (1, 12 + 1, 1):
							if variable == "cons_mths":
								raster = diroutraster + "\\" + variable
							else:
								raster = diroutraster + "\\" + variable + "_" + str(month)	
							if os.path.exists(raster):								
								OutAscii = diroutascii + "\\" + os.path.basename(raster) + ".asc"
								gp.AddMessage( "\n    Converting to ascii " + os.path.basename(raster) )
								gp.RasterToASCII_conversion(raster, OutAscii)
								
								# Compress ESRI-asciis files
								InZip = diroutascii + "\\" + os.path.basename(raster).split("_")[0] + "_asc.zip"
								os.system('7za a ' + InZip + " " + OutAscii)
								os.remove(OutAscii)
								if os.path.exists(OutAscii[:-3]+"prj"):
									os.remove(OutAscii[:-3]+"prj")
								gp.delete_management(raster)

					if os.path.exists(diroutraster + '\\info'):
						shutil.rmtree(diroutraster + '\\info')
					if os.path.exists(diroutraster + '\\log'):
						os.remove(diroutraster + '\\log')

				print " Done!!"
				checkTXT = open(dirout + "\\" + dataset + "\\" + rcp + "\\Global_" + str(resolution) + "\\" + model + "_extract_" + period + "_done.txt", "w")
				checkTXT.close()
				
			else:
				gp.AddMessage( "The model " + model + " " + period + " is already processed" )
				print "Processing the next period \n"

# copy to FTP and Send email
if toaddr != "NO":				
	dateCurr=time.strftime("%Y-%d-%m")
	folderFTP=dateCurr+"_"+os.path.basename(os.path.normpath(dirout))
	requestFTP=CopyToFTP+"\\"+folderFTP				
	if not os.path.exists(requestFTP):
		os.system('mkdir ' + requestFTP)						
	print '... Coping to FTP: '+dirout+" "+requestFTP
	os.system("robocopy "+dirout+" "+requestFTP+" /s /z /xf *.aml /xf *.py")

	print '... Sending email to: '+toaddr
	fromaddr = "ccafsclimate@gmail.com"
	ccaddr=["j.e.tarapues@cgiar.org","C.E.Navarro@CGIAR.ORG"] #
	# print ', '.join(ccaddr) 
	msg = MIMEMultipart()
	msg['From'] = fromaddr
	msg['To'] = toaddr
	msg['CC'] = ', '.join(ccaddr) 
	msg['Subject'] = "CCAFS-Climate. Climate Data request"
	body = "You are receiving this email because a request of data was made to the CCAFS-Climate.org team and your data request is complete and ready to download:\n \
	"+FTP+folderFTP+"\n\nThis email is automatically created and distributed, so please do not reply to this email. If you have questions write us to the following emails:j.e.tarapues@cgiar.org, C.E.Navarro@cgiar.org.\n\nRegards,\nCCAFS-Climate team."
	msg.attach(MIMEText(body, 'plain'))
	server = smtplib.SMTP('smtp.gmail.com', 587)
	server.starttls()
	server.login(fromaddr, "downscaled")
	text = msg.as_string()
	toaddrs = [toaddr] + ccaddr
	server.sendmail(fromaddr, toaddrs, text)
	server.quit()				
			
gp.AddMessage("\n \t ====> DONE!! <====")  