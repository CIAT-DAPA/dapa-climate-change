# ---------------------------------------------------------
# Author: Jaime Tarapues
# Purpouse: correr el script donde estan los scripts cut_process.aml, cut_GCM.aml
# ---------------------------------------------------------
# python cut_process.py T:\gcm\cmip5\downscaled C:\Workspace\request\request_oaiikulola C:\Workspace\request\request_oaiikulola\eaf.shp rcp60 30s ALL 2020_2049 tmin NO NO name@email.com YES

import arcgisscripting, os, sys, string, glob, subprocess, time
import smtplib
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText

gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")

# dirs
dirGCM='T:\\gcm\\cmip5\\downscaled'
CopyToFTP="Y:\\data\\data_requests"
FTP="ftp://ftp.ciat.cgiar.org/DAPA/projects/GCMPage/data/data_requests/"

dirbase = sys.argv[1] #T:\gcm\cmip5\downscaled
dirout = sys.argv[2]
mask = sys.argv[3]
rcps = sys.argv[4]
resolution = sys.argv[5]
models = sys.argv[6]
periods = sys.argv[7]
variable = sys.argv[8]
ascii = sys.argv[9]
wcl = sys.argv[10]# cut worldclim: YES|NO
toaddr = sys.argv[11] # send email: name@email.com|NO
tif = sys.argv[12] # convert to tiff

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

if periods == "ALL":
	periodlist = '2020_2049','2040_2069' #,'2060_2089','2070_2099'
else:
	periodlist = periods.split(",")
	
#Get lists of data
if variable == "ALL":
	variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")
	
if tif == "yes":
	filenameF="_tif.zip"
elif ascii=="yes":
	filenameF="_asc.zip"
else:
	filenameF=".zip"
	
gp.AddMessage( "Periods: " + str(periodlist) )	
gp.AddMessage( "Variables: " + str(variablelist))		

for rcp in rcplist:
	if models == "ALL":
		modellist = sorted(os.listdir(dirbase + "\\" + rcp + "\\global_" + str(resolution) ))
	else: 
		modellist = models.split(",")
	gp.AddMessage("Models: " + str(modellist))
	for model in modellist:
		for period in periodlist:
				for var in variablelist:
					if not os.path.exists(dirout+"\\"+rcp+"_extracts"+"\\global_" + str(resolution)+"\\"+model+"\\r1i1p1\\"+period+"\\_asciis\\"+var+filenameF):
						cmd = 'arc "'+'&run cut_process.aml '+ dirbase+' '+ dirout+' '+ mask+' '+rcp+' '+resolution+' '+var+' '+period+' '+model+' '+ascii+' '+wcl+' '+tif+'"'
						print '... processing: '
						proc = subprocess.call(cmd,shell=True)

# copy to FTP and Send email
if toaddr != "NO":				
	dateCurr=time.strftime("%Y-%d-%m")
	folderFTP=dateCurr+"_Request_"+os.path.basename(os.path.normpath(dirout))
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
	body = "Your data request is complete and ready to download:\n \
	"+FTP+folderFTP+"\n\nThis email is automatically created and distributed, so please do not reply to this email.\n\nRegards,\nCCAFS-Climate team."
	msg.attach(MIMEText(body, 'plain'))
	server = smtplib.SMTP('smtp.gmail.com', 587)
	server.starttls()
	server.login(fromaddr, "downscaled")
	text = msg.as_string()
	toaddrs = [toaddr] + ccaddr
	server.sendmail(fromaddr, toaddrs, text)
	server.quit()

print "\n \t Process done!!"