# ---------------------------------------------------------
# Author: Jaime Tarapues
# Purpouse: correr el script donde estan los scripts cut_process.aml, cut_GCM.aml
# ---------------------------------------------------------
# python D:\jetarapues\Request\Request_darango\Latinoamerica\cut_process_ensemble.py T:\gcm\cmip5\downscaled D:\jetarapues\Request\Request_darango\Latinoamerica S:\admin_boundaries\grid_files\pan_adm\pan0 rcp26 30s 2020_2049 prec NO YES NO
# python D:\jetarapues\Request\Request_darango\Latinoamerica\cut_process_ensemble.py T:\gcm\cmip5\downscaled D:\jetarapues\Request\Request_darango\Latinoamerica D:\jetarapues\Request\Request_darango\Latinoamerica\mask\latino ALL 30s 2040_2069 prec,tmax,tmin,tmean NO YES d.arango@cgiar.org

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
periods = sys.argv[6]
variable = sys.argv[7]
ascii = sys.argv[8]
wcl = sys.argv[9]# cut worldclim: YES|NO
toaddr = sys.argv[10] # send email: name@email.com|NO

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
	periodlist = '2020_2049','2040_2069','2060_2089','2070_2099'
else:
	periodlist = periods.split(",")
	
#Get lists of data
if variable == "ALL":
	variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")
	

gp.AddMessage( "Periods: " + str(periodlist) )	
gp.AddMessage( "Variables: " + str(variablelist))		

for rcp in rcplist:
	for period in periodlist:
			for var in variablelist:
				if not os.path.exists(dirout+"\\"+rcp+"_extracts_ens"+"\\global_" + str(resolution)+"\\"+period+"_"+var+"_cut_done.txt"):
					cmd = 'arc "'+'&run cut_process_ensemble.aml '+ dirbase+' '+ dirout+' '+ mask+' '+rcp+' '+resolution+' '+var+' '+period+' '+ascii+' '+wcl+'"'
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