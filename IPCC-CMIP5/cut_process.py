# ---------------------------------------------------------
# Author: Jaime Tarapues
# Purpouse: Copy rasters to another location
# ---------------------------------------------------------
# python D:\jetarapues\Request\Request_ocampo\cut_process.py T:\gcm\cmip5\downscaled D:\jetarapues\Request\Request_ocampo\pan S:\admin_boundaries\grid_files\pan_adm\pan0 rcp26 30s bcc_csm1_1 2020_2049 prec,tmin YES YES

import arcgisscripting, os, sys, string, glob, subprocess
gp = arcgisscripting.create(9.3)
gp.CheckOutExtension("Spatial")

dirbase = sys.argv[1]
rcp = sys.argv[2]

dirGCM='T:\\gcm\\cmip5\\downscaled'

# reslist = ["30s"] #, "2_5min", "5min", "10min" #["10min"]#
# varlist = ["bio","tmin","tmax","tmean","prec","cons"]
# periodDc = {"2020_2049", "2040_2069", "2060_2089", "2070_2099"}
# format = 'asc'#"grd"
# extension = "zip"

dirbase = sys.argv[1] #T:\gcm\cmip5\downscaled
dirout = sys.argv[2]
mask = sys.argv[3]
rcps = sys.argv[4]
resolution = sys.argv[5]
models = sys.argv[6]
periods = sys.argv[7]
variable = sys.argv[8]
ascii = sys.argv[9]
wcl = sys.argv[10]

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
	if models == "ALL":
		modellist = sorted(os.listdir(dirbase + "\\" + rcp + "\\global_" + str(resolution) ))
	else: 
		modellist = models.split(",")
	gp.AddMessage("Models: " + str(modellist))
	for model in modellist:
		for period in periodlist:
				for var in variablelist:
					if not os.path.exists(dirout+"\\"+rcp+"_extracts"+"\\global_" + str(resolution)+"\\"+model+"\\r1i1p1\\"+period+"\\_asciis\\"+var+"_asc.zip"):
						cmd = 'arc "'+'&run cut_process.aml '+ dirbase+' '+ dirout+' '+ mask+' '+rcp+' '+resolution+' '+var+' '+period+' '+model+' '+ascii+' '+wcl+'"'
						print '... processing: '
						proc = subprocess.call(cmd,shell=True)
				
print "\n \t Process done!!"