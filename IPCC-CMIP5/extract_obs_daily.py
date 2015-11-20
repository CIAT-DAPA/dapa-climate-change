# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: September 23th, 2014
# Updated: July 28th, 2014
# Purpose: Purpose: extract values daily data of cmip5 
# ----------------------------------------------------------------------------------

import os, sys, string,glob, shutil

# C:\\Python27\\ArcGIS10.1\\python.exe D:\jetarapues\_scripts\01_check_data_daily_cmip5.py D:\jetarapues\TEMP\extract 2011 2015 100000,85000 -72.301412,5.339301 T:\gcm\cmip5\raw\daily\rcp60\gfdl_esm2g\r1i1p1\original-data\hur_day_GFDL-ESM2G_rcp60_r1i1p1_20110101-20151231.nc D:/jetarapues/cdo/cdo.exe
# python D:\_scripts\dapa-climate-change\IPCC-CMIP5\extract_obs_daily.py D:\jetarapues\TEMP\extract 2008 2010 100000,85000 -72.301412,5.339301 U:\cropdata\agmerra\daily\nc-files\prec_daily_ts_agmerra_1980_2010_d9.nc4 cdo


#Syntax
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <Extract_MaskGCM.py> <dirout> <mask> <dataset> <sres> <resolution> <models> <periods> <variable> <ascii> <descfile>"
	print "   - ie: python CutGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3 D:\PRUEBAS_SCRIPTs D:\PRUEBAS_SCRIPTs\mask\mask_italia downscaled A2 2_5min bcc_csm1_1 2020_2049 prec YES YES"
	sys.exit(1)

#Set variables
dirout = sys.argv[1]
yi = sys.argv[2]
yf = sys.argv[3]
level = sys.argv[4]
coor=sys.argv[5]
file = sys.argv[6]
dircdo = sys.argv[7]

# Clean screen
os.system('cls')

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "Extract by coordinate:",coor
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
lon= coor.split(",")[0]
lat= coor.split(",")[1]

if float(lon) < 0:
	lon = float(lon) + 360		
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)	


name=os.path.basename(file)
var="NA"#name.split("_")[0]
time=name.split("_")[1]
obs=name.split("_")[3]
yearI=name.split("_")[4]
yearF=name.split("_")[5]


if int(yearI) >= int(yi):
	yei = yearI
if int(yearI) <= int(yi):	
	yei = yi						
if int(yearF) <= int(yf):	
	yef = yearF
if int(yearF) >= int(yf):	
	yef =yf						
odat = dirout+'\\'+name.split(".")[0]+".tab"#dirout+'/'+rcp+'_'+var+'_'+model+'_'+str(yei)+'-'+str(yef)+".tab"
if not os.path.exists(odat):
	print '\n...Extracting',name,'lon:'+str(lon)+' lat:'+lat,'Date:'+str(yei)+'-'+str(yef),'\n'
	if var == 'NA':
			os.system(dircdo+" -s -outputtab,year,month,day,lon,lat,value -remapnn,lon="+str(lon)+"_lat="+lat+' -selyear,'+str(yei)+'/'+str(yef)+' '+file+" > "+odat)
	else:
		if var == 'hur':
			os.system(dircdo+" -s -outputtab,year,month,day,lon,lat,value -remapnn,lon="+str(lon)+"_lat="+lat+' -selyear,'+str(yei)+'/'+str(yef)+' -selname,'+var+' -sellevidx,'+level+' '+file+" > "+odat)
		else:
			os.system(dircdo+" -s -outputtab,year,month,day,lon,lat,value -remapnn,lon="+str(lon)+"_lat="+lat+' -selyear,'+str(yei)+'/'+str(yef)+' -selname,'+var+' '+file+" > "+odat)
else:
	print '\t...Extracted',name

	
	
print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "Splitting:",name
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

split=dirout+'\\'+name.split(".")[0]#"\\splitByYear_"+rcp+'_'+var+'_'+model+'_'+str(yei)+'-'+str(yef)
if not os.path.exists(split):
	os.system('mkdir ' + split)
os.system(dircdo+" -s splityear "+file+" "+split+"\\"+var+"_")
	
	
print "DONE!!"	
