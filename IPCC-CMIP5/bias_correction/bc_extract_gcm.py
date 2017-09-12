# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: September 23th, 2014
# Updated: July 28th, 2014
# Purpose: Purpose: extract values daily data of cmip5 
# ----------------------------------------------------------------------------------

import os, sys, string,glob, shutil

# python D:\jetarapues\_scripts\bc_extract_gcm.py T:\gcm\cmip5\raw\daily\rcp45\gfdl_esm2m\r1i1p1\pr_day_GFDL-ESM2M_rcp45_r1i1p1_20060101-21001231.nc D:\jetarapues\Request\Request_cnavarro\bc\tes.tab 2006 2100 -72.301412 5.339301 YES cdo


#Syntax
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <Extract_MaskGCM.py> <dirout> <mask> <dataset> <sres> <resolution> <models> <periods> <variable> <ascii> <descfile>"
	print "   - ie: "
	sys.exit(1)

#Set variables
ifile = sys.argv[1]
odat = sys.argv[2]
yi = sys.argv[3]
yf = sys.argv[4]
lon=sys.argv[5]
lat=sys.argv[6]
vartype=sys.argv[7]
dircdo = sys.argv[8]

# Clean screen
os.system('cls')

name=os.path.basename(ifile)

if not os.path.exists(odat):
	# print '\n...Extracting',name,'lon:'+str(lon)+' lat:'+lat,'Date:'+str(yi)+'-'+str(yf),'\n'
	if vartype == 'NO':
		os.system(dircdo+" -s -outputtab,date,value -remapnn,lon="+str(lon)+"_lat="+lat+' -selyear,'+str(yi)+'/'+str(yf)+' '+ifile+" > "+odat)
	else:
		var=name.split("_")[0]
		if var == 'hur':
			os.system(dircdo+" -s -outputtab,date,value -remapnn,lon="+str(lon)+"_lat="+lat+' -selyear,'+str(yi)+'/'+str(yf)+' -selname,'+var+' -sellevel,85000 '+ifile+" > "+odat)
		else:
			os.system(dircdo+" -s -outputtab,date,value -remapnn,lon="+str(lon)+"_lat="+lat+' -selyear,'+str(yi)+'/'+str(yf)+' -selname,'+var+' '+ifile+" > "+odat)
# else:
	# print '\t...Extracted by coordinate',name

