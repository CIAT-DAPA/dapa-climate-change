# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: September 23th, 2014
# Updated: July 28th, 2014
# Purpose: Purpose: extract values daily data of cmip5 
# ----------------------------------------------------------------------------------

import os, sys, string,glob, shutil

# python G:\_scripts\dapa-climate-change\dapa-toolbox\IPCC-CMIP5\ExtractDataFromNC.py T:\gcm\cmip5\raw G:\jetarapues\Request\Request_camilo rcp26 r1i1p1 daily 2020 2070


#Syntax
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <Extract_MaskGCM.py> <dirbase> <dirout> <mask> <dataset> <sres> <resolution> <models> <periods> <variable> <ascii> <descfile>"
	print "   - ie: python CutGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3 D:\PRUEBAS_SCRIPTs D:\PRUEBAS_SCRIPTs\mask\mask_italia downscaled A2 2_5min bcc_csm1_1 2020_2049 prec YES YES"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
rcp = sys.argv[3]
ensem = sys.argv[4]
time = sys.argv[5]
yi = sys.argv[6]
yf = sys.argv[7]


# Clean screen
os.system('cls')

if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
	
coor=['-72.301412,5.339301']
# coor=['-75.210150,3.271258','-74.950259,3.951141','-73.470392,4.037831','-72.301412,5.339301','-75.854628,8.812796']

for xy in coor:
	print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	print "Extract by coordinate:",xy
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
	lon= xy.split(",")[0]
	lat= xy.split(",")[1]
	outFiles = dirout +"\\"+str(lon)+"-"+str(lat)
	if float(lon) < 0:
		lon = float(lon) + 360		
	if not os.path.exists(outFiles):
		os.system('mkdir ' + outFiles)	
	for root, dirs, files in os.walk(dirbase + "\\"  + time + "\\" + rcp):
		for name in files:
			if name.endswith((".nc", ".nc")): # formato de archivo
				var=name.split("_")[0]
				time=name.split("_")[1]
				model=name.split("_")[2]
				rcp=name.split("_")[3]
				ens=name.split("_")[4]
				yearI=name.split("_")[5].split(".")[0].split("-")[0][0:4]
				yearF=name.split("_")[5].split(".")[0].split("-")[1][0:4]
				if ens == ensem and root.split('\\')[-1]==ens and var!='hur' and var!='sfcWind':
					if int(yearI) >= int(yi):
						yei = yearI
					if int(yearI) <= int(yi):	
						yei = yi						
					if int(yearF) <= int(yf):	
						yef = yearF
					if int(yearF) >= int(yf):	
						yef =yf						
					odat = outFiles+'/'+rcp+'_'+var+'_'+model+'_'+str(yei)+'-'+str(yef)+".tab"
					if not os.path.exists(odat):
						print '\n...Extracting',name,'lon:'+str(lon)+' lat:'+lat,'Date:'+str(yei)+'-'+str(yef),'\n'
						if var == 'hur':
							os.system("G:\jetarapues\cdo160\cdo.exe -outputtab,year,month,day,lon,lat,value -remapnn,lon="+str(lon)+"_lat="+lat+' -selyear,'+str(yei)+'/'+str(yef)+' -selname,'+var+' -sellevel,85000 '+root+"\\"+name+" > "+odat)
						else:
							os.system("G:\jetarapues\cdo160\cdo.exe -outputtab,year,month,day,lon,lat,value -remapnn,lon="+str(lon)+"_lat="+lat+' -selyear,'+str(yei)+'/'+str(yef)+' -selname,'+var+' '+root+"\\"+name+" > "+odat)
					else:
						print '\t...Extracted',name

print "DONE!!"	
