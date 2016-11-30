# Import system modules

import os, sys, string, glob, shutil
# Define arguments
dirbase ="/mnt/data_climatewizard/AR5_Global_Daily_25k/" #sys.argv[1]
rcp = "historical"
start=20
fin=30
# Clearing screen and getting the arguments
#os.system("cls")

print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "   Prepare Monthly CMIP5 raw files    "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"


rcpDir = dirbase + "\\" + rcp
varlist=["pr"] #,"tasmin","tasmax"
modelist=["ACCESS1-0","bcc-csm1-1","CESM1-BGC","CNRM-CM5","CSIRO-Mk3-6-0","GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","MIROC-ESM","MIROC-ESM-CHEM","MIROC5","MPI-ESM-LR","MPI-ESM-MR","MRI-CGCM3","NorESM1-M"]
# modelist=["ACCESS1-0","bcc-csm1-1","CESM1-BGC","CNRM-CM5"]
# modelist=["CSIRO-Mk3-6-0","download.html","GFDL-CM3","GFDL-ESM2G"]
# modelist=["GFDL-ESM2M","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR"]
# modelist=["MIROC-ESM","MIROC-ESM-CHEM","MIROC5","MPI-ESM-LR","MPI-ESM-MR","MRI-CGCM3","NorESM1-M"]
rcpist=[rcp] #"historical","rcp45","rcp85"

for var in varlist:
	for rcp in rcpist:
		for model in modelist[int(start):int(fin)]:
			ncList = sorted(glob.glob(dirbase + var + "_day_BCSD_"+rcp+"_r1i1p1_"+model+"*.nc"))
			if not len(ncList) == 0:
				print rcp,model,var
				outmon=dirbase+'monthly/' + var+"/"+model.lower()+"/"+rcp
				outyear=dirbase+'annual/' + var+"/"+model.lower()+"/"+rcp
				if not os.path.isdir(outmon):
					os.system("mkdir -p " +outmon)
				if not os.path.isdir(outyear):
					os.system("mkdir -p " +outyear)					
				# BCSD_0.5deg_pr_Amon_access1-0_historical_r1i1p1_195001-200512
				staYear = os.path.basename(ncList[0]).split("_")[-1].split(".")[0]
				endYear = os.path.basename(ncList[-1]).split("_")[-1].split(".")[0]
				merNcdaily=outmon+"//BCSD_0.25deg_"+var + "_Amon_"+model+"_"+rcp+"_r1i1p1_"+staYear+"01-"+endYear+"12_daily.nc"
				merNc=outmon+"//BCSD_0.25deg_"+var + "_Amon_"+model+"_"+rcp+"_r1i1p1_"+staYear+"01-"+endYear+"12.nc"
				merNcUnits=outmon+"//BCSD_0.25deg_"+var + "_Amon_"+model+"_"+rcp+"_r1i1p1_"+staYear+"01-"+endYear+"12_unit.nc"
				merNc4=outmon+"//BCSD_0.25deg_"+var + "_Amon_"+model+"_"+rcp+"_r1i1p1_"+staYear+"01-"+endYear+"12.nc4"
				merNcyear=outyear+"//BCSD_0.25deg_"+var + "_Amon_"+model+"_"+rcp+"_r1i1p1_"+staYear+"01-"+endYear+"12_annual.nc"
				merNcyearUnit=outyear+"//BCSD_0.25deg_"+var + "_Amon_"+model+"_"+rcp+"_r1i1p1_"+staYear+"01-"+endYear+"12_annual_unit.nc"
				merNcyear4=outyear+"/BCSD_0.25deg_"+var + "_Amon_"+model+"_"+rcp+"_r1i1p1_"+staYear+"01-"+endYear+"12_annual.nc4"
				if not os.path.exists(merNc4):
					os.system("cdo -f nc4 mergetime " + ' '.join(ncList) + " " + merNcdaily)	
					print var,rcp,model,"done!"
					if var=="pr":
						factor="-mulc,86400 "
						txt = "cdo -m 1e+20 monsum " + merNcdaily + " " + merNc
					else:
						factor="-addc,273.15 "
						txt = "cdo -m 1e+20 monmean " + merNcdaily + " " + merNc
					
					print txt
					os.system(txt)	
					os.system("rm -rf "+merNcdaily)
					os.system("cdo -m 1e+20 "+factor+merNc + " " +merNcUnits)
					os.system("nccopy -d9 -k4 "+merNcUnits+ " " +merNc4)
					os.system("rm -rf "+merNcUnits)
					### YEARLY
					
				if not os.path.exists(merNcyear4):	
					if var=="pr":
						txtcmd = "cdo -m 1e+20 yearsum " + merNc + " " + merNcyear
					else:
						txtcmd = "cdo -m 1e+20 yearavg " + merNc + " " + merNcyear
					print txtcmd
					os.system(txtcmd)
					os.system("cdo -m 1e+20 "+factor+merNcyear + " " +merNcyearUnit)					
					os.system("nccopy -d9 -k4 "+merNcyearUnit+ " " +merNcyear4)
					os.system("rm -rf "+merNc)
					os.system("rm -rf "+merNcyearUnit)
					os.system("rm -rf "+merNcyear)
print "done!!"					


###### TMEAN 
# for rcp in rcpist:
	# for model in modelist[int(start):int(fin)]:
		# tmax = sorted(glob.glob(dirbase +"monthly/tasmax/" +model.lower()+"//"+rcp +"//*.nc"))
		# tmin = sorted(glob.glob(dirbase +"monthly/tasmin/" +model.lower()+"//"+rcp +"//*.nc"))
		# fnx = tmax
		# fnn = tmin
		# fnxe = tmax
		# ft = fnx.replace('tasmax', 'tmp')
		# fn = fnx.replace('tasmax', 'tas')
		# fne = fnxe.replace('tasmax', 'tas')
		###calc mean daily temp if doesn't already exist
		# if not path.exists(fne):
			# print "\n... calculating daily avg temp for %s%s" % (path.basename(fnamen), y)
			# txt1 = "cdo -m 1e+20 -add %s %s %s" % (fnn, fnx, ft)
			# print txt1
			# system(txt1)
			# txt2 = "cdo divc,2.0 %s %s" % (ft, fn)
			# print txt2
			# system(txt2)
			# txt3 = "rm -rf " + ft
			# print txt3
			# system(txt3)
			# txt4 = "ncrename -h -v tasmin,tas " + fn
			# print txt4
			# system(txt4)
			# txt5 = "mv " + fn + " " + fne
			# print txt5
			# system(txt5)
					
			
