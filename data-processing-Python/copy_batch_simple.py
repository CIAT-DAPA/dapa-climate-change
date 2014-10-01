# Author: Jaime Tarapues
# September, 2013

import sys, os, glob, shutil, re

# python D:\jaime\PYTHON\copy_batch_simple.py Z:\Request\Request_ousmane F:\raw\rcp26 .nc .nc rcp26 T:\gcm\cmip5\raw\daily
# python D:\jaime\PYTHON\copy_batch_simple.py Z:\Request\Request_ousmane F:\raw\rcp45 .nc .nc rcp45 T:\gcm\cmip5\raw\daily
# python D:\jaime\PYTHON\copy_batch_simple.py Z:\Request\Request_ousmane F:\raw\rcp60 .nc .nc rcp60 T:\gcm\cmip5\raw\daily
# python D:\jaime\PYTHON\copy_batch_simple.py Z:\Request\Request_ousmane F:\raw\rcp85 .nc .nc rcp85 T:\gcm\cmip5\raw\daily

dirbase = sys.argv[1]
dirout = sys.argv[2]
data1 = sys.argv[3] # type of files to copie
data2 = sys.argv[4] # type of files to copie
rcp = sys.argv[5]
data = sys.argv[6]

# selmodel=['cccma_cancm4', 'csiro_mk3_6_0', 'ipsl_cm5a_lr', 'miroc_miroc5', 'ncar_ccsm4', 'bcc_csm1_1', 'ec_earth', 'inm_cm4', 'ipsl_cm5a_mr', 'miroc_esm', 'miroc_esm_chem', 'mpi_esm_mr', 'mri_cgcm3', 'ncc_noresm1_m']

# selmodel= ['csiro_mk3_6_0', 'ipsl_cm5a_lr', 'miroc_miroc5', 'ncar_ccsm4', 'bnu_esm', 'gfdl_esm2m', 'inm_cm4', 'ipsl_cm5a_mr', 'miroc_esm', 'miroc_esm_chem', 'mpi_esm_mr', 'mri_cgcm3', 'ncc_noresm1_m']

selmodel= ['mpi_esm_mr']

dirbase=data+'\\' + rcp
print dirbase
for root, dirs, files in os.walk(dirbase):
	for name in files:
		if name.endswith((data1, data2)):
			out = dirout + "\\" + root[len(dirbase):] # numero de caracteres de dirbase
			# if root.split("\\")[-1] == "5min":
			# if name.find("prec")!=-1 or name.find("tmin")!=-1 or name.find("tmax")!=-1 or name.find("tmean")!=-1 or name.find("bio")!=-1:
			model=out.split('\\')[-2]
			ens=out.split('\\')[-1]
			var=name.split("_")[0]
			
			for i in selmodel:
				if model == i:
					
					if ens=='r1i1p1':
						
						if var=='tas' or var=='tasmin' or var=='tasmax' or var=='pr':
							print model,ens,var
							# if name.split("_")[-1]=="asc.zip":
							if not os.path.isdir(out):
								os.system('mkdir ' + out)
							if not os.path.isdir(out + "\\" + name):	
								os.system('robocopy ' + root + " " + out + ' '+ name +' /z /s')	
								# shutil.copy(root+'\\'+name, out + "\\" + name)
								# print root+'\\'+name, out + "\\" + name
							print "... copied",name
					
		
	


# for root, dirs, files in os.walk(dirbase):
	# for name in files:
		# if name.endswith((data1, data2)):
			# out = dirout + "\\" + root[len(dirbase):] # numero de caracteres de dirbase
			# if not os.path.isdir(out):
				# os.system('mkdir ' + out)
			# if not os.path.isdir(out + "\\" + name):	
				# os.system('robocopy ' + root + " " + out + ' '+ name +' /z /s')	 # shutil.copyfile(root+'\\'+name, dirout + "\\" + os.path.basename(root) + "\\" + name)
				

		
				
print "done!!!"
