# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: August 08th, 2014
# Purpose: Purpose: Cut by mask worldclim data, ensemble and anomalies
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob, shutil
gp = arcgisscripting.create(9.3)

# python Cut_ensemble_gcm_std_changes.py T:\gcm\cmip5\downscaled S:\admin_boundaries\grid_files\npl_adm\npl0 D:\cenavarro\csa_profiles\npl\by_model rcp45 2020_2049,2040_2069,2060_2089 NO 30s bio
# "npl", "zmb", "moz", "pak", "btn", "bgd", "ben", "gmb", "civ", "phl"

# Arguments
dirbase = sys.argv[1]
admdir = sys.argv[2]
dirout = sys.argv[3]
rcpI = sys.argv[4]
periods = sys.argv[5]
switch = sys.argv[6]
resol = sys.argv[7]
variable = sys.argv[8]


# Clean screen
os.system('cls')

gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK ENSEMBLE, ANOMALIES AND WORLDCLIM  "
print "~~~~~~~~~~~~~~~~~~~~~~\n"

if rcpI == "ALL":
	rcpList = 'rcp26','rcp45','rcp60','rcp85'
else:
	rcpList = rcpI.split(",")

if periods == "ALL":
	periodlist = '2020_2049','2040_2069','2060_2089','2070_2099'
else:
	periodlist = periods.split(",")

# variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
variablelist = ["bio" ]

if variable == "ALL":
	variablelist = ["bio","cons_mths","prec","tmin","tmax","tmean" ]
else:
	variablelist = variable.split(",")



worldclim = "S:\observed\gridded_products\worldclim\Global_"+resol	

### Changes by model

for rcp in rcpList:		

	modellist = sorted(os.listdir(dirbase + "\\" + rcp + "\\global_" + resol))

	for model in modellist:
	
		for period in periodlist:

			endir = dirbase + "\\"  + rcp + "\\global_" + resol + "\\" + model + "\\r1i1p1\\" + period 
			print "Cutting ", model, period
			for var in variablelist:
				if var == "bio":
					num = 19
				else:
					num = 12
				# for month in [1,12]:
				for month in [1, 12]:
					if var == "cons_mths":
						variable = var
					else:
						variable = var + "_" + str(month)
						
						diroutraster = dirout + "\\downscaled\\" + rcp + "\\" + model + "\\" + period 

						if not os.path.exists(diroutraster):
							os.system('mkdir ' + diroutraster)		
						
						bio = endir + "\\" + variable
						OutRaster = diroutraster + "\\" + os.path.basename(bio)
						
						# Para cortar datos ensemble
						if not gp.Exists(OutRaster):
							# print "... "+variable
							gp.ExtractByMask_sa(bio, admdir, OutRaster)
						# else:	
							# print variable

						# Para cortar datos worldclim
						dirworldclim = dirout + "\\worldclim"
						if not os.path.exists(dirworldclim):
							os.system('mkdir ' + dirworldclim)			
						base = worldclim+ "\\" + variable
						outworldclim =dirworldclim+ "\\" + variable +'_wc'
						if not gp.Exists(outworldclim):
							# print outworldclim
							gp.ExtractByMask_sa(base, admdir, outworldclim)
						# else:
							# print "...ya existe worldclim!", variable

						# Para calcular anomalias
						dirouanoma = dirout + "\\anomalies\\"+ rcp+ "\\" +model + "\\" + period 		
						if not os.path.exists(dirouanoma):
							os.system('mkdir ' + dirouanoma)			
						gp.workspace = diroutraster
						inrastertmax = diroutraster + "\\"+variable
						inrastertmin = dirworldclim + "\\"+variable+'_wc'
						if variable=="bio_1":
							inexpresion = "( "+inrastertmax + " - " + inrastertmin + " ) * 0.1"
						else:
							inexpresion =  'float ( ' +inrastertmax + ' - ' + inrastertmin + ' ) / ' +inrastertmin + ' * 100'
							# inexpresion =  inrastertmax + ' - ' + inrastertmin
						outAnomala= dirouanoma + "\\"+variable
						
						if not gp.Exists(outAnomala):
							# print outAnomala
							gp.SingleOutputMapAlgebra_sa(inexpresion, outAnomala)	
						# else:
							# print "...ya existe anomalia!", variable			
						

							
				print '...done',var
							
gp.AddMessage("\n \t ====> DONE!! <====")  