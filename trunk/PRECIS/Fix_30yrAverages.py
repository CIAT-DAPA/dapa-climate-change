# Description: Convert ASCII to GRID in a workspace 
# Author: Carlos Navarro
# Date: 11/04/10

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Fix_30yrAverages.py D:\climate_change\RCM_Data\SRES_A1B D:\Workspace\Fix_prec_30yr_tmp NO"
	sys.exit(1)

dirbase =sys.argv[1]
# dirout = sys.argv[2]
# if not os.path.exists(dirout):
	# os.system('mkdir ' + dirout)
# variable = sys.argv[3]
tmpdir = sys.argv[2]
baseline = sys.argv[3]

gp.CheckOutExtension("Spatial")
gp.toolbox="management"
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~"
print "        FIX TMEAN        "
print "~~~~~~~~~~~~~~~~~~~~~~~~~"

if baseline == "YES":

	modellist = sorted(os.listdir(dirbase))

	for model in modellist:
		
		period = "1961_1990"


		print dirbase,str(model)
		gp.workspace = dirbase + "\\" + str(model) + "\\30yrAverages\\" + str(period)
			
		for month in range(10, 12 + 1, 1):
			
			InPrec = gp.workspace + "\\prec_" + str(month)
			print os.path.basename(InPrec)
			TmpPrec = tmpdir + "\\prec_" + str(month)
			gp.Times_sa(InPrec, "30", TmpPrec)
			gp.delete_management(InPrec)
			gp.CopyRaster_management(TmpPrec, InPrec)
			gp.delete_management(TmpPrec)
			
			InTmean = gp.workspace + "\\tmean1_5_" + str(month)
			print os.path.basename(InTmean)
			TmpTmean = tmpdir + "\\tmean1_5_" + str(month)
			InExpression = InTmean + " - 546.3"
			gp.SingleOutputMapAlgebra_sa(InExpression, TmpTmean)
			gp.delete_management(InTmean)
			gp.CopyRaster_management(TmpTmean, InTmean)
			gp.delete_management(TmpTmean)

	print "Done!!!!"
			

if baseline == "NO":

	modellist = sorted(os.listdir(dirbase))
	
	for model in modellist:
		if not model == "ECHAM5":
			periodlist = sorted(os.listdir(dirbase + "\\" + model + "\\30yrAverages\\"))

			for period in periodlist:
			
				print dirbase,str(model),str(period)
				gp.workspace = dirbase + "\\" + str(model) + "\\30yrAverages\\" + str(period)
					
				for month in range(10, 12 + 1, 1):
					
					InPrec = gp.workspace + "\\prec_" + str(month)
					print os.path.basename(InPrec)
					TmpPrec = tmpdir + "\\prec_" + str(month)
					gp.Times_sa(InPrec, "30", TmpPrec)
					gp.delete_management(InPrec)
					gp.CopyRaster_management(TmpPrec, InPrec)
					gp.delete_management(TmpPrec)
					
					InTmean = gp.workspace + "\\tmean1_5_" + str(month)
					print os.path.basename(InTmean)
					TmpTmean = tmpdir + "\\tmean1_5_" + str(month)
					InExpression = InTmean + " - 546.3"
					gp.SingleOutputMapAlgebra_sa(InExpression, TmpTmean)
					gp.delete_management(InTmean)
					gp.CopyRaster_management(TmpTmean, InTmean)
					gp.delete_management(TmpTmean)
			
			
	print "Done!!!!"