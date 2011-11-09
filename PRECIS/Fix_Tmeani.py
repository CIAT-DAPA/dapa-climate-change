# Description: Convert ASCII to GRID in a workspace 
# Author: Carlos Navarro
# Date: 11/04/10

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Fix_30averages.py L:\climate_change\RCM_Data\Baseline D:\Workspace\Fix_prec_30yr_tmp YES"
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

if baseline = "YES":

	modellist = sorted(os.listdir(dirbase))

	for model in modellist:
		
		period = "1961_1990"


		print dirbase,str(model)
		gp.workspace = dirbase + "\\" + str(model) + "\\30yrAverages\\" + str(period)
			
		for month in range(10, 12, 1):
			InPrec = gp.workspace + "\\prec_" + str(month)
			TmpPrec = dirtemp + "\\prec_" + str(month)
			gp.Times_sa(InPrec, "30", TmpPrec)
			# gp.delete_management(InPrec)
			# gp.CopyRaster_management(TmpPrec, InPrec)3
			# gp.delete_management(TmpPrec)
			
			print "\n\t   ..done! \n"
			
			
				

if baseline = "YES":

	modellist = sorted(os.listdir(dirbase))

	for model in modellist:
		
		period = "1961_1990"

		# for period in periodlist:
			print dirbase + "\\" + str(model) + "\\" + str(period) + "\n"
			
			if not model == "20c3m" and not model == "diferences":
				
				InExpression = dirbase + "\\" + str(model) + "\\" + str(period) + "\\prec_annual" + " - " +  dirbase + "\\20c3m\\" + str(period) + "\\prec_annual"
				if not gp.Exists(dirbase + "\\diferences\\" + str(period) + "\\" + model[4:] + "_p"):
					gp.SingleOutputMapAlgebra_sa(InExpression, dirbase + "\\diferences\\" + str(period) + "\\" + model[4:] + "_p")
				gp.ExtractByMask_sa(dirbase + "\\diferences\\" + str(period) + "\\" + model[4:] + "_p", "D:\Masks\ColPlains\ColPlains.shp", dirbase + "\\diferences\\" + str(period) + "\\mask\\" + model[4:] + "_p")
				
				InExpression = dirbase + "\\" + str(model) + "\\" + str(period) + "\\tmean_annual" + " - " +  dirbase + "\\20c3m\\" + str(period) + "\\tmean_annual"
				if not gp.Exists(dirbase + "\\diferences\\" + str(period) + "\\" + model[4:] + "_t"):
					gp.SingleOutputMapAlgebra_sa(InExpression, dirbase + "\\diferences\\" + str(period) + "\\" + model[4:] + "_t")
				gp.ExtractByMask_sa(dirbase + "\\diferences\\" + str(period) + "\\" + model[4:] + "_t", "D:\Masks\ColPlains\ColPlains.shp", dirbase + "\\diferences\\" + str(period) + "\\mask\\" + model[4:] + "_t")

				
print "Done!!!!"