# ---------------------------------------------------------
# Author: Carlos Navarro
# Purpouse: Convert asciis to grids in a workspace
# ---------------------------------------------------------

import os, sys, glob, shutil
# gp = arcgisscripting.create(9.3)

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python rename_emergency.py H:\Final_Course\day_5\Crops D:\CIAT\CCAFS\Workshops\ecocrop-training-mozambique\Final_Course\day_6"
	sys.exit(1)

	
cropNameDir = {"cassava":"Moz_Cassava_l", "cotton":"moz_Cotton", "cowpea": "crop_Cowpea_l", "groundnut": "future_GroundNut", "millet":"moz_Millet", "pigeonpea":"Moz_Pigeonpea", "sorghum":"moz_Sorghum", "maize":"crop_maize"}
cropChgNameDir = {"cassava":"moz_cassava", "cotton":"moz_cotton", "cowpea": "moz_cowpea", "groundnut": "moz_groundnut", "millet":"moz_millet", "pigeonpea":"moz_pigeonpea", "sorghum":"moz_sorghum", "maize":"moz_maize"}
periodlist = "current", "2020_2049", "2040_2069"

# Arguments
dirbase = sys.argv[1]
dirout = sys.argv[2]

# Clear window
os.system('cls')

for period in periodlist:

	if period == "current":

		croplist = glob.glob(dirbase + "\\*")
		
		for crop in croplist:

			cropname = os.path.basename(crop)
			
			asclist = glob.glob(crop + "\\" + period + "\\analyses\\runs\\*")

			for asc in asclist:
				
				ascname = os.path.basename(asc)
				ascnamemod = ascname.replace(cropNameDir[cropname], cropChgNameDir[cropname])
						
				print period + " " + os.path.basename(asc)
				ascmod = asc.replace(ascname, ascnamemod)
				
				outdir = dirout + "\\" + period + "\\analyses\\runs"
				
				if not os.path.exists(outdir):
					os.system('mkdir ' + outdir)
				
				if not os.path.exists(outdir + "\\" + ascnamemod):
				
					shutil.copyfile(asc, outdir + "\\" + ascnamemod)


	else: 

		croplist = glob.glob(dirbase + "\\*")

		for crop in croplist:

			cropname = os.path.basename(crop)

			gcmlist = glob.glob(crop + "\\" + period + "\\analyses\\runs-future\\*")
			
			for gcm in gcmlist:
			
				gcmname = os.path.basename(gcm)
				
				asclist = glob.glob(gcm + "\\*")
				
				for asc in asclist:
					
					print period + " " + os.path.basename(asc)
					
					ascname = os.path.basename(asc)
					ascnamemod = ascname.replace(cropNameDir[cropname], cropChgNameDir[cropname])
					
					outdir = dirout + "\\" + period + "\\analyses\\runs-future\\" + gcmname
					
					if not os.path.exists(outdir):
						os.system('mkdir ' + outdir)
					
					if not os.path.exists(outdir + "\\" + ascnamemod):
			
						shutil.copyfile(asc, outdir + "\\" + ascnamemod)
