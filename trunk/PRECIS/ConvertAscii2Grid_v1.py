#------------------------------------------------------------------------------------------------------------------------------------------------------
# Description: Cut ASCII files from PRECIS in daily data and convert to GRID
# Author: 	   Carlos Navarro, Edward Guevara
# Date:  	   27/09/10
# Notes: 	   - Equivalences standar diagnostic variables are available in trunk\dapa-climate-change\PRECIS\Metereological_Variables.txt
# 		       - If an error ocurs, sup month folder and review and descompress input files if is necessary
#		       - Structure of daily data is   outputdir\monthly_grids\YYYY\Variable\Variable_MM\Variable_MMDD
# 		       - Structure of monthly data is   outputdir\monthly_grids\YYYY\Variable\Variable_MM
# 		       - Use "ConvertAscii2Grid_v1.py" for HadCM3Q3; for other scenarios use "ConvertAscii2Grid.py"
# -----------------------------------------------------------------------------------------------------------------------------------------------------

# Import system modules
import os, arcgisscripting, sys, glob, string, shutil

if len(sys.argv) < 6:
	os.system('clear')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python ConvertAscii2Grid_v1.py D:\climate_change\RCM_Data\SRES_A1B\HadCM3Q3 1959 2099 daily D:\climate_change\RCM_Data\SRES_A1B\HadCM3Q3"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
type = sys.argv[4]
dirout = sys.argv[5]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)	

# Dictionary with Standar diagnostic variables
decVar = {"00001": "SurPress", "00024": "TSmean", "00024.max": "TSmax", "00024.min": "TSmin", "02204": "CloudAm", "03234": "SLHeat", "03236": "Tmean1_5", 
		 "03236.max": "Tmax1_5", "03236.min": "Tmin1_5", "03237": "SHum1_5", "03245": "RHum1_5", "03249": "Wsmean", "03249.max": "Wsmax", "03296": "EvSS", 
		 "03297": "EvCR", "03298": "SubSR", "03299": "TransR", "03312": "EvPotR", "03313": "SoilMAF", "03510": "EvPotF1", "03511": "EvPotF2", 
		 "05216": "Prec", "08208": "SoilMRZ", "08225": "TSoil", "16222": "Press", "00024.mmax": "TSmmax", "08223": "SoilML", "16204": "RHum",
		 "00024.mmin": "TSmmin", "03236.mmax": "Tmmax1_5", "03236.mmin": "Tmmin1_5", "03249.mmax": "Wsmmax"} 

os.system('cls')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                         SPLIT ASCIIS PRECIS                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

gp = arcgisscripting.create(9.3)

if type == "daily":

	for year in range(inityear, finalyear + 1, 1):
    
		# Set workspace
		if os.path.exists(dirbase + "\\" + type + "_asciis\\" + str(year)) and not gp.Exists(dirbase + "\\" + type + "_grids\\Ascii2Grid_" + str(year) + "_done.txt"):
			
			print "         > Processing " + str(year)
			
			try:
				print "\n         > Copying inputs asciis... \n"
				if not os.path.exists(dirout + "\\" + type + "_asciis"):
					os.system('mkdir ' + dirout + "\\" + type + "_asciis")
				shutil.copytree(dirbase + "\\" + type + "_asciis\\" + str(year), dirout + "\\" + type + "_asciis\\" + str(year))
			except: 
				print "An error ocurrs while copied input ascii folder of " + str(year)
				sys.exit(2)

			gp.workspace = dirout + "\\" + type + "_asciis\\" + str(year)
			
			# 1. Split into daily files
			# Get a list of asciis in workspace
			print "\n        > Spliting into daily files ... \n"
			ascList = sorted(glob.glob(gp.workspace + "\\*.asc"))
			for asc in ascList:
				
				# Extact variable of the file name
				variable = os.path.basename(asc).split("_")[0:1][0]
				month = os.path.basename(asc).split("_")[-1][0:2]
				
				if not gp.Exists(dirout + "\\" + type + "_grids\\" + str(year) + "\\Ascii2Grid_" + str(decVar [variable]) + "_done.txt") and not str(variable) == "08223" and not str(variable) == "08225" and not str(variable) == "16204":
					
					print "\t  " + str(decVar[variable]) + "\t" + str(year) + "\t" + str(month)
					
					# Defining the splitlen of asc plain text for cut these files
					if str(variable) == "03249" or str(variable) == "03249.max" or str(variable) == "03249.mmax":
						splitLen = 128
					else:
						splitLen = 129 

					input = open(asc, "r").read().split("\n")

					at = 1
				
					for lines in range(0, len(input), splitLen):

						# Get the list slice
						outputData = input[lines:lines+splitLen]

						# Now open a temporal text file, join the new slice with newlines and write it out. Then close the file.
						if at < 10:
							baseName = str(decVar [variable]) + "_" + str(month) + "0" + str(at)
						else:
							baseName = str(decVar [variable]) + "_" + str(month) + str(at)
						
						tmpTXT = open(gp.workspace + "\\" + baseName + ".txt", "w")
						tmpTXT.write("\n".join(outputData))
						tmpTXT.close()
						
						outASC = open(gp.workspace + "\\" + baseName + ".asc", "w")
						outASC.write("NCOLS 135\nNROWS " + str(int(splitLen)-2) + "\nXLLCORNER -93.550025939941\nYLLCORNER -34.1700025200843\nCELLSIZE 0.439999997615814\nNODATA_VALUE -1073741824\n")
						outASC.close()
						
						tmpTXT = open(gp.workspace + "\\" + baseName + ".txt", "r")
						tmpTXT.readline()
						tmpTXT.readline()

						outASC = open(gp.workspace + "\\" + baseName + ".asc", "a")

						for line in tmpTXT.readlines():
							outASC.write(line)

						tmpTXT.close()
						outASC.close()
						
						os.system("del " + gp.workspace + "\\" + baseName + ".txt")
						
						if at == 31:
							os.system("del " + gp.workspace + "\\" + str(decVar [variable]) + "_" + str(month) + "31.asc")
							
						#Increase Counter
						at += 1
				else:
					print "         >" + str(decVar[variable]) + "\t" + str(year) + "\t" + str(month) + " splited" 
			
			print "\n         > Compressing compiled ascii input files ... \n"
			for asc in ascList:
				# Compress input files			
				InZipCom = gp.workspace + "\\_Compiled_asciis.zip"
				os.system("7za a " + InZipCom + " " + asc)
				os.system("del " + asc)
			
			# 2. Convert ASCII to Raster
			print "\n         > Converting Daily asciis to grids \n"
			
			ascdayList = sorted(glob.glob(gp.workspace + "\\*.asc"))
			for ascday in ascdayList:
									
				# Create outputfolder to Grid files
				diroutGrid = dirout + "\\" + type + "_grids\\" + str(year) + "\\" + os.path.basename(ascday)[:-9] 
				if not os.path.exists(diroutGrid):
					#shutil.rmtree(diroutGrid)
					os.system('mkdir ' + diroutGrid)
				#else:
					#os.system('mkdir ' + diroutGrid)
				
				OutGrid = diroutGrid + "\\" + os.path.basename(ascday)[:-4]
				if not gp.Exists(OutGrid):
					gp.ASCIIToRaster_conversion(ascday, OutGrid, "FLOAT")
					print "           " + os.path.basename(ascday)[:-4] + " converted"
				else:
					print "           " + os.path.basename(ascday)[:-4] + " converted"

				if str(ascday)[-8:-4] == "1230":
					#Create check file
					checkTXT = open(dirout + "\\" + type + "_grids\\" + str(year) + "\\Ascii2Grid_" + os.path.basename(ascday)[:-8] + "done.txt", "w")
					checkTXT.close()

			print "\n         > Compressing Daily asciis ... \n"	
			for ascday in ascdayList:	
			
				InZip = gp.workspace + "\\" + os.path.basename(ascday)[:-8] + str(year) + ".zip"
				os.system("7za a " + InZip + " " + ascday)
				os.system("del " + ascday)

			try:
				print "\n         > Copying out grids ... \n"
				if not os.path.exists(dirbase + "\\" + type + "_grids"):
					os.system('mkdir ' + dirbase + "\\" + type + "_grids")
				shutil.copytree(dirout + "\\" + type + "_grids\\" + str(year), dirbase + "\\" + type + "_grids\\" + str(year))
			except: 
				print "An error ocurrs while copied output grid folder of " + str(year)
				sys.exit(3)

			try:
				print "         > Copying asciis compresed ... \n"
				if not os.path.exists(dirbase + "\\" + type + "_asciis_compressed"):
					os.system('mkdir ' + dirbase + "\\" + type + "_asciis_compressed")
				shutil.copytree(dirout + "\\" + type + "_asciis\\" + str(year), dirbase + "\\" + type + "_asciis_compressed\\" + str(year))
			except: 
				print "Error copying output ascii folder of " + str(year)
				sys.exit(4)
			
			print "         > Removing intermediate folders ... \n"
			shutil.rmtree(dirout + "\\" + type + "_grids\\" + str(year))
			shutil.rmtree(dirout + "\\" + type + "_asciis\\" + str(year))
			shutil.rmtree(dirbase + "\\" + type + "_asciis\\" + str(year))
			os.rename(dirbase + "\\" + type + "_asciis_compressed\\" + str(year), dirbase + "\\" + type + "_asciis\\" + str(year))
			
			print "         > " + str(year) + " Done!\n"
			#Create check file
			checkTXT1 = open(dirbase + "\\" + type + "_grids\\Ascii2Grid_" + str(year) + "_done.txt", "w")
			checkTXT1.close()
		
		else :
			print "         > " + str(year) + " Done!\n"
		
	print "Processed all years!!\n"

if type == "monthly":

	for year in range(inityear, finalyear + 1, 1):
			
		if not gp.Exists(dirbase + "\\" + type + "_grids\\Ascii2Grid_" + str(year) + "_done.txt"):	

			try:
				print "\n         Copying inputs asciis... \n"
				if not os.path.exists(dirout + "\\" + type + "_asciis"):
					os.system('mkdir ' + dirout + "\\" + type + "_asciis")
				shutil.copytree(dirbase + "\\" + type + "_asciis\\" + str(year), dirout + "\\" + type + "_asciis\\" + str(year))
			except: 
				print "An error ocurrs while copied input ascii folder of " + str(year)
				sys.exit(2)
			
			# Set workspace
			gp.workspace = dirout + "\\" + type + "_asciis\\" + str(year)
			
			print "  > Processing " + gp.workspace + "\n"
			
			# 1. Editing input ascii text plane files
			print "  > Editing ascii files to convert to ESRI-Asciis... \n"
			
			# Get a list of asciis in workspace
			ascList = glob.glob(gp.workspace + "\\*.asc")
			for asc in ascList:
				# Extact variable of the file name
				variable = os.path.basename(asc).split("_")[0:1][0]
				month = os.path.basename(asc).split("_")[-1][0:2]
				
				if not gp.Exists(dirout + "\\" + type + "_grids\\" + str(year) + "\\Ascii2Grid_" + str(decVar [variable]) + "_done.txt") and not str(variable) == "08223" and not str(variable) == "08225" and not str(variable) == "16204":
					
					print "\t  " + str(decVar[variable]) + "\t" + str(year) + "\t" + str(month) 
					
					if str(variable) == "03249" or str(variable) == "03249.max" or str(variable) == "03249.mmax":
						splitLen = 128
					else:
						splitLen = 129 
					
					input = open(asc, "r").read().split("\n")

					for lines in range(0, splitLen, splitLen):

						# Get the list slice
						outputData = input[lines:lines+splitLen]

						# Now open a temporal text file, join the new slice with newlines and write it out. Then close the file.
						
						baseName = str(decVar [variable]) + "_" + str(month)
						
						tmpTXT = open(gp.workspace + "\\" + baseName + ".txt", "w")
						tmpTXT.write("\n".join(outputData))
						tmpTXT.close()
						
						outASC = open(gp.workspace + "\\" + baseName + ".asc", "w")					
						outASC.write("NCOLS 135\nNROWS " + str(int(splitLen)-2) + "\nXLLCORNER -93.550025939941\nYLLCORNER -34.1700025200843\nCELLSIZE 0.439999997615814\nNODATA_VALUE -1073741824\n")
						outASC.close()
						
						tmpTXT = open(gp.workspace + "\\" + baseName + ".txt", "r")
						tmpTXT.readline()
						tmpTXT.readline()

						outASC = open(gp.workspace + "\\" + baseName + ".asc", "a")

						for line in tmpTXT.readlines():
							outASC.write(line)

						tmpTXT.close()
						outASC.close()
						
						os.system("del " + gp.workspace + "\\" + baseName + ".txt")
					
					print "\n          " + str(decVar[variable]) + "\t" + str(year) + "\t" + str(month) + " edited"
					
			for asc in ascList:
				# Compress input files
				InZipCom = gp.workspace + "\\_Compiled_asciis.zip"
				os.system("7za a " + InZipCom + " " + asc)
				os.system("del " + asc)

			# 2. Convert ASCII to Raster
			print "\n         > Converting Monthly asciis to grids \n"
			
			ascmonthList = sorted(glob.glob(gp.workspace + "\\*.asc"))
			for ascmonth in ascmonthList:
									
				# Create outputfolder to Grid files
				diroutGrid = dirout + "\\" + type + "_grids\\" + str(year) + "\\" + os.path.basename(ascmonth)[:-9] 
				if not os.path.exists(diroutGrid):
					os.system('mkdir ' + diroutGrid)
				
				OutGrid = diroutGrid + "\\" + os.path.basename(ascmonth)[:-4]
				if not gp.Exists(OutGrid):
					gp.ASCIIToRaster_conversion(ascmonth, OutGrid, "FLOAT")
					print "           " + os.path.basename(ascmonth)[:-4] + " converted"
				else:
					print "           " + os.path.basename(ascmonth)[:-4] + " converted"

				if str(ascmonth)[-6:-4] == "12":
					#Create check file
					checkTXT = open(dirout + "\\" + type + "_grids\\" + str(year) + "\\Ascii2Grid_" + os.path.basename(ascmonth)[:-6] + "done.txt", "w")
					checkTXT.close()
				
			print "\n         > Compressing Daily asciis ... \n"	
			for ascmonth in ascmonthList:	
			
				InZip = gp.workspace + "\\" + os.path.basename(ascmonth)[:-6] + str(year) + ".zip"
				os.system("7za a " + InZip + " " + ascmonth)
				os.system("del " + ascmonth)

			try:
				print "\n         > Copying out grids ... \n"
				if not os.path.exists(dirbase + "\\" + type + "_grids"):
					os.system('mkdir ' + dirbase + "\\" + type + "_grids")
				shutil.copytree(dirout + "\\" + type + "_grids\\" + str(year), dirbase + "\\" + type + "_grids\\" + str(year))
			except: 
				print "An error ocurrs while copied output grid folder of " + str(year)
				sys.exit(3)

			try:
				print "         > Copying asciis compresed ... \n"
				if not os.path.exists(dirbase + "\\" + type + "_asciis_compressed"):
					os.system('mkdir ' + dirbase + "\\" + type + "_asciis_compressed")
				shutil.copytree(dirout + "\\" + type + "_asciis\\" + str(year), dirbase + "\\" + type + "_asciis_compressed\\" + str(year))
			except: 
				print "Error copying output ascii folder of " + str(year)
				sys.exit(4)
			
			print "         > Removing intermediate folders ... \n"
			shutil.rmtree(dirout + "\\" + type + "_grids\\" + str(year))
			shutil.rmtree(dirout + "\\" + type + "_asciis\\" + str(year))
			#shutil.rmtree(dirbase + "\\" + type + "_asciis\\" + str(year))
			#os.rename(dirbase + "\\" + type + "_asciis_compressed\\" + str(year), dirbase + "\\" + type + "_asciis\\" + str(year))
			
			print "         > " + str(year) + " Done!\n"
			#Create check file
			checkTXT1 = open(dirbase + "\\" + type + "_grids\\Ascii2Grid_" + str(year) + "_done.txt", "w")
			checkTXT1.close()
		
		else :
			print "         > " + str(year) + " Done!\n"
	
	print "Processed all years!!\n"