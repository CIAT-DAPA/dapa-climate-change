#---------------------------------------------------------------------------
# Description: Cut ASCII files from PRECIS in daily data and convert to GRID
# Author: Carlos Navarro, Edward Guevara
# Date: 27/09/10
#---------------------------------------------------------------------------

# Import system modules
import os, arcgisscripting, sys, glob, string, shutil

if len(sys.argv) < 5:
	os.system('clear')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python splitASC.py P:\climate_change\RCM_Data\SRES_A1B\HadCM3Q3 1959 2099 daily"
	sys.exit(1)

# --------------------------------------------------------------------------
# Notes: Equivalences standar diagnostic variables are available in trunk\dapa-climate-change\PRECIS\Metereological_Variables.txt
# --------------------------------------------------------------------------
	
	
# Arguments
dirbase = sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
type = sys.argv[4]
	
# Dictionary with Standar diagnostic variables
decVar = {"00001": "SurPress", "00024": "TSmean", "00024.max": "TSmax", "00024.min": "TSmin", "02204": "CloudAm", "03234": "SLHeat", "03236": "Tmean1_5", 
		 "03236.max": "Tmax1_5", "03236.min": "Tmin1_5", "03237": "SHum1_5", "03245": "RHum1_5", "03249": "Wsmean", "03249.max": "Wsmax", "03296": "EvSS", 
		 "03297": "EvCR", "03298": "SubSR", "03299": "TransR", "03312": "EvPotR", "03313": "SoilMois", "03510": "EvPotF1", "03511": "EvPotF2", 
		 "05216": "Prec", "08208": "SoilMoisR", "08223": "SoilMoisL", "08225": "TSoil", "16204": "RHum", "16222": "Press", "00024.mmax": "TSmmax", 
		 "00024.mmin": "TSmmin", "03236.mmax": "Tmmax1_5", "03236.mmin": "Tmmin1_5", "03249.mmax": "Wsmmax"}

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                         SPLIT ASCIIS PRECIS                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

gp = arcgisscripting.create(9.3)

if type == "daily":

	for year in range(inityear, finalyear + 1, 1):
    
		# Set workspace
		gp.workspace = dirbase + "\\" + type + "_asciis\\" + str(year)
		print "    ---> Processing " + gp.workspace + "\n"
		
		# Create outputfolder
		diroutGrid = dirbase + "\\" + type + "_grids\\" + str(year)
		if not os.path.exists(diroutGrid):
			os.system('mkdir ' + diroutGrid)
		
		ascList = glob.glob(gp.workspace + "\\*.asc")
		for asc in ascList:
			
			# Extact variable of the file name
			variable = os.path.basename(asc).split("_")[0:1][0]
			month = os.path.basename(asc).split("_")[-1][0:2]
			
			print "    ---> Processing " + str(decVar[variable]) + " " + str(variable) + " " + str(year) + " " + str(month) + "\n"
			
			Modificar ruta de salidaaaa
			Hacer una carpeta temporal de entrada para pruebas
			dirout = diroutGrid + "\\" + str(year) str(decVar [variable]) + "_" + str(month)
			# splitLen = 129

			# input = open(asc, 'r').read().split('\n')

			# at = 1
			# for lines in range(0, len(input), splitLen):

				# # First, get the list slice
				# outputData = input[lines:lines+splitLen]

				# # Now open the output file, join the new slice with newlines
				# # and write it out. Then close the file.
				# output = open(dirout + str(at) + '.txt', 'w')
				# output.write('\n'.join(outputData))
				# output.close()

				# # Increment the counter
				# at += 1

			# for SecTxt in range (1, 31, 1):
				# InTxt = open(dirout + str(SecTxt) + '.txt', 'r')
				# outASC = open(dirout + str(SecTxt) + '.asc', 'w')
				# outASC.write('NCOLS 151\nNROWS 143\nXLLCORNER 0\nYLLCORNER 0\nCELLSIZE 0.44\nNODATA_VALUE 99\n')
				# outASC.close()

				# InTxt.readline()
				# InTxt.readline()

				# outASC = open(dirout + str(SecTxt) + '.asc', 'a')

				# for line in InTxt.readlines():
					# outASC.write(line)

				# InTxt.close()
				# outASC.close()

				# os.system('del ' + dirout + str(SecTxt) + '.txt')

				# # Convert ASCII to Raster
				# print 'Day ' + str(SecTxt)
				# gp.workspace = 'D:\\borrar\\CarlosNavarro\\'

				# if gp.exists(dirout + str(SecTxt)):
					# gp.Delete_management(dirout + str(SecTxt))
				# gp.ASCIIToRaster_conversion(dirout + str(SecTxt) + '.asc', dirout + str(SecTxt), "FLOAT")

if type == "monthly":

		# Set workspace
		gp.workspace = dirbase + "\\" + type + "_asciis\\" + str(year)
		
		# Create outputfolder
		dirout = dirbase + "\\" + type + "_grids\\" + str(year)
		if not os.path.exists(dirout):
			os.system('mkdir ' + dirout)
		
		ascList = glob.glob(gp.workspace + ".asc")
		for asc in ascList:
		  
			# Extact variable of the file name
			variable = os.path.basename(asc).split("_")[0:1]
			month = os.path.basename(asc).split("_")[-1]
		
			dirout = str(decDc [variable]) + "_" + str(month)
			splitLen = 129

			input = open(asc, 'r').read().split('\n')

			at = 1
			for lines in range(0, len(input), splitLen):

				# First, get the list slice
				outputData = input[lines:lines+splitLen]

				# Now open the output file, join the new slice with newlines
				# and write it out. Then close the file.
				output = open(dirout + str(at) + '.txt', 'w')
				output.write('\n'.join(outputData))
				output.close()

				# Increment the counter
				at += 1

			for SecTxt in range (1, 31, 1):
				InTxt = open(dirout + str(SecTxt) + '.txt', 'r')
				outASC = open(dirout + str(SecTxt) + '.asc', 'w')
				outASC.write('NCOLS 151\nNROWS 143\nXLLCORNER 0\nYLLCORNER 0\nCELLSIZE 0.44\nNODATA_VALUE 99\n')
				outASC.close()

				InTxt.readline()
				InTxt.readline()

				outASC = open(dirout + str(SecTxt) + '.asc', 'a')

				for line in InTxt.readlines():
					outASC.write(line)

				InTxt.close()
				outASC.close()

				os.system('del ' + dirout + str(SecTxt) + '.txt')

				# Convert ASCII to Raster
				print 'Day ' + str(SecTxt)
				gp.workspace = 'D:\\borrar\\CarlosNavarro\\'

				if gp.exists(dirout + str(SecTxt)):
					gp.Delete_management(dirout + str(SecTxt))
				gp.ASCIIToRaster_conversion(dirout + str(SecTxt) + '.asc', dirout + str(SecTxt), "FLOAT")

