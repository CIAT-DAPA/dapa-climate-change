# Nombre: Edward Guevara
# Fecha: 15-Abril-2010


# Import the arcgisscripting module and create the geoprocessor
import os, sys, datetime, fnmatch, string, arcgisscripting

gp = arcgisscripting.create(9.3)

# Check out ArcGIS Spatial Analyst extension license

os.system('cls')

# MAIN PROGRAM #
print '\n\n,,,Extract data of MRI Daily\n\n'

# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++
# Variables definition
FolderName=raw_input("\n. Please Enter Folder Name (SP0A, SN0A or SF0A):  ").upper()
programs_path = os.getcwd()
GRD_dir = 'F:\\MRI_Colombia\\OutExtract\\' + FolderName + '\\prec\\'

if FolderName == 'SN0A':
	print '\n  *** Note: First Date is 2015,1,1 and the last date is 2039,12,31 ***'
elif FolderName == 'SP0A':
	print '\n  *** Note: First Date is 1979,1,1 and the last date is 2003,12,31 ***'
elif FolderName == 'SF0A':
	print '\n  *** Note: First Date is 2075,1,1 and the last date is 2099,12,31 ***'
else:
	exit()

IniDateIn=raw_input("\n. Please Enter Initial Date (yyyy,mm,dd):  ").split(',')
IniDateD = datetime.date(int(IniDateIn[0]),int(IniDateIn[1]),int(IniDateIn[2]))
IniDateO = IniDateD.toordinal()

FinDateIn=raw_input("\n. Please Enter Final Date (yyyy,mm,dd):  ").split(',')
FinDateD = datetime.date(int(FinDateIn[0]),int(FinDateIn[1]),int(FinDateIn[2]))
FinDateO = FinDateD.toordinal() + 1

CVS_file = raw_input("\n. Intro path and name of .csv file:  ")
# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++

print "\n  Fecha Inicial: ", IniDateD.strftime("%Y.%m.%d") + ' <---> ' + " Fecha Final: ", FinDateD.strftime("%Y.%m.%d") + '\n'

def VariableClimate(VarCli):
	NextDateO = range(IniDateO, FinDateO, 1)

	outFile = open(programs_path + '\\' + CVS_file, "w")
	outFile.write('Raster,MIN,MAX,MEA,STD,CEX\n')
	
	for DateO in NextDateO:
		DateD = datetime.date.fromordinal(DateO)
		YearD = DateD.strftime("%Y")
		MonthD = DateD.strftime("%m")
		DayD = DateD.strftime("%d")

		if MonthD < 10:
			MonthD =  "0" + str(MonthD)

		if DayD < 10:
			DayD =  "0" + str(DayD)

		# Extract point value from raster
		RasterName = GRD_dir + YearD + '\\prec_' + YearD + MonthD + DayD
		print RasterName
		if gp.exists(RasterName):
			MIN = gp.GetRasterProperties_management(RasterName, "MINIMUM")
			MAX = gp.GetRasterProperties_management(RasterName, "MAXIMUM")
			MEA = gp.GetRasterProperties_management(RasterName, "MEAN")
			STD = gp.GetRasterProperties_management(RasterName, "STD")
			CEX = gp.GetRasterProperties_management(RasterName, "CELLSIZEX")

			outFile.write(RasterName + ',' + MIN.getoutput(0) + "," + MAX.getoutput(0) + "," + MEA.getoutput(0) + "," + STD.getoutput(0) + "," + CEX.getoutput(0) + "\n")

		else:
			outFile.write(RasterName + ',NO EXISTE\n')

	outFile.close()
			

VariableClimate('prec')
##VariableClimate('tmax')
##VariableClimate('tmin')

