# Nombre: Edward Guevara
# Fecha: 15-Abril-2010
# Proposito: Extrae datos Diarios TRMM a partir de archivo .csv con coordenadas

# Import the arcgisscripting module and create the geoprocessor
import os, sys, datetime, fnmatch, string, arcgisscripting

gp = arcgisscripting.create(9.3)

# Check out ArcGIS Spatial Analyst extension license

os.system('cls')

# MAIN PROGRAM #
print '\n\n\t\t\tExtract data of MRI Daily\n\n'

# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++
# Variables definition
FolderName=raw_input("\n. Please Enter Folder Name (SN0A or SP0A):  ").upper()
programs_path = os.getcwd()
GRD_dir = 'E:\\MRI_Colombia\\OutExtract\\' + FolderName + '\\'
OUT_dir = programs_path + '\\OutExtract\\' + FolderName + '\\'

if FolderName == 'SN0A':
	print '\n  *** Note: First Date is 2015,1,1 and the last date is 2039,12,31 ***'
elif FolderName == 'SP0A':
	print '\n  *** Note: First Date is 1979,1,1 and the last date is 2003,12,31 ***'
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

### Making Folders
if not os.path.exists(OUT_dir):
	os.system('md ' + OUT_dir)

print "\n  Fecha Inicial: ", IniDateD.strftime("%Y.%m.%d") + ' <---> ' + " Fecha Final: ", FinDateD.strftime("%Y.%m.%d") + '\n'

def VariableClimate(VarCli):
	NextDateO = range(IniDateO, FinDateO, 1)

	inFile = open(CVS_file, "r")
	inFile.readline()

	for line in inFile.readlines():
		element = line[:-1].split(',')
		station = element[0]
		latitude = element[1]
		longitude = element[2]
		print '\n--> Station: ' + station
		print '    Latitude: ' + latitude + ', Longitude: ' + longitude
		print '    Variable: ' + VarCli
		print '... proccessing' 

		csvFile = station + '.csv'

		if not os.path.exists(OUT_dir + csvFile):

			outFile = open(OUT_dir + csvFile, "w")
			outFile.write('DATE,MAX,MIN\n')
			
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
				RasterValueMax = GRD_dir + 'Tmax\\' + YearD + '\\Tmax' + '_' + YearD + MonthD + DayD
				RasterValueMin = GRD_dir + 'Tmin\\' + YearD + '\\Tmin' + '_' + YearD + MonthD + DayD
				
				if gp.exists(RasterValueMax) and gp.exists(RasterValueMin):
					PointValueMax = gp.GetCellValue_management(RasterValueMax, str(longitude) + ' ' + str(latitude), "1")
					ValuePointMax =  gp.GetMessages(1)[:-1]
					
					PointValueMin = gp.GetCellValue_management(RasterValueMin, str(longitude) + ' ' + str(latitude), "1")
					ValuePointMin =  gp.GetMessages(1)[:-1]
					
					if str(ValuePointMax) == 'NoData' or float(ValuePointMax) >= 100 or float(ValuePointMax) <= -100:
						ValuePointMax = '-99'
						
					if str(ValuePointMin) == 'NoData' or float(ValuePointMin) >= 100 or float(ValuePointMin) <= -100:
						ValuePointMin = '-99'
					
					outFile.write(MonthD + '/' + DayD + '/' + YearD + ',' + ValuePointMax + ',' + ValuePointMin + '\n')
				else:
					outFile.write(MonthD + '/' + DayD + '/' + YearD + ',-99.00' + ',-99.00' + '\n')

			outFile.close()
			
	inFile.close()

##VariableClimate('prec')
VariableClimate('Temp')
##VariableClimate('tmin')
##
