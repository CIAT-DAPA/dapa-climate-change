# Nombre: Edward Guevara
# Fecha: 11Octubre-2010
# Proposito: Corta datos MRI a partir de mascara

# Import the arcgisscripting module and create the geoprocessor
import os, sys, datetime, fnmatch, string, arcgisscripting

gp = arcgisscripting.create(9.3)

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

# Setting OverWriteOutput to True allows geoprocessing tools to overwrite
#    the output if it already exists.
gp.OverWriteOutput = 1

os.system('cls')

# MAIN PROGRAM #
print '\n\n\t\t\tExtract data of MRI Daily\n\n'

# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++
# Variables definition
FolderName=raw_input("\n. Please Enter Folder Name (SF0A, SN0A or SP0A):  ").upper()
programs_path = os.getcwd()
GRD_dir = '\\\\172.22.33.79\\geodata\\MRIData\\MRIAAIGrid\\' + FolderName + '\\'
OUT_dir = programs_path + '\\OutExtract\\' + FolderName + '\\'

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

mask_cut = raw_input("\n. Intro path of mask cut:  ")
# +++++++++++++++++++++++++++++++++++++++++++++++# +++++++++++++++++++++++++++++++++++++++++++++++

### Making Folders
if not os.path.exists(OUT_dir):
	os.system('md ' + OUT_dir)

print "\n  Fecha Inicial: ", IniDateD.strftime("%Y.%m.%d") + ' <---> ' + " Fecha Final: ", FinDateD.strftime("%Y.%m.%d") + '\n'

def VariableClimate(VarCli):
	NextDateO = range(IniDateO, FinDateO, 1)

	for DateO in NextDateO:
		DateD = datetime.date.fromordinal(DateO)
		YearD = DateD.strftime("%Y")
		MonthD = DateD.strftime("%m")
		DayD = DateD.strftime("%d")

		if MonthD < 10:
			MonthD =  "0" + str(MonthD)

		if DayD < 10:
			DayD =  "0" + str(DayD)

		# Extract by mask raster
		InMask = gp.Mask
		InRaster = GRD_dir + 'OUT_' + YearD + MonthD + '010000\\' + VarCli + '_' + MonthD + '_' + DayD
##			print '    . OUT_' + YearD + MonthD + '010000\\' + VarCli + '_' + MonthD + '_' + DayD

		if os.path.exists(GRD_dir + 'OUT_' + YearD + MonthD + '010000\\' + VarCli + '_' + DayD + '.asc.gz'):
			print 'Existe ' + VarCli + '_' + DayD + '.asc.gz'
			os.system('7za e -o'+ OUT_dir + ' ' + GRD_dir + 'OUT_' + YearD + MonthD + '010000\\' + VarCli + '_' + DayD + '.asc.gz')
			InRaster = OUT_dir + VarCli + '_' + DayD + '.asc'
			OutPointFeatures = OUT_dir + VarCli + '_' + DayD + '.shp'
			tmpRaster = OUT_dir + YearD + '\\t_day_' + YearD[2:] + '' + MonthD + '' + DayD
			OutRaster = OUT_dir + YearD + '\\prec_' + YearD + MonthD + DayD

			if not os.path.exists(OUT_dir + YearD):
				os.system('md ' + OUT_dir + YearD)

			if not gp.exists(OutRaster):
				gp.Extent = mask_cut
				gp.SnapRaster = mask_cut
				gp.Mask = mask_cut

				# Process: RasterToPoint_conversion
				gp.RasterToPoint_conversion(InRaster, OutPointFeatures)

				# Process: Spline
				gp.Spline_sa(OutPointFeatures, "GRID_CODE", tmpRaster, "0.0416666666667", "REGULARIZED", "0", "4")

				# Process: Con
				gp.Con_sa(tmpRaster, "0", OutRaster, tmpRaster, "VALUE < 0")

				# Deleted selected OutPointFeatures
				gp.delete_management(OutPointFeatures)
				# Deleted selected tmpRaster
				gp.delete_management(tmpRaster)

			os.system('del /F /Q ' + InRaster)
			
##			gp.ExtractByMask_sa(InRaster, InMask, OutRaster)

		else:
			print "NO EXISTE " + GRD_dir + 'OUT_' + YearD + MonthD + '010000\\' + VarCli + '_' + DayD + '.asc.gz'

VariableClimate('prec')
##VariableClimate('tmax')
##VariableClimate('tmin')

