# Description: Convert units for monthly datasets of PRECIS
# Author: Carlos Navarro
# Date: 16/07/12

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ConvertMonthlyUnits.py L:\climate_change\RCM_Data SRES_A1B ECHAM5 D:\climate_change\RCM_Data_1 monthly YES"
	sys.exit(1)

dirbase = sys.argv[1]
sres = sys.argv[2]
model = sys.argv[3]
dirout = sys.argv[4]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
frec = sys.argv[5]
juliano = sys.argv[6]


gp.CheckOutExtension("Spatial")
gp.toolbox="management"
os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     CONVERT MONTHLY UNITS PRECIS     "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

yearlist = sorted(os.listdir(dirbase + "\\" + sres + "\\" + str(model) + "\\" + frec + "_grids"))

for year in yearlist:
	
	if (int(year)%4==0 and not int(year)%100==0) or int(year)%400==0:
		daysdic = {"01": 31, "02": 29, "03": 31, "04": 30, "05": 31, "06": 30, "07": 31, "08": 31, "09": 30, "10": 31, "11": 30, "12": 31}
	else:
		daysdic = {"01": 31, "02": 28, "03": 31, "04": 30, "05": 31, "06": 30, "07": 31, "08": 31, "09": 30, "10": 31, "11": 30, "12": 31}
		
	fluxlist = "Prec", "EvCR", "EvPotR", "EvSS", "SubSR", "TransR"
	for var in fluxlist:
		
		gp.workspace = dirbase + "\\" + sres + "\\" + str(model) + "\\" + frec + "_grids" + "\\" + str(year) + "\\" + var
		outgriddir =  dirout + "\\" + sres + "\\" + str(model) + "\\" + frec + "_grids" + "\\" + str(year)
		if not os.path.exists(outgriddir):
			os.system('mkdir ' + outgriddir)
		
		print "Converting units of ", var, frec + " Grids", " (" + model + "," + str(year) + ")\n"
		
		gridlist = sorted(gp.ListRasters("", "GRID"))
		for grid in gridlist:
			
			month = os.path.basename(grid).split("_")[1]
			
			print frec, str(year), grid
			if not gp.Exists(outgriddir + "\\" + grid):
				if juliano == "YES":
					InExpression = "abs(" + grid + " * 86400 * " + str(daysdic [month]) + ")"
				else:
					InExpression = "abs(" + grid + " * 2592000)"
				
				gp.SingleOutputMapAlgebra_sa(InExpression, outgriddir + "\\" + grid)
			
		print "\nDone!\n"
	
	templist = "TSmean", "TSmmax", "TSmmin", "Tmean1_5", "Tmmax1_5", "Tmmin1_5"
	for var in templist:
		
		gp.workspace = dirbase + "\\" + sres + "\\" + str(model) + "\\" + frec + "_grids" + "\\" + str(year) + "\\" + var
		outgriddir =  dirout + "\\" + sres + "\\" + str(model) + "\\" + frec + "_grids" + "\\" + str(year)
		if not os.path.exists(outgriddir):
			os.system('mkdir ' + outgriddir)
		
		print "Converting units of ", var, frec + " Grids", " (" + model + "," + str(year) + ")\n"
		
		gridlist = sorted(gp.ListRasters("", "GRID"))
		for grid in gridlist:
			
			month = os.path.basename(grid).split("_")[1]
			
			if os.path.basename(grid).split("_")[0] == "tmean1":
				gridvar = "tmean"
				month = os.path.basename(grid).split("_")[2]
			elif os.path.basename(grid).split("_")[0] == "tmmax1":
				gridvar = "tmax"
				month = os.path.basename(grid).split("_")[2]
			elif os.path.basename(grid).split("_")[0] == "tmmin1":
				month = os.path.basename(grid).split("_")[2]
				gridvar = "tmin"
			else:
				gridvar = os.path.basename(grid).split("_")[0]
				month = os.path.basename(grid).split("_")[1]
				
			print frec, str(year), grid
			if not gp.Exists(outgriddir + "\\" + gridvar + "_" + month):

				InExpression = "int((" + grid + " - 273.15) * 10 + 0.5)"
				gp.SingleOutputMapAlgebra_sa(InExpression, outgriddir + "\\" + gridvar + "_" + month)
			
		print "\nDone!\n"

	otherlist = "RHum1_5", "SHum1_5", "CloudAm", "EvPotF1", "EvPotF2", "Press", "SLHeat", "SoilMAF", "SoilMRZ", "Wsmean", "Wsmmax"
	for var in otherlist:
		
		gp.workspace = dirbase + "\\" + sres + "\\" + str(model) + "\\" + frec + "_grids" + "\\" + str(year) + "\\" + var
		outgriddir =  dirout + "\\" + sres + "\\" + str(model) + "\\" + frec + "_grids" + "\\" + str(year)
		if not os.path.exists(outgriddir):
			os.system('mkdir ' + outgriddir)
		
		print "Converting units of ", var, frec + " Grids", " (" + model + "," + str(year) + ")\n"
		
		gridlist = sorted(gp.ListRasters("", "GRID"))
		for grid in gridlist:
			
			if os.path.basename(grid).split("_")[0] == "rhum1":
				gridvar = "rhum"
				month = os.path.basename(grid).split("_")[2]
			elif os.path.basename(grid).split("_")[0] == "shum1":
				gridvar = "shum"
				month = os.path.basename(grid).split("_")[2]
			else:
				gridvar = os.path.basename(grid).split("_")[0]
				month = os.path.basename(grid).split("_")[1]
			
			print frec, str(year), grid
			if not gp.Exists(outgriddir + "\\" + gridvar + "_" + month):

				gp.copy_management(grid, outgriddir + "\\" + gridvar + "_" + month)
		
		print "\nDone!\n"
		
print "\nProcess done! \n"
