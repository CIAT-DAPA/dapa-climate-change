# ------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/08/11
# -------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python TemperatureCalc.py D:\Workspace\Requests\Jvalencia\Ensemble\2010_2039 D:\Workspace\Requests\Jvalencia\Ensemble\2010_2039 tmean"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
variable = sys.argv[3]
#Clear screen
os.system('cls')
gp.CheckOutExtension("Spatial")

if variable == "tmean":

	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	print "     		Tmean calculation			 "  
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

	gp.workspace = dirbase

	for month in range (1, 12 + 1, 1):

		print "Calculating month " + str(month)
		InExpression = "(" + gp.workspace + "\\tmax_" + str(month) + " + " + gp.workspace + "\\tmin_" + str(month) + ") / 2"
		gp.SingleOutputMapAlgebra_sa(InExpression,  dirout + "\\tmean_" + str(month))
					
	print "Process done!!!"    

if variable == "dtr":

	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	print "     		DTR calculation			 "  
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

	gp.workspace = dirbase

	for month in range (1, 12 + 1, 1):

		print "Calculating month " + str(month)
		InExpression = gp.workspace + "\\tmax_" + str(month) + " - " + gp.workspace + "\\tmin_" + str(month)
		gp.SingleOutputMapAlgebra_sa(InExpression,  dirout + "\\dtr_" + str(month))

		
if variable == "tmax":
		
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	print "     		Tmax calculation			 "  
	print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

	gp.workspace = dirbase

	for month in range (1, 12 + 1, 1):
	
		print "Calculating month " + str(month)
		if gp.Exists(gp.workspace + "\\dtr" + str(month)):
			InExpression2 = gp.workspace + "\\tmin_" + str(month) + " + " + dirout + "\\dtr_" + str(month) + " + " +  dirout + "\\dtr_" + str(month)
			gp.SingleOutputMapAlgebra_sa(InExpression2,  dirout + "\\tmax_" + str(month))

		else:
			print "Calculating month " + str(month)
			InExpression = gp.workspace + "\\tmax_" + str(month) + " - " + gp.workspace + "\\tmin_" + str(month)
			gp.SingleOutputMapAlgebra_sa(InExpression,  dirout + "\\dtr_" + str(month))
			
			InExpression2 = gp.workspace + "\\tmin_" + str(month) + " + " + dirout + "\\dtr_" + str(month) + " + " +  dirout + "\\dtr_" + str(month)
			gp.SingleOutputMapAlgebra_sa(InExpression2,  dirout + "\\tmax_" + str(month))
			
	print "Process done!!!"    
