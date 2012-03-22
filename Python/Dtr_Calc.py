# ------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 30/08/11
# -------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python DTR_Calc.py D:\Indir D:\Outdir"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
#Clear screen
os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     		Tmax calculation			 "  
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

gp.workspace = dirbase

for month in range (1, 12 + 1, 1):

	print "Calculating month " + str(month)
	InExpression = gp.workspace + "\\tean_" + str(month) + " - " + gp.workspace + "\\tmin_" + str(month)
	gp.SingleOutputMapAlgebra_sa(InExpression,  dirout + "\\dtr_" + str(month))
	
	InExpression2 = gp.workspace + "\\tmin_" + str(month) + " + " + dirout + "\\dtr_" + str(month) + " + " +  dirout + "\\dtr_" + str(month)
	gp.SingleOutputMapAlgebra_sa(InExpression2,  dirout + "\\tmax_" + str(month))
	gp.delete_management(dirout + "\\dtr_" + str(month))
			
print "Process done!!!"    
