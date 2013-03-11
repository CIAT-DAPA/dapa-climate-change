# ---------------------------------------------------------------------------
# marksimgcm.py
# Created on: Vie Oct 31 2010 7:40 p.m.
# Modified Wed 08 - 2011. 
#   Ernesto Giron E. e.giron.e@gmail.com
#   GIS/RS Senior Consultant
# Usage:
# Description: MarkSim DSSAT weather file generator
# MarkSim DSSAT weather file generator.
# This application uses the well-known MarkSim application (Jones & Thornton 2000, Jones et al 2002)
# working off a 30 arc-second climate surface derived from WorldClim (Hijmans et al 2005).
# Point and click on the map and up to 99 WTG files are prepared ready for use with DSSAT.
# Download and unpack to a directory on your machine and they are ready for use with
# the DSSAT4 crop modelling system (Hoogenboom et al 2003).
# For further info about the model please contact to: Dr. Peter Jones.
# ---------------------------------------------------------------------------
import sys, os
import glob

from ctypes import *

ds = windll.LoadLibrary("D:\\MarkSimGCM\\Scripts\\marksimgcm.dll")

#This prints the arguments, just to double check
print "\n"
print "MarkSim run parameters"
print "-----------------------------------"
print "WorldClim file: " + sys.argv[1]
print "MarkSim data: " + sys.argv[2]
print "GCM data: " + sys.argv[3]
print "Output workspace: " + sys.argv[4]
print "Location name: " + sys.argv[5]
print "Global Climate Model: " + sys.argv[6]
print "Emissions scenario: " + sys.argv[7]
print "Target year: " + sys.argv[8]
print "Random seed: " + sys.argv[9]
print "Number of replications: " + sys.argv[10]
print "Latitude: " + sys.argv[11]
print "Longitude: " + sys.argv[12]


# Script arguments...
weights = [sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5], sys.argv[6], sys.argv[7],sys.argv[8], sys.argv[9], sys.argv[10], sys.argv[11], sys.argv[12]]

#[0] = location of WorldClim 30s file
#[1] = location of marksim data
#[2] = location of GCM data
#[3] = location of output workspace
#[4] = model: ech, csi, avr, bcc, cnr, inm, mir
#[5] = scenario: a1,b1,a2
#[6] = year
#[7] = seed
#[8] = replications
#[9] = latitude
#[10] = longitude

#Set local variables
#gp.Workspace = gp.scratchworkspace
#gp.OverwriteOutput = 1

#WorldClim 30 arc sec file
if str(weights[0]) == '#':
	#gp.AddError("Error: WorldClim 30 arc sec file not found: " + str(weights[0]))
	raise InputError("Error")
elif str(weights[0]) != '#':
	#wc = 'F:\peterj\MarkSimGCM\WorldClim\worldll.mtg'
	wc = str(weights[0])
	f_wc = c_char_p(wc)

#Path for MarSim data
if str(weights[1]) == '#':
	#gp.AddWarning("Path for MarSim data not found: " + str(weights[0]))
	#gp.AddError("Error: Path for MarkSim data not found: " + str(weights[1]))
	raise InputError("Error")
elif str(weights[1]) != '#':
	#mk = 'F:\peterj\MarkSimGCM\MarkDat'
	mk = str(weights[1])
	f_mk = c_char_p(mk)

#Path for GCM data
if str(weights[2]) == '#':
	#gp.AddError("Error: Path for GCM4 data not found: " + str(weights[2]))
	raise InputError("Error")
elif str(weights[2]) != '#':
	#gcm = 'F:/peterj/MarkSimGCM/gcm4data'
	gcm = str(weights[2])
	f_gcm = c_char_p(gcm)

#Output Directory with PlaceName  - default PlaceNameUnknown
if str(weights[3]) == '#':
	#gp.AddError("Error: Place Name is Unknown: " + str(weights[3]))
	raise InputError("Error")
elif str(weights[3]) != '#':
	#place = 'Cali'
	ws = str(weights[3]) 
	#f_place = c_char_p(place)
	#Create directory with place name in scratch workspace
	#outdir = 'C:\Workspace\ILRI\Scratch' #+ place
	#outdir = 'c:/arcgisserver/arcgisjobs/ilri/marksimgcmv1_gpserver/j9fc39b47fd8845f7bda5ec103d96c2a7/scratch'
	#outdir = gp.scratchworkspace + "\\" + place
	if not os.path.exists(ws):
		os.makedirs(ws)
	
#Site name (merged with workspace)
if str(weights[4]) == '#':
	#gp.AddError("Error: Place Name is Unknown: " + str(weights[3]))
	raise InputError("Error")
elif str(weights[4]) != '#':
	#place = 'Cali'
	site = str(weights[4])
	#f_place = c_char_p(place)
	#Create directory with place name in scratch workspace
	#outdir = gp.scratchworkspace + "\\" + place
	outdir = ws + "\\" + site
	if not os.path.exists(outdir):
		os.makedirs(outdir)
	f_out = c_char_p(outdir)
    #gp.AddMessage("Output Directory: "+ str(outdir))

#Model
if str(weights[5]) == '#':
	#gp.AddError("Error: Model is Unknown: " + str(weights[4]))
	raise InputError("Error")
elif str(weights[5]) != '#':
	#model = 'ech'
	model = str(weights[5])
	f_model = c_char_p(model)

#Scenario
if str(weights[6]) == '#':
	#gp.AddError("Error: Scenario is Unknown: " + str(weights[5]))
	raise InputError("Error")
elif str(weights[6]) != '#':
	#scenario = 'a1'
	scenario = str(weights[6])
	f_scn = c_char_p(scenario)

#Year of Simulation
if str(weights[7]) == '#':
	#gp.AddError("Error: Year of Simulation is Unknown: " + str(weights[6]))
	raise InputError("Error")
elif str(weights[7]) != '#':
	year = c_int(int(weights[7]))

# Seed
# seed = c_int(int(1234))
if str(weights[8]) == '#':
	#gp.AddError("Error: Seed is Unknown: " + str(weights[7]))
	raise InputError("Error")
elif str(weights[8]) != '#':
	seed = c_int(int(weights[8]))
    
#Number of Replications
if str(weights[9]) == '#':
	#gp.AddError("Error: Number of Replications is Unknown: " + str(weights[8]))
	raise InputError("Error")
elif str(weights[9]) != '#' and eval(str(weights[9])) < 100:
	#hasta 99 replicas
	n = c_int(int(weights[9]))

#Site_Latitude
if str(weights[10]) == '#':
	#gp.AddError("Error: Site Latitude: " + str(weights[9]))
	raise InputError("Error")
elif eval(str(weights[10])) > -90 and eval(str(weights[10])) < 90:
	#elif str(weights[9]) != '#':
	#lat = c_float(float(52.73))
	lat = c_float(float(weights[10]))
else:
	#gp.AddWarning("Site Latitude number incorrect: " + str(weights[9]))
	raise InputError("Error")

#Site_Longitude
if str(weights[11]) == '#':
	#gp.AddError("Error: Site Longitude: " + str(weights[10]))
	raise InputError("Error")
elif eval(weights[11]) > -180 and eval(weights[11]) < 180:
	#elif str(weights[9]) != '#':
	#lng = c_float(float(-2.99))
	lng = c_float(float(weights[11]))
else:
	#gp.AddWarning("Site Longitude number incorrect: " + str(weights[10]))
	raise InputError("Error")

#Return_Code
rc = c_int()

ds.MARKSIMGCM.restype = None

ds.MARKSIMGCM.argtypes=[c_char_p, c_char_p, c_char_p, c_char_p, c_char_p, c_char_p, POINTER(c_int), POINTER(c_int), POINTER(c_int), POINTER(c_float), POINTER(c_float), POINTER(c_int)]

#print "Listo para Calcular\n"
ds.MARKSIMGCM(f_wc, f_mk, f_gcm, f_out, f_model, f_scn, year, seed, n, lat, lng, rc)

#gp.AddWarning("Return Code: " + str(rc.value))

def getCode(rc):
	return {
		0: 'Process finished successfully',
		1: 'construct_clx: Cannot construct clx with an unrotated climate record',
		2: 'camdon: Radiation out of range in camdon, cannot be negative',
		3: 'avs: Non convergence in root',
		4: 'avs: Av unusually high',
		5: 'rainseq_direct: You have not called markset_direct',
		6: 'findclus: You have not loaded the cluster data with loadclus',
		7: 'loadclus: Cluster seed file not available, Make sure path is correct',
		8: 'loadclus: Error opening betas.out file',
		9: 'loadclus: Error reading betas.out file',
		10: 'loadclus: Error opening ps.out file',
		11: 'loadclus: Error reading ps.out file',
		12: 'loadclus: Error reading sds.out file',
		13: 'loadclus: Error opening dlag1.out file',
		14: 'loadclus: Error opening sds.out file',
		15: 'loadclus: Error reading dlag1.out file',
		16: 'loadclus: Error reading dlag2.out file',
		17: 'loadclus: Error reading dlag3.out file',
		18: 'loadclus: Error opening avmats.cor file',
		19: 'loadclus: Error reading avmats.cor file',
		20: 'loadclus: Error reading cluster seed file',
		21: 'read_GCM_data: Error opening GCM data, check path, model or scenario',
		22: 'read_GCM_data: Error reading GCM data, file integrity',
		23: 'nearest_met_record: Search for met record failed',
		24: 'gcm4: Data not loaded',
		25: 'gcm4: No GCM points within range',
		26: 'dcpmat: Variance-covariance matrix singular',
		27: 'dcpmat: Variance-covariance matrix not positive definite',
		28: 'dcpmat: Singularity in lower triangular matrix. This is not possible',
		29: 'probit: Probability out of range in probit',
		100: 'marksimgcm: Unable to find climate at pixel',
		101: 'marksimgcm: Subdirectory already exists',
		102: 'marksimgcm : Unable to create directory',
		103: 'marksimgcm: Unable to open climate output file',
		104: 'marksimgcm: Error writing climate output file'
	}[rc]

#output dll values
returncode = getCode(rc.value)
print returncode

def isfloat(s):
	try:
		float(s)
		return True
	except ValueError:
		return False

## Exportar los Resultados a un archivo log
jsonfile = outdir + "\\marksimgcmresults.log"
# print "Writing to file: %s" % jsonfile

# Abriendo archivo para escribir resultados
file = open(jsonfile, 'w')
file.write("\"version\": \"1.0\", \"encoding\": \"UTF-8\", \n")
file.write("\"developed by\": \"Ernesto Giron and Peter Jones - ILRI - 2010.\", \n")
file.write("\"inputs\": \n")
file.write("				lat: "+ str(lat.value) +",     \n")
file.write("				lng: "+ str(lng.value) +",  \n")
file.write("				Model: "+ model +",     \n")
file.write("				Scenario: "+ scenario +",     \n")
file.write("				Year: "+ str(year.value) +", \n")
file.write("				Seed: "+ str(seed.value) +", \n")
file.write("				Number of Replications: '"+ str(n.value) +"',   \n")
file.write("				Site: '"+  site  +"',   \n")
file.write("				Return Code: "+ str(rc.value) +"   \n")
file.write("				\n")
file.write("\"outputs\": \n")
file.write("				outdir: '"+ outdir +"',     \n")
file.write("				msgrc: '"+ returncode +"',  \n")
file.write("				\n")
file.write("\n")
file.close()

del rc
