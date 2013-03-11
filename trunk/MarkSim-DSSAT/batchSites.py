#Read in a data file and apply MarkSimGCM python script

import sys, os
import csv

csvFile = sys.argv[1]
model = sys.argv[2]

# Dictionary models
decDc = {"avr": "average", "bcc": "bccr_bcm2_0", "cnr": "cnrm_cm3", "csi": "csiro_mk3_5", "ech": "mpi_echam5", "inm": "inm_cm3_0", "mir": "miroc3_2_medres"}

yearList = "2020", "2050"

for year in yearList: 
	dataIn = csv.reader(open(csvFile, 'rb'), delimiter=' ', quotechar=' ')
	for row in dataIn:
		rowData = ', '.join(row)
		siteName = rowData.split(",")[0]
		siteLat = rowData.split(",")[1]
		siteLong = rowData.split(",")[2]
		workspace = "D:\\MarkSimGCM\\TOR_GCM\\" + str(year) + "\\" + str(decDc[str(model)]) + "\\" + os.path.basename(csvFile)[:-4]
		#Running MarkSimGCM
		os.system("marksimgcm-v1.1.py D:/MarkSimGCM/worldll.mtg D:/MarkSimGCM/MarkDat D:/MarkSimGCM/gcm4data  " + workspace + " " + siteName + " " + model + " a2 " + str(year) + " 1234 99 " + siteLat + " " + siteLong)

#[0] = location of WorldClim 30s file FIJO
#[1] = location of marksim data FIJO 
#[2] = location of GCM data FIJO
#[3] = location of output workspace FIJO 
#[4] = model: ech, csi, avr, bcc, cnr, inm, mir VAR
#[5] = scenario: a1,b1,a2 FIJO
#[6] = year FIJO
#[7] = seed FIJO
#[8] = replications FIJO
#[9] = latitude 
#[10] = longitude
#[11] = country
