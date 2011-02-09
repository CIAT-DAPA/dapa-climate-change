#-----------------------------------------------------------------------
# Description: Convert outputs PRECIS (.pp) to ASCII
# Author: Carlos Navarro
# Date: 13/09/10
#-----------------------------------------------------------------------

# Import system modules
import os, sys, glob, string, shutil

if len(sys.argv) < 6:
	os.system('clear')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - ie linux: python ConvertPP2Ascii.py /mnt/GIS-HD717/PrecisData/archive gen /mnt/GIS-HD716/climate_change/RCM_Data HadCM3Q3 SRES_A1B"
	sys.exit(1)

# --------------------------------------------------------------------------------------------------------------------------
# Notes:
# You must call ". setvars" on home/precis before run ConvertPP2Ascii.py script
# Change the tmpdir
# dirbase:  Path pp files
# runid:    First three letters of runid. The full list of runids are in trunk\dapa-climate-change\PRECIS\Progress_Runs_Precis.xls
# variable: Variable in the PRECIS model, corresponding to the STASH code. The full list are in
#           trunk\dapa-climate-change\PRECIS\Metereological_Variables_PRECIS.xlsx
# type:     The time period over which the data gas been processed and the amount of data in the file.
#           The possibilities are:
#               pa = Timeseries of daily data spanning 1 month
#               pm = Monthly average for 1 month
#               ps = 3-month seasonal average data for 1 season
#               py = Annual average data for 1 year
# Model:    The full list of models are in trunk\dapa-climate-change\PRECIS\PRECIS_Scenarios.txt
# Scenario: The full list of scenarios are in trunk\dapa-climate-change\PRECIS\PRECIS_Scenarios.txt
# dirout:   The structure of outputs files are <RCM_Data\PRECIS_datatype\GCM\Scenario\Year\GCM_scenario_variable_YYYYMM.asc>
#-----------------------------------------------------------------------------------------------------------------------------

# Arguments

dirbase = sys.argv[1]
runid = sys.argv[2]
#variable = sys.argv[3]
#type = sys.argv[3]
dirout = sys.argv[3]
gcm = sys.argv[4]
scenario = sys.argv[5]

# Create dirout and temporal folders

if not os.path.exists(dirout):
    os.system('mkdir ' + dirout)

os.system('clear')

print "\n"
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "                          PP PRECIS TO ASCII                       "
print "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "\n"

# Dictionary with single letter date stamp equivalences
decDc = {"e": 1940, "f": 1950, "g": 1960, "h": 1970, "i": 1980, "j": 1990, "k": 2000, "l": 2010,
         "m": 2020, "n": 2030, "o": 2040, "p": 2050, "q": 2060, "r": 2070, "s": 2080, "t": 2090}

# Dictionary month equivalences
monDc = {"jan": 1, "feb": 2, "mar": 3, "apr": 4, "may": 5, "jun": 6, 
         "jul": 7, "aug": 8, "sep": 9, "oct": 10, "nov": 11, "dec": 12}

# Standar diagnostic list
variables = ["00024", "00024.max", "00024.min", "00024.mmin", "02204", "03234", "03236", "03236.max", 
	     "03236.min", "03237", "03245", "03249", "03249.max", "03296", "03297", "03298", "03299",
	     "03312", "03313", "03510", "03511", "05216", "08208", "08223", "08225", "16204", "16222",
	     "00024.mmax", "00024.mmin", "03236.mmax", "03236.mmin", "03249.mmax"]

for variable in variables:

  print "    > Converting " + variable + " pp files to ascii \n"

  # Get a list of all runids 
  folList = glob.glob(dirbase + "/" + runid + "*")

  for fol in folList:

    tmpdir = "/data2/precis/Workspace"
    if not os.path.exists(tmpdir):
      os.system('mkdir ' + tmpdir)
    else:
      shutil.rmtree(tmpdir)
      os.system('mkdir ' + tmpdir)

    print "\n    > Searching  into  " + fol + "\n"

    # Get a list of all pp files into runid folder
    ppList = glob.glob(fol + "/" + str(variable) + "/" + fol.split("/")[-1] + "a.p*." + variable + ".pp")

    for pp in ppList:
      
      # Extact year of the file name
      type = os.path.basename(pp)[7:9]
      decade = os.path.basename(pp)[9:10]
      year = os.path.basename(pp)[10:11]
      month = os.path.basename(pp)[11:14]
      date = int(decDc [decade]) + int(year)

      #Creating outputs folders to ascii files by years
      if not os.path.exists(dirout + "/" + scenario):
	os.system('mkdir ' + dirout + "/" + scenario)
      if not os.path.exists(dirout + "/" + scenario + "/" + gcm):
	os.system('mkdir ' + dirout + "/" + scenario + "/" + gcm)

      # For daily data

      if type == "pa":

	if not os.path.exists(dirout + "/" + scenario + "/" + gcm + "/daily_asciis"):
	  os.system('mkdir ' + dirout + "/" + scenario + "/" + gcm + "/daily_asciis")

	diroutASC = dirout + "/" + scenario + "/" + gcm + "/daily_asciis/" + str(date)  
	if not os.path.exists(diroutASC):
	  os.system('mkdir ' + diroutASC)

	if int(monDc [month]) < 10 and not os.path.exists(diroutASC + "/" + variable + "_0" + str(monDc [month]) + ".asc"):

	  print "    ---> Processing " + gcm + " " + scenario + " " + variable + " " + str(date) + " " +  str(month)

	  # Remove a rm from pp fields
	  os.system("pprr -z -O /data2/precis/precis182/tmp/pprr.out -r 8 -e " + tmpdir + " -o " + os.path.basename(pp) + " " + pp)
	  print "    ---> Rim removed"

	  # Convert pp files to ascii format 
	  # Structure ascii filenames: variable_MM.asc
	  outASC = diroutASC + "/" + variable + "_0" + str(monDc [month]) + ".asc"
	  os.system("pp2ascii -f asciipp -O /data2/precis/precis182/tmp/pp2ascii.out -o " + outASC + " " + tmpdir + "/" + os.path.basename(pp))
	  print "    ---> Converted to Ascii \n"

	if int(monDc [month]) > 9 and not os.path.exists(diroutASC + "/" + variable + "_" + str(monDc [month]) + ".asc"):

	  print "    ---> Processing " + gcm + " " + scenario + " " + variable + " " + str(date) + " " +  str(month)

	  # Remove a rm from pp fields
	  os.system("pprr -z -O /data2/precis/precis182/tmp/pprr.out -r 8 -e " + tmpdir + " -o " + os.path.basename(pp) + " " + pp)
	  print "    ---> Rim removed"

	  # Convert pp files to ascii format 
	  # Structure ascii filenames: variable_MM.asc
	  outASC = diroutASC + "/" + variable + "_" + str(monDc [month]) + ".asc"
	  os.system("pp2ascii -f asciipp -O /data2/precis/precis182/tmp/pp2ascii.out -o " + outASC + " " + tmpdir + "/" + os.path.basename(pp))
	  print "    ---> Converted to Ascii \n"

      # For monthly data

      if type == "pm":

	if not os.path.exists(dirout + "/" + scenario + "/" + gcm + "/monthly_asciis"):
	  os.system('mkdir ' + dirout + "/" + scenario + "/" + gcm + "/monthly_asciis")

	diroutASC = dirout + "/" + scenario + "/" + gcm + "/monthly_asciis/" + str(date)  
	if not os.path.exists(diroutASC):
	  os.system('mkdir ' + diroutASC)

	if int(monDc [month]) < 10 and not os.path.exists(diroutASC + "/" + variable + "_0" + str(monDc [month]) + ".asc"):

	  print "    ---> Processing " + gcm + " " + scenario + " " + variable + " " + str(date) + " " +  str(month)

	  # Remove a rm from pp fields
	  os.system("pprr -z -O /data2/precis/precis182/tmp/pprr.out -r 8 -e " + tmpdir + " -o " + os.path.basename(pp) + " " + pp)
	  print "    ---> Rim removed"

	  # Convert pp files to ascii format 
	  # Structure ascii filenames: variable_MM.asc
	  outASC = diroutASC + "/" + variable + "_0" + str(monDc [month]) + ".asc"
	  os.system("pp2ascii -f asciipp -O /data2/precis/precis182/tmp/pp2ascii.out -o " + outASC + " " + tmpdir + "/" + os.path.basename(pp))
	  print "    ---> Converted to Ascii \n"

	if int(monDc [month]) > 9 and not os.path.exists(diroutASC + "/" + variable + "_" + str(monDc [month]) + ".asc"):

	  print "    ---> Processing " + gcm + " " + scenario + " " + variable + " " + str(date) + " " +  str(month)

	  # Remove a rm from pp fields
	  os.system("pprr -z -O /data2/precis/precis182/tmp/pprr.out -r 8 -e " + tmpdir + " -o " + os.path.basename(pp) + " " + pp)
	  print "    ---> Rim removed"

	  # Convert pp files to ascii format 
	  # Structure ascii filenames: variable_MM.asc
	  outASC = diroutASC + "/" + variable + "_" + str(monDc [month]) + ".asc"
	  os.system("pp2ascii -f asciipp -O /data2/precis/precis182/tmp/pp2ascii.out -o " + outASC + " " + tmpdir + "/" + os.path.basename(pp))
	  print "    ---> Converted to Ascii \n"

    #Remove temporal dir
    print "    > Removing temporal files"
    shutil.rmtree(tmpdir)

print "    > Done!!!"