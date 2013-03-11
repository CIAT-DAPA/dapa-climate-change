# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 23-7-2012
# Purpose: Create tile from GCM data
# ----------------------------------------------------------------------------------

import arcpy, os, sys, string, glob, grid2asciitiff_tiles
from arcpy import env

#Syntax
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python tiles_countries.py S:\data\portals\analogues-gcm-data\ExtractByCountry b1 S:\data\portals\analogues-gcm-data\TilesByCountry 30s 2020_2049"
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
sres = sys.argv[2]
dirout = sys.argv[3]
resolution = sys.argv[4]
period = sys.argv[5]

os.system('cls')

countrytilelist = "tza", "zaf" #"eth", 
# "afg", "ago", "arg", "aus", "bfa", "bgd", "bhs", "blr", "bol", "bra", "bwa", "caf", "can", "chl", "chn", "civ", "cmr", "cog", "col",\
# "cub", "deu", "dnk", "dza", "ecu", "egy", "eri", "esh", "esp", "fin", "fji", "fra", "gab", "gbr", "gha", "gin", "gnq", "grc", "grl",\
# "guy", "hrv", "idn", "ind", "irn", "irq", "isl", "ita", "jpn", "kaz", "kgz", "khm", "kir", "kor", "lao", "lby", "mar", "mdg",\
# "mdv", "mex", "mli", "mlt", "mmr", "mng", "moz", "mrt", "mus", "mwi", "mys", "nam", "ncl", "ner", "nic", "nga", "nor", "npl",  "omn",\
# "pak", "per", "phl", "png", "pol", "prk", "prt", "pry", "pyf", "rom",  "sau", "sdn", "sen", "sgp", "sjm", "slb", "som", "swe",\
# "syr", "tca", "tcd", "tha", "tjk", "tkm", "tun", "tur", "ukr", "ury",  "uzb", "ven", "vnm", "vut", "yem", "zaf",\
# "zmb", "zwe", "nzl", "rus", "usa", "eth", "tza", "ken", "uga"

# "ala", "alb", "and", "are", "arm", "ata", "atg", "aut", "aze", "bdi", "bel", "ben", "bgr", "bhr", "bih", "blz", "bmu", "brb", "brn", "btn", "che", "com",\
# "cpv", "cri", "cyp", "cze", "dji", "dma", "dom", "est", "fro", "geo", "glp", "gmb", "gnb", "grd", "gtm", "guf", "hnd", "hti", "hun", "irl", "isr", "jam",\
# "jor", "kwt", "lbn", "lbr", "lca", "lie", "lka", "lso", "ltu", "lux", "lva", "mac", "mco", "mda", "mkd", "mnp", "mtq", "nld", "pan", "pri", "qat", "reu",\
# "rwa", "sle", "slv", "smr", "stp", "sur", "svk", "svn", "swz", "syc", "tgo", "ton", "twn", "vct", "wsm"

countryDic = {"bfa": "2 2 ", "bgd": "2 2 ", "bhs": "2 2 ", "blr": "2 2 ", "bwa": "2 2 ", "civ": "2 2 ", "cmr": "2 2 ", "cog": "2 2 ", "cub": "2 2 ",\
			"deu": "2 2 ", "dnk": "2 2 ", "eri": "2 2 ", "esh": "2 2 ", "gab": "2 2 ", "gha": "2 2 ", "gin": "2 2 ", "gnq": "2 2 ", "grc": "2 2 ", "guy": "2 2 ",\
			"hrv": "2 2 ", "irq": "2 2 ", "isl": "2 2 ", "ken": "2 2 ", "kgz": "2 2 ", "khm": "2 2 ", "kir": "2 2 ", "kor": "2 2 ", "lao": "2 2 ", "mus": "2 2 ",\
			"mwi": "2 2 ", "ncl": "2 2 ", "nic": "2 2 ", "npl": "2 2 ", "omn": "2 2 ", "pol": "2 2 ", "prk": "2 2 ", "pry": "2 2 ", "pyf": "2 2 ", "rom": "2 2 ",\
			"sen": "2 2 ", "sgp": "2 2 ", "slb": "2 2 ", "syr": "2 2 ", "tca": "2 2 ", "tjk": "2 2 ", "tun": "2 2 ", "uga": "2 2 ", "ury": "2 2 ",\
			"vut": "2 2 ", "yem": "2 2 ", "zwe": "2 2 ", "afg": "3 3 ", "ago": "3 3 ", "bol": "3 3 ", "caf": "3 3 ", "ecu": "3 3 ", "egy": "3 3 ",\
			"eth": "3 3 ", "fin": "3 3 ", "fra": "3 3 ", "gbr": "3 3 ", "ita": "3 3 ", "mar": "3 3 ", "mdg": "3 3 ", "mdv": "3 3 ",\
			"mmr": "3 3 ", "moz": "3 3 ", "mrt": "3 3 ", "mys": "3 3 ", "nam": "3 3 ", "ner": "3 3 ", "nga": "3 3 ", "phl": "3 3 ", "png": "3 3 ",\
			"som": "3 3 ", "swe": "3 3 ", "tcd": "3 3 ", "tha": "3 3 ", "tkm": "3 3 ", "tur": "3 3 ", "tza": "3 3 ", "ukr": "3 3 ", "uzb": "3 3 ",\
			"ven": "3 3 ", "vnm": "3 3 ", "zmb": "3 3 ", "col": "4 4 ", "esp": "4 4 ", "irn": "4 4 ", "lby": "4 4 ", "mli": "4 4 ", "mlt": "4 4 ",\
			"mng": "4 4 ", "nor": "4 4 ", "pak": "4 4 ", "per": "4 4 ", "prt": "4 4 ", "sau": "4 4 ", "sdn": "4 4 ", "dza": "5 5 ", "mex": "5 5 ", "sjm": "5 5 ",\
			"zaf": "5 5 ", "arg": "6 6 ", "idn": "6 6 ", "jpn": "6 6 ", "kaz": "6 6 ", "ind": "7 7 ", "bra": "9 9 ", "chl": "9 9 ",\
			"grl": "9 9 ", "aus": "10 10 ", "chn": "10 10 ", "fji": "11 11 ", "can": "13 13 ", "nzl": "13 13 ", "rus": "13 13 ", "usa": "13 13 "}
checkDic = {"bfa": "3", "bgd": "3", "bhs": "3", "blr": "3", "bwa": "3", "civ": "3", "cmr": "3", "cog": "3", "cub": "3",\
			"deu": "3", "dnk": "3", "eri": "3", "esh": "3", "gab": "3", "gha": "3", "gin": "3", "gnq": "3", "grc": "3", "guy": "3",\
			"hrv": "3", "irq": "3", "isl": "3", "ken": "3", "kgz": "3", "khm": "3", "kir": "3", "kor": "3", "lao": "3", "mus": "3",\
			"mwi": "3", "ncl": "3", "nic": "3", "npl": "3", "omn": "3", "pol": "3", "prk": "3", "pry": "3", "pyf": "3", "rom": "3",\
			"sen": "3", "sgp": "3", "slb": "3", "syr": "3", "tca": "3", "tjk": "3", "tun": "3", "uga": "3", "ury": "3",\
			"vut": "3", "yem": "3", "zwe": "3", "afg": "8", "ago": "8", "bol": "8", "caf": "8", "ecu": "8", "egy": "8",\
			"eth": "8", "fin": "8", "fra": "8", "gbr": "8", "ita": "8", "mar": "8", "mdg": "8", "mdv": "8",\
			"mmr": "8", "moz": "8", "mrt": "8", "mys": "8", "nam": "8", "ner": "8", "nga": "8", "phl": "8", "png": "8",\
			"som": "8", "swe": "8", "tcd": "8", "tha": "8", "tkm": "8", "tur": "8", "tza": "8", "ukr": "8", "uzb": "8",\
			"ven": "8", "vnm": "8", "zmb": "8", "col": "15", "esp": "15", "irn": "15", "lby": "15", "mli": "15", "mlt": "15",\
			"mng": "15", "nor": "15", "pak": "15", "per": "15", "prt": "15", "sau": "15", "sdn": "15", "dza": "24", "mex": "24", "sjm": "24",\
			"zaf": "24", "arg": "35", "idn": "35", "jpn": "35", "kaz": "35", "ind": "48", "bra": "80", "chl": "80",\
			"grl": "80", "aus": "99", "chn": "99", "fji": "120", "can": "168", "nzl": "168", "rus": "168", "usa": "168"}

for country in countrytilelist:

	if not os.path.exists(dirout + "\\SRES_" + sres + "\\" + country + "_tiles_countries_done.txt"):
		
		modellist = sorted(os.listdir(dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution)))
		
		print "~~~~~~~~~~~~~~~~~~~"
		print "  TILES COUNTRIES  "
		print "~~~~~~~~~~~~~~~~~~~"
		
		for model in modellist:
				
			if model == "current":
				arcpy.env.workspace = dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution) + "\\" + model
				diroutGrids = dirout + "\\Baseline\\" + country + "_" + str(resolution) + "\\" + model
			else:
				arcpy.env.workspace = dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution) + "\\" + model + "\\" + str(period)
				diroutGrids = dirout + "\\SRES_" + sres + "\\" + country + "_" + str(resolution) + "\\" + model + "\\" + str(period)
			
			print "\nProcessing",country,model,period,"\n"
			rasterList = arcpy.ListRasters("*", "GRID")
			for raster in rasterList:
				
				if os.path.basename(raster).split("_")[0] == "bio" or os.path.basename(raster).split("_")[0] == "prec" or os.path.basename(raster).split("_")[0] == "tmean" or os.path.basename(raster).split("_")[0] == "dtr":
					
					diroutGridsVar = diroutGrids + "\\" + os.path.basename(raster).split("_")[0]
					if not os.path.exists(diroutGridsVar):
						os.system('mkdir ' + diroutGridsVar)

					if not arcpy.Exists(diroutGridsVar + "\\" + raster + "_" + str(checkDic [country])):
						
						trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
						for trashfile in trashList:
							os.remove(trashfile)
						
						print "\tspliting .. ",raster
						
						rasterdeleteList = sorted(glob.glob(diroutGridsVar + "\\" + os.path.basename(raster) + "_*"))
						for rasterdelete in rasterdeleteList:
							arcpy.Delete_management(rasterdelete)
						
						arcpy.SplitRaster_management(raster, diroutGridsVar, raster + "_", "NUMBER_OF_TILES", "GRID", "#", str(countryDic [country]), "#", "0", "PIXELS", "#", "#")
						print "\t" + raster,"tiled"					
						
						trashList = sorted(glob.glob(diroutGridsVar + "\\" + raster + "*.*"))
						for trashfile in trashList:
							os.remove(trashfile)
						
					else:
						print "\t" + raster,"tiled"
				
		#Compressing and delete input files
		# os.system("7za a -mmt8 " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution) + ".zip " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution))
		# os.system("rmdir /s /q " + dirbase + "\\SRES_" + sres + "\\downscaled\\" + country + "_" + str(resolution))
		
		#Create check file
		checkTXT = open(dirout + "\\SRES_" + sres + "\\" + country + "_tiles_countries_done.txt", "w")
		checkTXT.close()

	else:
		
		print "\n\t",country," Tiled!"
	
	if os.path.exists(dirout + "\\SRES_" + sres + "\\" + country + "_tiles_countries_done.txt") and not os.path.exists(dirout + "\\SRES_" + sres + "\\" + country + "_grid2tiff_countries_done.txt"):
		
		##Scenarios
		grid2asciitiff_tiles.mainfunction(dirout, dirout, country, sres, period, resolution, "YES")
		
		## Compressing country files
		# print " \nCompressing country folder"
		# os.system("7za a -mmt12 " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution + ".zip " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution)
		# os.system("rmdir /s /q " + dirout + "\\SRES_" + sres + "\\" + country + "_" + resolution)
		# print " \nCompression done!"
		
		checkTXT = open(dirout + "\\SRES_" + sres + "\\" + country + "_grid2tiff_countries_done.txt", "w")
		checkTXT.close()
	
		print "\n",country," Tiles converted!"

	else:
		print "\n",country," Tiles converted!"
	
print "\n Process done!"
