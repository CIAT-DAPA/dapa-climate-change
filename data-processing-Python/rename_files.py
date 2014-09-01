# Author: Jaime Tarapues

import os, sys, glob, string, shutil, errno

# python D:\jaime\PYTHON\rename.py S:\portals\ccafs_climate\download_data\files\data\precis\precis_andes
 
dirbase = sys.argv[1]

# os.chdir(dirbase)
# lista = glob.glob("*.a")
# for w in lista:
	# print "renombrando: ", w
	# base = os.path.splitext(w)[0]
	# os.rename(w, base + ".asc")

	
for root, dirs, files in os.walk(dirbase):
	for name in files:
		if name.endswith((".zip", ".zip")):	
			# if len(name)==11:
			print name
			# os.rename(root + "\\"+ name, root + "\\"+ name[:-4]+ "0.txt")

			os.rename(root + "\\"+ name, root + "\\"+ name.replace("no_tile","andes_precis"))
			# str.replace
			
print "\tDONE!!"			