# Jaime Tarapues
# Calcula el peso total de todos los subdirectorios 
# python get_size_directory.py G:\ G:\jetarapues

import os,sys,time
from os import walk
dirbase = sys.argv[1]
out = sys.argv[2]

def get_size(start_path = '.'):
	try:
		total_size = 0
		for dirpath, dirnames, filenames in os.walk(start_path):
			for f in filenames:
				fp = os.path.join(dirpath, f)

				total_size += os.path.getsize(fp)

				# total_size += os.stat(fp).st_size
		if total_size > (536870912.0*2):
			total_siz = str(round((total_size/(536870912.0*2)),3))+"\t"+"gb"
		elif total_size >(1024*1024.0) and total_size<=(536870912.0*2):
			total_siz = str(round((total_size/(1024*1024.0)),3))+"\t"+"mb"
		else:
			total_siz = str(total_size)+"\t"+"bytes"
			
		return total_siz
	except:
		pass		

# print get_size(dirbase)	
file=out + "\\size_directories.txt"
outFile = open(file, "w")
outFile.write("Directory: "+dirbase+"\tDate:"+time.strftime("%d/%m/%Y")+"\n\nFOLDER" + "\t" + "SIZE" + "\t" + "UNIT" + "\n")


folderList = next(os.walk(dirbase))[1]
for folder in folderList:
	print folder+"\t"+get_size(dirbase+"\\"+folder)
	outFile = open(file , "a")
	outFile.write(folder+"\t"+get_size(dirbase+"\\"+folder)+'\n')

outFile.close()