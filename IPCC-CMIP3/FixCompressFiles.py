periodDc = {"2010_2039": "2020s", "2020_2049": "2030s", "2030_2059": "2040s", "2040_2069": "2050s", "2050_2079": "2060s", "2060_2089": "2070s", "2070_2099": "2080s"}

txtFile = "G:\jtarapues\sres_a1b_asc_test_compress_files.txt"
txtFile = open(txtFile, "r")
for line in txtFile:
	if line.find("Processing") == 0:
		inZip = line.split(" ")[2]
	
	elif line.find("Error: Can not open file as archive") == 0:
		print "Corregir todo el archivo", a 
		os.remove(inZip)
		
		res = os.path.basename(inZip).split("_")[-4]
		sres = os.path.basename(inZip).split("_")[-7]
		var =  os.path.basename(inZip).split("_")[-5]
		period =  os.path.basename(inZip).split("_")[-6]
		model = '_'.join(os.path.basename(inZip).split("_")[:-8])

		gridDir = "S:\data\gcm\cmip3\downscaled\\sres_" + sres + "\\Global_" + period + "\\" + model + "\\" + periodDc[str(period)]
		rasterlist(var + "*" + , "GRID")
		for raster in rasterlist:
			crear carpeta por variable
				7z ascFile 
				
	elif line.find("Data Error") == 0 or line.find("CRC Failed") == 0:	
		print "Corregir un archivo", a 
		os.system("7z d " + inZip + line.split(" ")[1] -r)
		
		res = os.path.basename(inZip).split("_")[-4]
		sres = os.path.basename(inZip).split("_")[-7]
		var =  os.path.basename(inZip).split("_")[-5]
		period =  os.path.basename(inZip).split("_")[-6]
		model = '_'.join(os.path.basename(inZip).split("_")[:-8])

		gridFile = "S:\data\gcm\cmip3\downscaled\\sres_" + sres + "\\Global_" + period + "\\" + model + "\\" + periodDc[str(period)] + "\\" + line.split(" ")[1][:-4]
		grid2ascii(gridFile, ascFile)
		7z ascFile 

		