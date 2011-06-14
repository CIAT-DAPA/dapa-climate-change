import os, arcgisscripting

gp = arcgisscripting.create(9.3)

splitLen = 145		 # 145 lines per file
outputBase = 'output_' # output.1.txt, output.2.txt, etc.

# This is shorthand and not friendly with memory
# on very large files (Sean Cavanagh), but it works.
input = open('00001_12.asc', 'r').read().split('\n')

at = 1
for lines in range(0, len(input), splitLen):
	# First, get the list slice
	outputData = input[lines:lines+splitLen]

	# Now open the output file, join the new slice with newlines
	# and write it out. Then close the file.
	output = open(outputBase + str(at) + '.txt', 'w')
	output.write('\n'.join(outputData))
	output.close()

	# Increment the counter
	at += 1

for SecTXT in range (1, 31, 1):
	inTXT = open(outputBase + str(SecTXT) + '.txt', 'r')
	outASC = open(outputBase + str(SecTXT) + '.asc', 'w')
	outASC.write('NCOLS 151\nNROWS 143\nXLLCORNER 0\nYLLCORNER 0\nCELLSIZE 0.44\nNODATA_VALUE 99\n')
	outASC.close()

	inTXT.readline()
	inTXT.readline()

	outASC = open(outputBase + str(SecTXT) + '.asc', 'a')

	for line in inTXT.readlines():
		outASC.write(line)

	inTXT.close()
	outASC.close()

	os.system('del ' + outputBase + str(SecTXT) + '.txt')

	# Convert ASCII to Raster
	print 'Day ' + str(SecTXT)
	gp.workspace = 'D:\\borrar\\CarlosNavarro\\'

	if gp.exists(outputBase + str(SecTXT)):
		gp.Delete_management(outputBase + str(SecTXT))
	gp.ASCIIToRaster_conversion(outputBase + str(SecTXT) + '.asc', outputBase + str(SecTXT), "FLOAT")
	