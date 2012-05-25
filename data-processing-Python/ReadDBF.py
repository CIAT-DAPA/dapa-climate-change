from dbfpy.dbf import Dbf
import string, glob, os
dirbase = "D:\Workspace\PiuraTumbes\_extract_CRU3_1_tmp"
dbfList = sorted(glob.glob(dirbase + "\\*.dbf"))

outtxtFile = dirbase + "\\__extract_CRU3_1_tmp.txt"
if os.path.isfile(outtxtFile):
    outFile = open(outtxtFile, "a")
else:
    outFile = open(outtxtFile, "w")
outFile.write("Variable\tMonth\tTumbes\tPiura\n")

for dbf in dbfList:

    dbfFile = Dbf(dbf, True)
    outFile.write(os.path.basename(dbf)[4:-4].split("_")[0] + "\t" + os.path.basename(dbf)[4:-4].split("_")[1] +"\n")
    for rec in dbfFile:
        outFile.write("a" + str(rec[3]) + "\n")
##        for fldName in dbfFile.fieldNames:
##            outFile.write(fldName + "\t" + str(rec[fldName]) + "\n")

outFile.close()
dbfFile.close()