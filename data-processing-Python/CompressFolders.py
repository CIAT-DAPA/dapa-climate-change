import os, sys, glob, string
# python CompressFolders.py E:\yapu_climate_risk\indices
dirbase = sys.argv[1]

dsList = glob.glob(dirbase + "\\*")
for ds in dsList:

    print ds
    os.system("C:\Program Files\7-Zip\7z.exe a -mmt=24 " + ds + ".zip " + ds)
    # os.system("rmdir /s/q " + ds)

