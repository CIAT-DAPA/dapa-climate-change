# -----------------------------------------------------------------
# Author: Johannes Signer 
# Modified by: Carlos Navarro
# Date: September 21th, 2011
# Purpose: Produces output files from MarkSim orgnized by folders 
# -----------------------------------------------------------------

import os, sys

# set Pathes
# script to run marksim
script = "D:\\_scripts\\dapa-climate-change\\MarkSim-DSSAT\\1_runing_interpolations.py"
# where marksim located
marksim = "D:\_scripts\dapa-climate-change\MarkSim-DSSAT\lib\marksim.zip"
# where the results will be copied to
out_file = "D:\Workspace\MS_llanos\MS\OUTPUT\results"
dirout = "D:\\Workspace\\"
dirbase = "D:\Workspace\MS_llanos\_wc30s"
basename = "MS2_5min"

year = "2000"
# gcmlist = "worldclim", "bccr_bcm2_0"
gcm = "worldclim"

# run marskim
# for gcm in gcmlist:
os.system("python " + script + " " + dirbase + "\\" + year + "\\" + gcm + ".zip " + out_file + " " + marksim + " " + dirout + " " + basename )
