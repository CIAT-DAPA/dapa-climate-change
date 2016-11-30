from sys import exit, argv
from os import path, mkdir

from sys import exit
from os import path, system, mkdir
from cdms2 import setNetcdfShuffleFlag, setNetcdfDeflateLevelFlag, setNetcdfDeflateFlag
from string import split
from datetime import datetime
from daily_stats_cdms_utils import MosaicFiles


RootDir = 'D:\CIAT\Projects\wocat\AR5_Global_Daily_25k'
model = "ACCESS1-0"
scens = 'historical'

fn_hist_tas = RootDir + "/" + model + "/junk/tas_day_BCSD" +"_historical_r1i1p1_"+ model + "_"

fname = fn_hist_tas
StComHis = 1950
EnComHis = 2005

styr = StComHis
enyr = EnComHis

OUTROOT = "D:\CIAT\Projects\wocat\AR5_Global_Daily_25k\out_stats"
OUTTEMP = "D:\CIAT\Projects\wocat\AR5_Global_Daily_25k"
sDic = {1:"TDJF", 2:"TMAM", 3:"TJJA", 4:"TSON"}

fn_nodir = split(fname, "/")[-1]
ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"