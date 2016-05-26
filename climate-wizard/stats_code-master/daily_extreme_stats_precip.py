#!/usr/local/cdat5.2/bin/python

"""Module for computing precipitation extreme stats mostly using CDO utilities"""

from sys import exit
from os import path, system, mkdir
from cdms2 import setNetcdfShuffleFlag, setNetcdfDeflateLevelFlag, setNetcdfDeflateFlag
from string import split
from datetime import datetime
from daily_stats_cdms_utils import MosaicFiles

setNetcdfShuffleFlag(0)
setNetcdfDeflateFlag(0)
setNetcdfDeflateLevelFlag(0)


RootDir = '/mnt/BCSD'

OUTROOT = '/mnt/out_stats'
if not path.isdir(OUTROOT):
    mkdir(OUTROOT)

OUTTEMP = '/home/edarague'
if not path.isdir(OUTTEMP):
    mkdir(OUTTEMP)

# added as fgobal institution attribute to output files
txtinst = "Santa Clara U.,Climate Central,The Nature Conservancy"

# input files are on 0->360 longitude convention. To switch to a -180->180 grid:
# cdo sellonlatbox,-180,180,-90,90 ifile ofile
# which works for global domains only.For smaller domains:
# cdo griddes ifile > mygrid ; then edit mygrid and set xfirst to the new value
# cdo setgrid,mygrid ifile ofile


def calc_ppmm(fnamep='', styr=0, enyr=0, model=''):
    if not path.exists(RootDir + "/" + model + "/junk"):
        system("mkdir -p " + RootDir + "/" + model + "/junk")
    fnp_nodir = split(fnamep, "/")[-1]
    nyrs = enyr - styr + 1
    for i in range(nyrs):
        y = styr + i
        fnp = OUTTEMP + "/" + model + "/junk/" + fnp_nodir + str(y) + ".nc"
        fnpr = RootDir + "/" + model + "/junk/" + fnp_nodir + str(y) + ".nc"
        fnpr = fnpr.replace('pr', 'prmm')
        if not path.exists(fnp):
            if y == enyr:
                print 'infile not found: ', fnp, ' ...skipping last year'
                break
            else:
                raise Exception('infile not found: ' + fnp)
        # calc pp in mm if not extist
        if not path.exists(fnpr):
            print "\n... calculating daily pp in mm for %s" % (path.basename(fnp))
            txt1 = "cdo -m 1e+20 -mulc,86400 %s tmpfile" % fnp
            print txt1
            system(txt1)
            txt2 = "ncatted -a units,pr,o,c,'mm/day' tmpfile"
            print txt2
            system(txt2)
            txt3 = "mv tmpfile " + fnpr
            print txt3
            system(txt3)


def ptot(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to PTOT %s %d %d' % (fname, styr, enyr, model)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('prmm_day', 'PTOT')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr+str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr+i
            print "\n... computing Ptot for year ",y
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print 'infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            if i == 0:
                txt = "cdo -m 1e+20 monsum " + fn + " " + ofallmon
                print txt
                system(txt)
            else:
                txt = "cdo -m 1e+20 monsum " + fn + " junk_mon.nc"
                print txt
                system(txt)
                txt = "cdo -b F32 cat " + ofallmon + " junk_mon.nc junk_mon_cat.nc"
                print txt
                system(txt)
                txt = "rm -rf junk_mon.nc " + ofallmon + " && mv junk_mon_cat.nc " + ofallmon
                print txt
                system(txt)
        now = datetime.now()
        txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
        txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        txtcmd = "ncrename -h -v pr,PTOT " + ofallmon
        print txtcmd
        system(txtcmd)
        # create yearly summary file
        txtcmd = "cdo -m 1e+20 yearsum " + ofallmon + " " + ofall
        print txtcmd
        system(txtcmd)
        txtmvmon = "mv %s %s" % (ofallmon, ofallmonr)
        print txtmvmon
        system(txtmvmon)
        txtmv = "mv %s %s" % (ofall, ofallr)
        print txtmv
        system(txtmv)
        return ofall
    else:
        print "\n... nothing to do, %s exist!\n" % ofall


def cdd(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to CDD %s %d %d' % (fname, styr, enyr, model)
    nyrs = enyr-styr+1
    fn_nodir = split(fname,"/")[-1]
    ofall = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('prmm_day', 'CDD')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr+i
            print "\n... computing CDD for year ",y
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print 'infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            if i == 0:
                txt = "cdo -m 1e+20 eca_cdd " + fn + " " + ofallmon
                system(txt)
            else:
                txt = "cdo -m 1e+20 eca_cdd " + fn + " junk_mon.nc"
                print txt
                system(txt)
                txt = "cdo -b F32 cat " + ofallmon + " junk_mon.nc junk_mon_cat.nc"
                print txt
                system(txt)
                txt = "rm -rf junk_mon.nc " + ofallmon + " && mv junk_mon_cat.nc " + ofallmon
                print txt
                system(txt)
        now = datetime.now()
        txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
        txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
        system(txtcmd)
        #new variable name created by CDO:
        txtnewvar = "consecutive_dry_days_index_per_time_period"
        txtcmd = "ncrename -h -v " + txtnewvar + ",cdd " + ofall
        system(txtcmd)
        txtnewvar = "number_of_cdd_periods_with_more_than_5days_per_time_period"
        txtcmd = "ncrename -h -v " + txtnewvar + ",cdd5 " + ofall
        system(txtcmd)
        #split out second variable into new file
        ofall2 = ofall.replace('CDD', 'CDD5')
        txtcmd = "ncks -O -h -v cdd5 " + ofall+" "+ofall2
        system(txtcmd)
        #remove cdd5 from outfile - -O for forced overwrite
        txtcmd = "ncks -O -h -v cdd " + ofall + " "+ofall
        system(txtcmd)
        # create yearly summary file
        txtcmd = "cdo -m 1e+20 yearsum " + ofallmon + " " + ofall
        print txtcmd
        system(txtcmd)
        txtmvmon = "mv %s %s" % (ofallmon, ofallmonr)
        print txtmvmon
        system(txtmvmon)
        txtmv = "mv %s %s" % (ofall, ofallr)
        print txtmv
        system(txtmv)
        return ofall
    else:
        print "\n... nothing to do, %s exist!\n" % ofall


def r02(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R02 %s %d %d' % (fname, styr, enyr, model)
    nyrs = enyr-styr+1
    fn_nodir = split(fname,"/")[-1]
    ofall = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('prmm_day', 'R02')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr+str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr+i
            print "\n... computing R02 for year ",y
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print '... infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            for j in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]:
                fx = " -selmon," + str(j) + " " + fn
                if i == 0 and j == 1:
                    txt = "cdo -m 1e+20" + fx + " selmon_1.nc"
                    print "..." + txt
                    system(txt)
                    txt = "cdo -m 1e+20 eca_pd,0.2 selmon_1.nc eca_pd_1.nc"
                    print "... " + txt
                    system(txt)
                else:
                    txt = "cdo -m 1e+20" + fx + " selmon_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
                    txt = "cdo -m 1e+20 eca_pd,0.2 selmon_" + str(j) + ".nc eca_pd_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
            if i == 0:
                txt = "cdo cat eca_pd_1.nc eca_pd_2.nc eca_pd_3.nc eca_pd_4.nc eca_pd_5.nc eca_pd_6.nc " \
                      "eca_pd_7.nc eca_pd_8.nc eca_pd_9.nc eca_pd_10.nc eca_pd_11.nc eca_pd_12.nc junkmon.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc eca_pd_*.nc"
                print "... " + txt
                system(txt)
            else:
                txt = "cdo cat eca_pd_1.nc eca_pd_2.nc eca_pd_3.nc eca_pd_4.nc eca_pd_5.nc eca_pd_6.nc " \
                      "eca_pd_7.nc eca_pd_8.nc eca_pd_9.nc eca_pd_10.nc eca_pd_11.nc eca_pd_12.nc junkmon_eca_pd.nc"
                print "... " + txt
                system(txt)
                txt = "cdo cat junkmon.nc junkmon_eca_pd.nc junkmon_tmp.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc timsum_*.nc junk_pd02_oneyear.nc junkmon.nc junkmon_eca_pd.nc"
                print "... " + txt
                system(txt)
                txt = "mv junkmon_tmp.nc junkmon.nc"
                print "... " + txt
                system(txt)
        # modify variable name and other attributes
        txt = "mv junkmon.nc " + ofallmon
        print "\n... " + txt
        system(txt)
        now = datetime.now()
        txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
        txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofallmon
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofallmon
        system(txtcmd)
        # new variable name created by CDO:
        txtnewvar = "precipitation_days_index_per_time_period"
        txtcmd = "ncrename -h -v " + txtnewvar + ",pd02 " + ofallmon
        system(txtcmd)
        # create yearly summary file
        txtcmd = "cdo -m 1e+20 yearsum " + ofallmon + " " + ofall
        print "... " + txtcmd
        system(txtcmd)
        txtmvmon = "mv %s %s" % (ofallmon, ofallmonr)
        print "... " + txtmvmon
        system(txtmvmon)
        txtmv = "mv %s %s" % (ofall, ofallr)
        print "... " + txtmv
        system(txtmv)
        return ofall
    else:
        print "\n... nothing to do, %s exist!\n" % ofall


def r5d(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R5D %s %d %d' % (fname, styr, enyr, model)
    nyrs = enyr-styr+1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('prmm_day', 'R5D')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr+str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr+i
            print "\n... computing R5D for year ",y
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print 'infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            if i == 0:
                txt = "cdo -m 1e+20 eca_rx5day " + fn + " " + ofallmon
                print txt
                system(txt)
            else:
                txt = "cdo -m 1e+20 eca_rx5day " + fn + " junk_mon.nc"
                print txt
                system(txt)
                txt = "cdo -b F32 cat " + ofallmon + " junk_mon.nc junk_mon_cat.nc"
                print txt
                system(txt)
                txt = "rm -rf junk_mon.nc " + ofallmon + " && mv junk_mon_cat.nc " + ofallmon
                print txt
                system(txt)
        now = datetime.now()
        txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
        txtcmd = "ncatted -h -a history,global,o,c,'"+ txthist + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'"+ txtinst + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        #new variable name created by CDO:
        txtnewvar = "highest_five_day_precipitation_amount_per_time_period"
        txtcmd = "ncrename -h -v " + txtnewvar + ",r5d " + ofall
        print txtcmd
        system(txtcmd)
        # create yearly summary file
        # txtcmd = "cdo -m 1e+20 yearsum " + ofallmon + " " + ofall
        # print txtcmd
        # system(txtcmd)
        #remove extra variable(s) created by CDO:
        txtcmd = "ncks -O -h -v r5d " + ofallmon + " " + ofallmon
        print txtcmd
        system(txtcmd)
        txtmvmon = "mv %s %s" % (ofallmon, ofallmonr)
        print txtmvmon
        system(txtmvmon)
        # txtmv = "mv %s %s" % (ofall, ofallr)
        # print txtmv
        # system(txtmv)
        return ofall
    else:
        print "\n... nothing to do, %s exist!\n" % ofall


def sdii(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to SDII %s %d %d' % (fname, styr, enyr, model)
    nyrs = enyr-styr+1
    fn_nodir = split(fname,"/")[-1]
    ofall = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('prmm_day', 'SDII')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr+str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr+i
            print "\n... computing SDII for year ",y
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print '... infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            for j in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]:
                fx = " -selmon," + str(j) + " " + fn
                if i == 0 and j == 1:
                    txt = "cdo -m 1e+20" + fx + " selmon_1.nc"
                    print "..." + txt
                    system(txt)
                    txt = "cdo -m 1e+20 eca_sdii selmon_1.nc eca_sdii_1.nc"
                    print "... " + txt
                    system(txt)
                else:
                    txt = "cdo -m 1e+20" + fx + " selmon_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
                    txt = "cdo -m 1e+20 eca_sdii selmon_" + str(j) + ".nc eca_sdii_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
            if i == 0:
                txt = "cdo cat eca_sdii_1.nc eca_sdii_2.nc eca_sdii_3.nc eca_sdii_4.nc eca_sdii_5.nc eca_sdii_6.nc " \
                      "eca_sdii_7.nc eca_sdii_8.nc eca_sdii_9.nc eca_sdii_10.nc eca_sdii_11.nc eca_sdii_12.nc junkmon.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc eca_sdii_*.nc"
                print "... " + txt
                system(txt)
            else:
                txt = "cdo cat eca_sdii_1.nc eca_sdii_2.nc eca_sdii_3.nc eca_sdii_4.nc eca_sdii_5.nc eca_sdii_6.nc " \
                      "eca_sdii_7.nc eca_sdii_8.nc eca_sdii_9.nc eca_sdii_10.nc eca_sdii_11.nc eca_sdii_12.nc junkmon_eca_sdii.nc"
                print "... " + txt
                system(txt)
                txt = "cdo cat junkmon.nc junkmon_eca_sdii.nc junkmon_tmp.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc junk_eca_sdii_oneyear.nc junkmon.nc junkmon_eca_sdii.nc"
                print "... " + txt
                system(txt)
                txt = "mv junkmon_tmp.nc junkmon.nc"
                print "... " + txt
                system(txt)
            print "Done with months, now doing annual\n"
            if i == 0:
                txt = "cdo -m 1e+20 eca_sdii " + fn + " " + ofall
                system(txt)
            else:
                txt = "cdo -m 1e+20 eca_sdii " + fn + " junk_year.nc"
                system(txt)
                txt = "cdo cat junk_year.nc " + ofall
                system(txt)
         # modify variable name and other attributes
        txt = "mv junkmon.nc " + ofallmon
        print "\n... " + txt
        system(txt)
        now = datetime.now()
        txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
        txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
        system(txtcmd)
        txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
        system(txtcmd)
        #new variable name created by CDO:
        txtnewvar = "simple_daily_intensitiy_index_per_time_period"
        txtcmd = "ncrename -h -v "+txtnewvar + ",sdii " + ofallmon
        system(txtcmd)
        txtcmd = "ncrename -h -v "+txtnewvar + ",sdii " + ofall
        system(txtcmd)
        txtmvmon = "mv %s %s" % (ofallmon, ofallmonr)
        print "... " + txtmvmon
        system(txtmvmon)
        txtmv = "mv %s %s" % (ofall, ofallr)
        print "... " + txtmv
        system(txtmv)
        return ofall
    else:
        print "\n... nothing to do, %s exist!\n" % ofall

#90th percentile - wet days only
def R90 (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R90 %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R90')
    for i in range(nyrs):
        y = styr+i
        print "computing R90 for year ",y
        #fn = fname + str(y) + ".nc"
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        if (i==0):
            txt = "cdo -m 1e+20 yearpctl,90 "+fn+" -yearmin "+fn+" -yearmax "+fn+" "+ofall
            system(txt)
        else:
            txt = "cdo -m 1e+20 yearpctl,90 "+fn+" -yearmin "+fn+" -yearmax "+fn+" junk_mon.nc"
            system(txt)
            txt = "cdo cat junk_mon.nc "+ofall
            system(txt)
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    system(txtcmd)
    txtcmd = "ncrename -h -v pr,r90 " + ofall
    system(txtcmd)
    return ofall

#ref period only - wet days only
def R90ref (fname='',styr=0,enyr=0):
    #to create a file with 366 time steps and a 90pctl value for each cell, cat all files together:
    #uses a 5-day running mean
    if not styr > 1899 and enyr < 2006 and (enyr>styr):
        raise 'incorrect args passed to R90ref %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R90ref')
    ofbig = OUTTEMP+"/"+fn_nodir+"all."+str(styr)+"-"+str(enyr)+".nc"
    txtcmd = "ncrcat -n "+str(nyrs)+",4,1 "+OUTTEMP+"/"+fn_nodir+str(styr)+".nc " + ofbig
    if not path.exists(ofbig):
        system(txtcmd)
        print "created combined file ",ofbig
    else:
        print "combined file already exists: ",ofbig
    ofpartflist=[]
    #for only rainy days (P>1 mm)
    #slice big file spatially - 18 zonal slices
    for i in range(18):
        lat1=-90+(i*10)
        lat2=-90+((i+1)*10)
        filepart="sellonlatbox,0,360,"+str(lat1)+","+str(lat2)
        ofjunk= OUTTEMP+"/r90pref_ofbigpart.nc"
        ofjunkm= OUTTEMP+"/r90pref_ofbigpart_masked.nc"
        ofpart= OUTTEMP+"/r90pref_ydrunpctl90."+str(i)+".nc"
        txt="cdo -m 1e+20 sellonlatbox,0,360,"+str(lat1)+","+str(lat2)+" "+ofbig+" "+ofjunk
        system(txt)
        txt1 = "cdo ifthen -gtc,1 "+ofjunk+" "+ofjunk+" "+ofjunkm
        system(txt1)
        txtcmd = "cdo -m 1e+20 ydrunpctl,90,5 "+ofjunkm+" -ydrunmin,5 "+ofjunkm+" -ydrunmax,5 "+ofjunkm+" "+ofpart
        system(txtcmd)
        ofpartflist.append(ofpart)
    print "finished separate files, putting them back together..."
    MosaicFiles(ofpartflist,'pr',ofall)
    #done with aggregation
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    system(txtcmd)
    txtcmd = "ncrename -h -v pr,r90ref " + ofall
    system(txtcmd)
    return ofall

#future periods only - wrt wet day 90th pctile
def R90P (fname='',reffile='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R90P %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R90P')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('pr','R90P')
    fnref_nodir = split(reffile,"/")[-1]
    for i in range(nyrs):
        y = styr+i
        print "computing R90P for year ",y
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        #mask for minimum precipitation amount
        txt1 = "cdo ifthen -gtc,1 "+fn+" "+fn+" "+OUTTEMP+"/junk_r90p.nc"
        system(txt1)
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 eca_r90p -selmon,"+str(j)+" "+OUTTEMP+"/junk_r90p.nc -selmon,"+str(j)+" "+reffile+" "+ofallmon
                system(txt)
            else:
                txt = "cdo -m 1e+20 eca_r90p -selmon,"+str(j)+" "+OUTTEMP+"/junk_r90p.nc -selmon,"+str(j)+" "+reffile+" junk_mon.nc"
                system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                system(txt)
        print "Done with months, now doing annual\n"
        if (i==0):
            txt = "cdo -m 1e+20 eca_r90p "+OUTTEMP+"/junk_r90p.nc "+reffile+" "+ofall
            system(txt)
        else:
            txt = "cdo -m 1e+20 eca_r90p "+OUTTEMP+"/junk_r90p.nc "+reffile+" junk_mon.nc"
            system(txt)
            txt = "cdo cat junk_mon.nc "+ofall
            system(txt)
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    system(txtcmd)
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "wet_days_wrt_90th_percentile_of_reference_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",r90p " + ofallmon
    system(txtcmd)
    txtcmd = "ncrename -h -v "+txtnewvar + ",r90p " + ofall
    system(txtcmd)
    return ofall

#future periods only
def R90PTOT (fname='',reffile='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R90PTOT %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R90PTOT')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('pr','R90PTOT')
    fnref_nodir = split(reffile,"/")[-1]
    for i in range(nyrs):
        y = styr+i
        print "computing R90PTOT for year ",y
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        #mask for minimum precipitation amount
        txt1 = "cdo ifthen -gtc,1 "+fn+" "+fn+" "+OUTTEMP + "/junk_r90p.nc"
        system(txt1)
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 eca_r90ptot -selmon,"+str(j)+" "+OUTTEMP + "/junk_r90p.nc -selmon,"+str(j)+" "+reffile+" "+ofallmon
                system(txt)
            else:
                txt = "cdo -m 1e+20 eca_r90ptot -selmon,"+str(j)+" "+OUTTEMP + "/junk_r90p.nc -selmon,"+str(j)+" "+reffile+" junk_mon.nc"
                system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                system(txt)
        print "Done with months, now doing annual\n"
        if (i==0):
            txt = "cdo -m 1e+20 eca_r90ptot "+OUTTEMP + "/junk_r90p.nc "+reffile+" "+ofall
            system(txt)
        else:
            txt = "cdo -m 1e+20 eca_r90ptot "+OUTTEMP + "/junk_r90p.nc "+reffile+" junk_mon.nc"
            system(txt)
            txt = "cdo cat junk_mon.nc "+ofall
            system(txt)
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    system(txtcmd)
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "precipitation_percent_due_to_R90p_days"
    txtcmd = "ncrename -h -v "+txtnewvar + ",r90ptot " + ofallmon
    system(txtcmd)
    txtcmd = "ncrename -h -v "+txtnewvar + ",r90ptot " + ofall
    system(txtcmd)
    return ofall



####for spi, first create a timeseries object for each on-masked grid cell:
#>>> first_date = ts.Date('D', '2009-01-01')
#>>> series = ts.time_series([1, 2, 3, 4], start_date=first_date)
#>>> series
#timeseries([1 2 3 4],
#   dates = [01-Jan-2009 ... 04-Jan-2009],
#   freq  = D)
