#!/usr/local/cdat5.2/bin/python

"""Module for computing temperature extreme stats mostly using CDO utilities"""

from sys import exit
from os import path, system, mkdir
from cdms2 import setNetcdfShuffleFlag, setNetcdfDeflateLevelFlag, setNetcdfDeflateFlag
from string import split
from datetime import datetime
from daily_stats_cdms_utils import MosaicFiles

setNetcdfShuffleFlag(0)
setNetcdfDeflateFlag(0)
setNetcdfDeflateLevelFlag(0)

# necesario para temperaturas promedio calculadas
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


def copy_files(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to copy_files %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    if not path.exists(OUTTEMP + "/" + model + "/junk"):
        system("mkdir -p " + OUTTEMP + "/" + model + "/junk")
    for i in range(nyrs):
        y = styr + i
        fn = fname + str(y) + ".nc"
        if not path.exists(fn):
            if y == enyr:
                print '\ninfile not found: ', fn, ' ...skipping last year'
                break
            else:
                print '\ninfile not found: ', fn
                exit(0)
        of = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
        if not path.exists(of):
            txt = "ln -s " + fn + " " + of
            print "\n... creating link for file " + fn + " to directory " + OUTTEMP + "/" + model + "/junk"
            system(txt)
        else:
            print "\n... link to " + fn + " already exists in directory " + OUTTEMP + "/" + model + \
                  "/junk\n\tskipping..."


def calc_tavg(fnamen='', fnamex='', styr=0, enyr=0, model=''):
    if not path.exists(RootDir + "/" + model + "/junk"):
        system("mkdir -p " + RootDir + "/" + model + "/junk")
    fnx_nodir = split(fnamex, "/")[-1]
    fnn_nodir = split(fnamen, "/")[-1]
    nyrs = enyr - styr + 1
    for i in range(nyrs):
        y = styr + i
        fnx = OUTTEMP + "/" + model + "/junk/" + fnx_nodir + str(y) + ".nc"
        fnn = OUTTEMP + "/" + model + "/junk/" + fnn_nodir + str(y) + ".nc"
        fnxe = RootDir + "/" + model + "/junk/" + fnx_nodir + str(y) + ".nc"
        ft = fnx.replace('tasmax', 'tmp')
        fn = fnx.replace('tasmax', 'tas')
        fne = fnxe.replace('tasmax', 'tas')
        if not (path.exists(fnx) and path.exists(fnn)):
            if y == enyr:
                print 'infile not found: ', fnx, fnn, ' ...skipping last year'
                break
            else:
                raise Exception('infile not found: ' + fnx + ' or ' + fnn)
        # calc mean daily temp if doesn't already exist
        if not path.exists(fne):
            print "\n... calculating daily avg temp for %s%s" % (path.basename(fnamen), y)
            txt1 = "cdo -m 1e+20 -add %s %s %s" % (fnn, fnx, ft)
            print txt1
            system(txt1)
            txt2 = "cdo divc,2.0 %s %s" % (ft, fn)
            print txt2
            system(txt2)
            txt3 = "rm -rf " + ft
            print txt3
            system(txt3)
            txt4 = "ncrename -h -v tasmin,tas " + fn
            print txt4
            system(txt4)
            txt5 = "mv " + fn + " " + fne
            print txt5
            system(txt5)


def tavg(fname='', styr=0, enyr=0, model=''):
    if not path.exists(OUTROOT + "/" + model + "/"):
        system('mkdir ' + OUTROOT + "/" + model + "/")
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to tavg (%s, %d, %d, %s)' % (fname, styr, enyr, model)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    ofall = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('r1i1p1_', '')).replace('_day', '')
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr + i
            print "\n computing tavg for %s%s " % (path.basename(fname), y)
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print 'infile not found: %s ...skipping last year' % fn
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            if i == 0:
                txt = "cdo -m 1e+20 monmean " + fn + " " + ofallmon
                print txt
                system(txt)
            else:
                txt = "cdo -m 1e+20 monmean " + fn + " junk_mon.nc"
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
        txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        # create yearly summary file
        txtcmd = "cdo -m 1e+20  yearavg " + ofallmon + " " + ofall
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
        print "\n... nothing to do, %s exist!\n" % ofallmon


def txx(fname='', styr=0, enyr=0, model=''):
    if not path.exists(OUTROOT + "/" + model + "/"):
        system('mkdir ' + OUTROOT + "/" + model + "/")
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to TXX %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('tasmax_day', 'TXX')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr + i
            print "\n> computing TXX for year ", y
            # fn = fname + str(y) + ".nc"
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print 'infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            if i == 0:
                txt = "cdo -m 1e+20 monmax " + fn + " " + ofallmon
                print txt
                system(txt)
            else:
                txt = "cdo -m 1e+20 monmax " + fn + " junk_mon.nc"
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
        txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        txtcmd = "ncrename -h -v tasmax,txx " + ofallmon
        print txtcmd
        system(txtcmd)
        # create yearly summary file
        txtcmd = "cdo -m 1e+20 yearmax " + ofallmon + " " + ofall
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


def tnn(fname='', styr=0, enyr=0, model=''):
    if not path.exists(OUTROOT + "/" + model + "/"):
        system('mkdir ' + OUTROOT + "/" + model + "/")
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to TNN %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('tasmin_day', 'TNN')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr + i
            print "\n> computing TNN for year ", y
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print 'infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            if i == 0:
                txt = "cdo -m 1e+20 monmin " + fn + " " + ofallmon
                print txt
                system(txt)
            else:
                txt = "cdo -m 1e+20 monmin " + fn + " junk_mon.nc"
                print txt
                system(txt)
                txt = "cdo -b F32 cat " + ofallmon + " junk_mon.nc junk_mon_cat.nc"
                print txt
                system(txt)
                txt = "rm -rf junk_mon.nc " + ofallmon + " && mv junk_mon_cat.nc " + ofallmon
                print txt
                system(txt)
        # modify variable name and other attributes
        now = datetime.now()
        txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
        txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofallmon
        print txtcmd
        system(txtcmd)
        txtcmd = "ncrename -h -v tasmin,tnn " + ofallmon
        print txtcmd
        system(txtcmd)
        # create yearly summary file
        txtcmd = "cdo -m 1e+20 yearmin " + ofallmon + " " + ofall
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


def gd10(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to GD10 %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('tas_day', 'GD10')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr + i
            print "\n> computing GD10 for year ", y
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print '... infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            txt = "cdo -m 1e+20 -gtc,283.15 " + fn + " gtc10.nc"
            print "... " + txt
            system(txt)
            txt = "cdo -m 1e+20 -subc,283.15 " + fn + " subc10.nc"
            print "... " + txt
            system(txt)
            txt = "cdo -m 1e+20 mul gtc10.nc subc10.nc junk_gd10_oneyear.nc"
            print "... " + txt
            system(txt)
            system(txt)
            txt = "rm gtc10.nc subc10.nc"
            print "... " + txt
            system(txt)
            for j in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]:
                fx = " -selmon," + str(j) + " junk_gd10_oneyear.nc "
                if i == 0 and j == 1:
                    txt = "cdo -m 1e+20" + fx + "selmon_1.nc"
                    print "..." + txt
                    system(txt)
                    txt = "cdo -m 1e+20 timsum selmon_1.nc timsum_1.nc"
                    print "... " + txt
                    system(txt)
                else:
                    txt = "cdo -m 1e+20" + fx + "selmon_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
                    txt = "cdo -m 1e+20 timsum selmon_" + str(j) + ".nc timsum_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
            if i == 0:
                txt = "cdo cat timsum_1.nc timsum_2.nc timsum_3.nc timsum_4.nc timsum_5.nc timsum_6.nc " \
                      "timsum_7.nc timsum_8.nc timsum_9.nc timsum_10.nc timsum_11.nc timsum_12.nc junkmon.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc timsum_*.nc junk_gd10_oneyear.nc"
                print "... " + txt
                system(txt)
            else:
                txt = "cdo cat timsum_1.nc timsum_2.nc timsum_3.nc timsum_4.nc timsum_5.nc timsum_6.nc " \
                      "timsum_7.nc timsum_8.nc timsum_9.nc timsum_10.nc timsum_11.nc timsum_12.nc junkmon_tisum.nc"
                print "... " + txt
                system(txt)
                txt = "cdo cat junkmon.nc junkmon_tisum.nc junkmon_tmp.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc timsum_*.nc junk_gd10_oneyear.nc junkmon.nc junkmon_tisum.nc"
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
        txtnewvar = "tas"
        txtcmd = "ncrename -h -v " + txtnewvar + ",gd10 " + ofallmon
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
        print "... nothing to do, %s exist!\n" % ofall


def hd18(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to HD18 %s %d %d' % (fname, styr, enyr, model)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('tas_day', 'HD18')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr + i
            print "\n> computing HD18 for year ", y
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
                    txt = "cdo -m 1e+20 eca_hd,18 selmon_1.nc eca_hd_1.nc"
                    print "... " + txt
                    system(txt)
                else:
                    txt = "cdo -m 1e+20" + fx + " selmon_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
                    txt = "cdo -m 1e+20 eca_hd,18 selmon_" + str(j) + ".nc eca_hd_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
            if i == 0:
                txt = "cdo cat eca_hd_1.nc eca_hd_2.nc eca_hd_3.nc eca_hd_4.nc eca_hd_5.nc eca_hd_6.nc " \
                      "eca_hd_7.nc eca_hd_8.nc eca_hd_9.nc eca_hd_10.nc eca_hd_11.nc eca_hd_12.nc junkmon.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc eca_hd_*.nc"
                print "... " + txt
                system(txt)
            else:
                txt = "cdo cat eca_hd_1.nc eca_hd_2.nc eca_hd_3.nc eca_hd_4.nc eca_hd_5.nc eca_hd_6.nc " \
                      "eca_hd_7.nc eca_hd_8.nc eca_hd_9.nc eca_hd_10.nc eca_hd_11.nc eca_hd_12.nc junkmon_eca_hd.nc"
                print "... " + txt
                system(txt)
                txt = "cdo cat junkmon.nc junkmon_eca_hd.nc junkmon_tmp.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc timsum_*.nc junk_hd18_oneyear.nc junkmon.nc junkmon_eca_hd.nc"
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
        txtnewvar = "tas"
        txtcmd = "ncrename -h -v " + txtnewvar + ",hd18 " + ofallmon
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
        print "... nothing to do, %s exist!\n" % ofall


def cd18(fname='', styr=0, enyr=0, model=''):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to CD18 %s %d %d' % (fname, styr, enyr, model)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofallmon = fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    fn_nodirr = ((split(fname, "/")[-1]).replace('tas_day', 'CD18')).replace('_r1i1p1', '')
    ofallr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".nc"
    ofallmonr = OUTROOT + "/" + model + "/" + fn_nodirr + str(styr) + "-" + str(enyr) + ".monthly.nc"
    if not path.exists(ofallmonr):
        for i in range(nyrs):
            y = styr + i
            print "\n> computing CD18 for year ", y
            fn = OUTTEMP + "/" + model + "/junk/" + fn_nodir + str(y) + ".nc"
            if not path.exists(fn):
                if y == enyr:
                    print '... infile not found: ', fn, ' ...skipping last year'
                    break
                else:
                    raise Exception('infile not found: %s' % fn)
            txt = "cdo -m 1e+20 -gtc,291.15 " + fn + " gtc18.nc"
            print "... " + txt
            system(txt)
            txt = "cdo -m 1e+20 -subc,291.15 " + fn + " subc18.nc"
            print "... " + txt
            system(txt)
            txt = "cdo -m 1e+20 mul gtc18.nc subc18.nc junk_cd18_oneyear.nc"
            print "... " + txt
            system(txt)
            system(txt)
            txt = "rm gtc18.nc subc18.nc"
            print "... " + txt
            system(txt)
            for j in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]:
                fx = " -selmon," + str(j) + " junk_cd18_oneyear.nc "
                if i == 0 and j == 1:
                    txt = "cdo -m 1e+20" + fx + "selmon_1.nc"
                    print "..." + txt
                    system(txt)
                    txt = "cdo -m 1e+20 timsum selmon_1.nc timsum_1.nc"
                    print "... " + txt
                    system(txt)
                else:
                    txt = "cdo -m 1e+20" + fx + "selmon_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
                    txt = "cdo -m 1e+20 timsum selmon_" + str(j) + ".nc timsum_" + str(j) + ".nc"
                    print "... " + txt
                    system(txt)
            if i == 0:
                txt = "cdo cat timsum_1.nc timsum_2.nc timsum_3.nc timsum_4.nc timsum_5.nc timsum_6.nc " \
                      "timsum_7.nc timsum_8.nc timsum_9.nc timsum_10.nc timsum_11.nc timsum_12.nc junkmon.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc timsum_*.nc junk_cd18_oneyear.nc"
                print "... " + txt
                system(txt)
            else:
                txt = "cdo cat timsum_1.nc timsum_2.nc timsum_3.nc timsum_4.nc timsum_5.nc timsum_6.nc " \
                      "timsum_7.nc timsum_8.nc timsum_9.nc timsum_10.nc timsum_11.nc timsum_12.nc junkmon_tisum.nc"
                print "... " + txt
                system(txt)
                txt = "cdo cat junkmon.nc junkmon_tisum.nc junkmon_tmp.nc"
                print "... " + txt
                system(txt)
                txt = "rm selmon_*.nc timsum_*.nc junk_cd18_oneyear.nc junkmon.nc junkmon_tisum.nc"
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
        txtnewvar = "tas"
        txtcmd = "ncrename -h -v " + txtnewvar + ",cd18 " + ofallmon
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
        print "... nothing to do, %s exist!\n" % ofall


def TX90(fname='', styr=0, enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to TX90 %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTROOT + "/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofall = ofall.replace('tasmax', 'TX90')
    for i in range(nyrs):
        y = styr + i
        print "computing TX90 for year ", y
        # fn = fname + str(y) + ".nc"
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not path.exists(fn):
            if y == enyr:
                print 'infile not found: ', fn, ' ...skipping last year'
                nyrs = nyrs - 1
                break
            else:
                raise 'infile not found: ', fn
        if (i == 0):
            txt = "cdo -m 1e+20 yearpctl,90 " + fn + " -yearmin " + fn + " -yearmax " + fn + " " + ofall
            system(txt)
        else:
            txt = "cdo -m 1e+20 yearpctl,90 " + fn + " -yearmin " + fn + " -yearmax " + fn + " junk_mon.nc"
            system(txt)
            txt = "cdo cat junk_mon.nc " + ofall
            system(txt)
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofall
    system(txtcmd)
    txtcmd = "ncrename -h -v tasmax,tx90 " + ofall
    system(txtcmd)
    return ofall


def TXnorm(fname='', styr=0, enyr=0):
    # to create a file with 365 time steps and a TXnorm value for each cell, cat all files together
    # uses a 5-day running mean
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to TXnorm %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTROOT + "/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofall = ofall.replace('tasmax', 'TXnorm')
    ofbig = OUTTEMP + "/" + fn_nodir + "all." + str(styr) + "-" + str(enyr) + ".nc"
    txtcmd = "ncrcat -n " + str(nyrs) + ",4,1 " + OUTTEMP + "/" + fn_nodir + str(styr) + ".nc " + ofbig
    if not path.exists(ofbig):
        system(txtcmd)
        print "created combined file ", ofbig
    else:
        print "combined file already exists: ", ofbig
    txtcmd = "cdo -m 1e+20 ydrunmean,5 " + ofbig + " " + ofall
    system(txtcmd)
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofall
    system(txtcmd)
    txtcmd = "ncrename -h -v tasmax,TXnorm " + ofall
    system(txtcmd)
    return ofall


def FD(fname='', styr=0, enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to FD %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTROOT + "/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofall = ofall.replace('tasmin', 'FD')
    ofallmon = OUTROOT + "/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    ofallmon = ofallmon.replace('tasmin', 'FD')
    for i in range(nyrs):
        y = styr + i
        print "computing FD for year ", y
        # fn = fname + str(y) + ".nc"
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not path.exists(fn):
            if y == enyr:
                print 'infile not found: ', fn, ' ...skipping last year'
                nyrs = nyrs - 1
                break
            else:
                raise 'infile not found: ', fn
        for j in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]:
            if (i == 0 and j == 1):
                txt = "cdo -m 1e+20 eca_fd -addc,273.15 -selmon," + str(j) + " " + fn + " " + ofallmon
                system(txt)
            else:
                txt = "cdo -m 1e+20 eca_fd -addc,273.15 -selmon," + str(j) + " " + fn + " junk_mon.nc"
                system(txt)
                txt = "cdo cat junk_mon.nc " + ofallmon
                system(txt)
    # modify variable name and other attributes
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofallmon
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofallmon
    system(txtcmd)
    # new variable name created by CDO:
    txtnewvar = "frost_days_index_per_time_period"
    txtcmd = "ncrename -h -v " + txtnewvar + ",fd " + ofallmon
    system(txtcmd)
    # create yearly summary file
    txtcmd = "cdo -m 1e+20 yearsum " + ofallmon + " " + ofall
    system(txtcmd)
    return ofall


# cuts off last year to have global complete coverage
def GSL(fnamen='', fnamex='', fnamemask='', styr=0, enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to FD %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fnx_nodir = split(fnamex, "/")[-1]
    fnn_nodir = split(fnamen, "/")[-1]
    fn_nodir = fnx_nodir.replace('tasmax', 'tas')
    ofall = OUTROOT + "/" + fnx_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofall = ofall.replace('tasmax', 'GSL')
    ofbig = OUTTEMP + "/" + fn_nodir + "all." + str(styr) + "-" + str(enyr) + ".nc"
    for i in range(nyrs):
        y = styr + i
        fnx = OUTTEMP + "/" + fnx_nodir + str(y) + ".nc"
        fnn = OUTTEMP + "/" + fnn_nodir + str(y) + ".nc"
        fn = fnx.replace('tasmax', 'tas')
        if not (path.exists(fnx) and path.exists(fnn)):
            if y == enyr:
                print 'infile not found: ', fnx, fnn, ' ...skipping last year'
                nyrs = nyrs - 1
                break
            else:
                raise 'infile not found: ', fnx, fnn
        # calc mean daily temp if doesn't already exist
        if not path.exists(fn):
            print "calculating daily avg temp\n"
            txt = "cdo -m 1e+20 divc,2.0 -add " + fnn + " " + fnx + " " + fn
            system(txt)
            txtcmd = "ncrename -h -v tasmin,tas " + fn
            system(txtcmd)
    # create big file of tavg for all years if it doesn't already exist
    txtcmd = "ncrcat -n " + str(nyrs) + ",4,1 " + OUTTEMP + "/" + fn_nodir + str(styr) + ".nc " + ofbig
    if not path.exists(ofbig):
        system(txtcmd)
        print "created combined file ", ofbig
    else:
        print "combined file already exists: ", ofbig
    # truncate the last year
    ey = styr + nyrs - 2
    txt = "cdo -m 1e+20 selyear," + str(styr) + "," + str(
        ey) + " -eca_gsl -addc,273.15 " + ofbig + " " + fnamemask + " " + ofall
    system(txt)
    # modify variable name and other attributes
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofall
    system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "thermal_growing_season_length"
    txtcmd = "ncrename -h -v " + txtnewvar + ",gsl " + ofall
    system(txtcmd)
    return ofall


def TXnorm(fname='', styr=0, enyr=0):
    # to create a file with 365 time steps and a TXnorm value for each cell, cat all files together
    # uses a 5-day running mean
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to TXnorm %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTROOT + "/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofall = ofall.replace('tasmax', 'TXnorm')
    ofbig = OUTTEMP + "/" + fn_nodir + "all." + str(styr) + "-" + str(enyr) + ".nc"
    txtcmd = "ncrcat -n " + str(nyrs) + ",4,1 " + OUTTEMP + "/" + fn_nodir + str(styr) + ".nc " + ofbig
    if not path.exists(ofbig):
        system(txtcmd)
        print "created combined file ", ofbig
    else:
        print "combined file already exists: ", ofbig
    txtcmd = "cdo -m 1e+20 ydrunmean,5 " + ofbig + " " + ofall
    system(txtcmd)
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofall
    system(txtcmd)
    txtcmd = "ncrename -h -v tasmax,TXnorm " + ofall
    system(txtcmd)
    return ofall


def HWDI(fnamex='', reffile='', styr=0, enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr > styr):
        raise 'incorrect args passed to HWDI %s %d %d' % (fname, styr, enyr)
    nyrs = enyr - styr + 1
    fnx_nodir = split(fnamex, "/")[-1]
    ofall = OUTROOT + "/" + fnx_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofall = ofall.replace('tasmax', 'HWDI')
    for i in range(nyrs):
        y = styr + i
        fnx = OUTTEMP + "/" + fnx_nodir + str(y) + ".nc"
        print "Now working on HWDI for year ", y
        if not (path.exists(fnx)):
            if y == enyr:
                print 'infile not found: ', fnx, ' ...skipping last year'
                nyrs = nyrs - 1
                break
            else:
                raise 'infile not found: ', fnx
        if not path.exists(reffile):
            raise "reffile not found", reffile
        if (i == 0):
            txt = "cdo -m 1e+20 eca_hwdi " + fnx + " " + reffile + " " + ofall
            system(txt)
        else:
            txt = "cdo -m 1e+20 eca_hwdi " + fnx + " " + reffile + " junk_mon.nc"
            system(txt)
            txt = "cdo cat junk_mon.nc " + ofall
            system(txt)
    # modify variable name and other attributes
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofall
    system(txtcmd)
    # new variable name created by CDO:
    txtnewvar = "heat_wave_duration_index_wrt_mean_of_reference_period"
    txtcmd = "ncrename -h -v " + txtnewvar + ",hwdi " + ofall
    system(txtcmd)
    return ofall




# generic ref period percentile for temperatures only (precip needs to be masked)
def Tref(fname='', pctl='', varin='', styr=0, enyr=0):
    # to create a file with 366 time steps and a TNN value for each cell
    # uses a 5-day running mean
    if pctl <= 0 or pctl >= 100:
        raise 'percentile out of range'
    if not styr > 1899 and enyr < 2008 and (enyr > styr):
        raise 'incorrect args passed to Tref %s %d %d' % (fname, styr, enyr)
    if varin == 'tasmin':
        varout = 'TN' + str(pctl) + 'Pref'
    elif varin == 'tasmax':
        varout = 'TX' + str(pctl) + 'Pref'
    elif varin == 'tas':
        varout = 'TG' + str(pctl) + 'Pref'
    else:
        raise "invalid input variable in Tref\n"
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTROOT + "/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofall = ofall.replace(varin, varout)
    ofbig = OUTTEMP + "/" + fn_nodir + "all." + str(styr) + "-" + str(enyr) + ".nc"
    txtcmd = "ncrcat -n " + str(nyrs) + ",4,1 " + OUTTEMP + "/" + fn_nodir + str(styr) + ".nc " + ofbig
    if not path.exists(ofbig):
        system(txtcmd)
        print "created combined file ", ofbig
    else:
        print "combined file already exists: ", ofbig
    ofpartflist = []
    #slice big file spatially - 18 zonal slices
    for i in range(18):
        lat1 = -90 + (i * 10)
        lat2 = -90 + ((i + 1) * 10)
        filepart = "sellonlatbox,0,360," + str(lat1) + "," + str(lat2)
        ofjunk = OUTTEMP + "/" + varin + "_ref_ofbigpart.nc"
        ofpart = OUTTEMP + "/" + varin + "_ref_ydrunpctl" + str(pctl) + "." + str(i) + ".nc"
        txt = "cdo -m 1e+20 sellonlatbox,0,360," + str(lat1) + "," + str(lat2) + " " + ofbig + " " + ofjunk
        system(txt)
        txtcmd = "cdo -m 1e+20 ydrunpctl," + str(
            pctl) + ",5 " + ofjunk + " -ydrunmin,5 " + ofjunk + " -ydrunmax,5 " + ofjunk + " " + ofpart
        system(txtcmd)
        ofpartflist.append(ofpart)
    print "finished separate files, putting them back together..."
    MosaicFiles(ofpartflist, varin, ofall)
    #done with aggregation
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofall
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofall
    system(txtcmd)
    txtcmd = "ncrename -h -v " + varin + "," + varout.lower() + " " + ofall
    system(txtcmd)
    return ofall


def TP(fname='', pctl='', varin='', reffile='', styr=0, enyr=0):
    if pctl != 10 and pctl != 90:
        raise 'percentile out of range'
    if not styr > 1899 and enyr < 2008 and (enyr > styr):
        raise 'incorrect args passed to Tref %s %d %d' % (fname, styr, enyr)
    if varin == 'tasmin':
        varout = 'TN' + str(pctl) + 'P'
        if pctl == 10:
            varlong = "cold_nights_percent_wrt_10th_percentile_of_reference_period"
        else:
            varlong = "warm_nights_percent_wrt_90th_percentile_of_reference_period"
    elif varin == 'tasmax':
        varout = 'TX' + str(pctl) + 'P'
        if pctl == 10:
            varlong = "very_cold_days_percent_wrt_10th_percentile_of_reference_period"
        else:
            varlong = "very_warm_days_percent_wrt_90th_percentile_of_reference_period"
    elif varin == 'tas':
        varout = 'TG' + str(pctl) + 'P'
        if pctl == 10:
            varlong = "cold_days_percent_wrt_10th_percentile_of_reference_period"
        else:
            varlong = "warm_days_percent_wrt_90th_percentile_of_reference_period"
    else:
        raise "invalid input variable in TP\n"
    nyrs = enyr - styr + 1
    fn_nodir = split(fname, "/")[-1]
    ofall = OUTROOT + "/" + fn_nodir + str(styr) + "-" + str(enyr) + ".nc"
    ofall = ofall.replace(varin, varout)
    ofallmon = OUTROOT + "/" + fn_nodir + str(styr) + "-" + str(enyr) + ".monthly.nc"
    ofallmon = ofallmon.replace(varin, varout)
    fnref_nodir = split(reffile, "/")[-1]
    cdocmd = "eca_" + varout.lower()
    for i in range(nyrs):
        y = styr + i
        print "computing " + varout + " for year ", y
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        # txtcmd = "cdo -m 1e+20 -add "+reffile+" -sub "+fn+" "+fn+" "+ reffile_with_timesteps
        if not path.exists(fn):
            if y == enyr:
                print 'infile not found: ', fn, ' ...skipping last year'
                nyrs = nyrs - 1
                break
            else:
                raise 'infile not found: ', fn
        for j in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]:
            if (i == 0 and j == 1):
                txt = "cdo -m 1e+20 " + cdocmd + " -selmon," + str(j) + " " + fn + " -selmon," + str(
                    j) + " " + reffile + " " + ofallmon
                system(txt)
            else:
                txt = "cdo -m 1e+20 " + cdocmd + " -selmon," + str(j) + " " + fn + " -selmon," + str(
                    j) + " " + reffile + " junk_mon.nc"
                system(txt)
                txt = "cdo cat junk_mon.nc " + ofallmon
                system(txt)
    now = datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'" + txthist + "' " + ofallmon
    system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'" + txtinst + "' " + ofallmon
    system(txtcmd)
    # change new variable name created by CDO to desired variable name:
    txtcmd = "ncrename -h -v " + varlong + "," + varout + " " + ofallmon
    system(txtcmd)
    # create yearly summary file - weight by number of days per month
    txtcmd = "cdo -m 1e+20 divdpy -yearsum -muldpm " + ofallmon + " " + ofall
    system(txtcmd)
    return ofall
