#!/usr/local/cdat5.2/bin/python

"""Module for computing temperature extreme stats mostly using CDO utilities"""

import sys,os,string
import cdms2
import string
import datetime
import numpy as np
import daily_stats_cdms_utils as CdmsUtils

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

OUTROOT = '/media/data/out_stats'
OUTTEMP = '/media/data/junk'
if not os.path.isdir(OUTROOT):
    os.mkdir(OUTROOT)
if not os.path.isdir(OUTTEMP):
    os.mkdir(OUTTEMP)

#added as fgobal institution attribute to output files
txtinst = "Santa Clara U.,Climate Central,The Nature Conservancy"

#input files are on 0->360 longitude convention. To switch to a -180->180 grid:
#cdo sellonlatbox,-180,180,-90,90 ifile ofile
#which works for global domains only.For smaller domains:
#cdo griddes ifile > mygrid ; then edit mygrid and set xfirst to the new value
#cdo setgrid,mygrid ifile ofile

def CopyFiles (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to CopyFiles %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    for i in range(nyrs):
        y = styr+i
        fn = fname + str(y) + ".nc"
        print "copying file for year ",y," to new location"
        fn = fname + str(y) + ".nc"
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                print 'infile not found: ',fn
                sys.exit(0)
        of = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not os.path.exists(of):
#            txt = "cdo copy " + fn + " " + of
#            print "copying file " + fn + " to directory " + OUTTEMP
            txt = "ln -s " + fn + " " + of
            print "creating link for file " + fn + " to directory " + OUTTEMP
            os.system(txt)
        else:
            print "link to "+fn+" already exists in directory "+OUTTEMP+"\n\tskipping..."
#            print "file "+fn+" already exists in directory "+OUTTEMP+"\n\tskipping..."
            
def CalcTavg (fnamen='',fnamex='',styr=0,enyr=0):
    fnx_nodir = string.split(fnamex,"/")[-1]
    fnn_nodir = string.split(fnamen,"/")[-1]
    fn_nodir = fnx_nodir.replace('tasmax','tas')
    nyrs = enyr-styr+1
    for i in range(nyrs):
        y = styr+i
        fnx = OUTTEMP + "/" + fnx_nodir + str(y) + ".nc"
        fnn = OUTTEMP + "/" + fnn_nodir + str(y) + ".nc"
        fn = fnx.replace('tasmax','tas')
        if not (os.path.exists(fnx) and os.path.exists(fnn)):
            if y == enyr:
                print 'infile not found: ',fnx,fnn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ' % (fnx,fnn)
        #calc mean daily temp if doesn't already exist
        if not os.path.exists(fn):
            print "calculating daily avg temp\n"
            txt = "cdo -m 1e+20 divc,2.0 -add "+fnn+" "+fnx+" "+fn
            os.system(txt)
            txtcmd = "ncrename -h -v tasmin,tas " + fn
            os.system(txtcmd)

def TAVG (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to TAVG %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    #ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    #ofall = ofall.replace('tasmax','TXX')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    #ofallmon = ofallmon.replace('tasmax','TXX')
    for i in range(nyrs):
        y = styr+i
        print "computing TAVG for year ",y
        #fn = fname + str(y) + ".nc"
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        if (i==0):
            txt = "cdo -m 1e+20 monmean " + fn + " " + ofallmon
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 monmean " + fn + " junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc " + ofallmon
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    return ofallmon

def TXX (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to TXX %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tasmax','TXX')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('tasmax','TXX')
    for i in range(nyrs):
        y = styr+i
        print "computing TXX for year ",y
        #fn = fname + str(y) + ".nc"
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        if (i==0):
            txt = "cdo -m 1e+20 monmax " + fn + " " + ofallmon
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 monmax " + fn + " junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc " + ofallmon
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncrename -h -v tasmax,txx " + ofallmon
    os.system(txtcmd)
    #create yearly summary file
    txtcmd = "cdo -m 1e+20 yearmax "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall

def TNN (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to TNN %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tasmin','TNN')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('tasmin','TNN')
    for i in range(nyrs):
        y = styr+i
        print "computing TNN for year ",y
        #fn = fname + str(y) + ".nc"
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        if (i==0):
            txt = "cdo -m 1e+20 monmin " + fn + " " + ofallmon
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 monmin " + fn + " junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc " + ofallmon
            os.system(txt)
    #modify variable name and other attributes
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncrename -h -v tasmin,tnn " + ofallmon
    os.system(txtcmd)
    #create yearly summary file
    txtcmd = "cdo -m 1e+20 yearmin "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall

def TX90 (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to TX90 %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tasmax','TX90')
    for i in range(nyrs):
        y = styr+i
        print "computing TX90 for year ",y
        #fn = fname + str(y) + ".nc"
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        if (i==0):
            txt = "cdo -m 1e+20 yearpctl,90 "+fn+" -yearmin "+fn+" -yearmax "+fn+" "+ofall
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 yearpctl,90 "+fn+" -yearmin "+fn+" -yearmax "+fn+" junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc "+ofall
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncrename -h -v tasmax,tx90 " + ofall
    os.system(txtcmd)
    return ofall

def TXnorm (fname='',styr=0,enyr=0):
    #to create a file with 365 time steps and a TXnorm value for each cell, cat all files together
    #uses a 5-day running mean
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to TXnorm %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tasmax','TXnorm')
    ofbig = OUTTEMP+"/"+fn_nodir+"all."+str(styr)+"-"+str(enyr)+".nc"
    txtcmd = "ncrcat -n "+str(nyrs)+",4,1 "+OUTTEMP+"/"+fn_nodir+str(styr)+".nc " + ofbig
    if not os.path.exists(ofbig):
        os.system(txtcmd)
        print "created combined file ",ofbig
    else:
        print "combined file already exists: ",ofbig
    txtcmd = "cdo -m 1e+20 ydrunmean,5 "+ofbig+" "+ofall
    os.system(txtcmd)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncrename -h -v tasmax,TXnorm " + ofall
    os.system(txtcmd)
    return ofall

def FD (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to FD %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tasmin','FD')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('tasmin','FD')
    for i in range(nyrs):
        y = styr+i
        print "computing FD for year ",y
        #fn = fname + str(y) + ".nc"
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 eca_fd -addc,273.15 -selmon,"+str(j)+" "+fn+" "+ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 eca_fd -addc,273.15 -selmon,"+str(j)+" "+fn+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
    #modify variable name and other attributes
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "frost_days_index_per_time_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",fd " + ofallmon
    os.system(txtcmd)
    #create yearly summary file
    txtcmd = "cdo -m 1e+20 yearsum "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall

#cuts off last year to have global complete coverage
def GSL (fnamen='',fnamex='',fnamemask='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to FD %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fnx_nodir = string.split(fnamex,"/")[-1]
    fnn_nodir = string.split(fnamen,"/")[-1]
    fn_nodir = fnx_nodir.replace('tasmax','tas')
    ofall = OUTROOT+"/"+fnx_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tasmax','GSL')
    ofbig = OUTTEMP+"/"+fn_nodir+"all."+str(styr)+"-"+str(enyr)+".nc"
    for i in range(nyrs):
        y = styr+i
        fnx = OUTTEMP + "/" + fnx_nodir + str(y) + ".nc"
        fnn = OUTTEMP + "/" + fnn_nodir + str(y) + ".nc"
        fn = fnx.replace('tasmax','tas')
        if not (os.path.exists(fnx) and os.path.exists(fnn)):
            if y == enyr:
                print 'infile not found: ',fnx,fnn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fnx,fnn
        #calc mean daily temp if doesn't already exist
        if not os.path.exists(fn):
            print "calculating daily avg temp\n"
            txt = "cdo -m 1e+20 divc,2.0 -add "+fnn+" "+fnx+" "+fn
            os.system(txt)
            txtcmd = "ncrename -h -v tasmin,tas " + fn
            os.system(txtcmd)
    #create big file of tavg for all years if it doesn't already exist
    txtcmd = "ncrcat -n "+str(nyrs)+",4,1 "+OUTTEMP+"/"+fn_nodir+str(styr)+".nc " + ofbig
    if not os.path.exists(ofbig):
        os.system(txtcmd)
        print "created combined file ",ofbig
    else:
        print "combined file already exists: ",ofbig
    #truncate the last year
    ey=styr+nyrs-2
    txt = "cdo -m 1e+20 selyear,"+str(styr)+","+str(ey)+" -eca_gsl -addc,273.15 "+ofbig+" "+fnamemask+" "+ofall
    os.system(txt)
    #modify variable name and other attributes
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "thermal_growing_season_length"
    txtcmd = "ncrename -h -v "+txtnewvar + ",gsl " + ofall
    os.system(txtcmd)
    return ofall

def TXnorm (fname='',styr=0,enyr=0):
    #to create a file with 365 time steps and a TXnorm value for each cell, cat all files together
    #uses a 5-day running mean
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to TXnorm %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tasmax','TXnorm')
    ofbig = OUTTEMP+"/"+fn_nodir+"all."+str(styr)+"-"+str(enyr)+".nc"
    txtcmd = "ncrcat -n "+str(nyrs)+",4,1 "+OUTTEMP+"/"+fn_nodir+str(styr)+".nc " + ofbig
    if not os.path.exists(ofbig):
        os.system(txtcmd)
        print "created combined file ",ofbig
    else:
        print "combined file already exists: ",ofbig
    txtcmd = "cdo -m 1e+20 ydrunmean,5 "+ofbig+" "+ofall
    os.system(txtcmd)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncrename -h -v tasmax,TXnorm " + ofall
    os.system(txtcmd)
    return ofall

def HWDI (fnamex='',reffile='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to HWDI %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fnx_nodir = string.split(fnamex,"/")[-1]
    ofall = OUTROOT+"/"+fnx_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tasmax','HWDI')
    for i in range(nyrs):
        y = styr+i
        fnx = OUTTEMP + "/" + fnx_nodir + str(y) + ".nc"
        print "Now working on HWDI for year ",y
        if not (os.path.exists(fnx)):
            if y == enyr:
                print 'infile not found: ',fnx,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fnx
        if  not os.path.exists(reffile):
            raise "reffile not found",reffile
        if (i==0):
            txt = "cdo -m 1e+20 eca_hwdi "+fnx+" "+reffile+" "+ofall
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 eca_hwdi "+fnx+" "+reffile+" junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc "+ofall
            os.system(txt)
    #modify variable name and other attributes
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "heat_wave_duration_index_wrt_mean_of_reference_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",hwdi " + ofall
    os.system(txtcmd)
    return ofall

def CD18 (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to CD18 %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    ofall = OUTROOT+"/"+fname+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tas','CD18')
    ofallmon = OUTROOT+"/"+fname+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('tas','CD18')
    for i in range(nyrs):
        y = styr+i
        fn = OUTTEMP + "/" + fname + str(y) + ".nc"
        print "Now working on CD18 for year ",y
        if not (os.path.exists(fn)):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        txt = "cdo -m 1e+20 mul -subc,18 "+fn+" -gtc,18 "+fn+" "+OUTTEMP+"/junk_cd18_oneyear.nc"
        os.system(txt)
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            fx = " -selmon,"+str(j)+" "+OUTTEMP+"/junk_cd18_oneyear.nc"
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 timsum "+fx+" "+ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 timsum "+fx+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
    #modify variable name and other attributes
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "tas"
    txtcmd = "ncrename -h -v "+txtnewvar + ",cd18 " + ofallmon
    os.system(txtcmd)
    #create yearly summary file
    txtcmd = "cdo -m 1e+20 yearsum "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall

def HD18 (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to HD18 %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    ofall = OUTROOT+"/"+fname+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tas','HD18')
    ofallmon = OUTROOT+"/"+fname+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('tas','HD18')
    for i in range(nyrs):
        y = styr+i
        fn = OUTTEMP + "/" + fname + str(y) + ".nc"
        print "Now working on HD18 for year ",y
        if not (os.path.exists(fn)):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 eca_hd,18 -addc,273.15 -selmon,"+str(j)+" "+fn+" "+ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 eca_hd,18 -addc,273.15 -selmon,"+str(j)+" "+fn+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
    #modify variable name and other attributes
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "heating_degree_days_per_time_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",hd18 " + ofallmon
    os.system(txtcmd)
    #create yearly summary file
    txtcmd = "cdo -m 1e+20 yearsum "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall

def GD10 (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to GD10 %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    ofall = OUTROOT+"/"+fname+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('tas','GD10')
    ofallmon = OUTROOT+"/"+fname+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('tas','GD10')
    for i in range(nyrs):
        y = styr+i
        fn = OUTTEMP + "/" + fname + str(y) + ".nc"
        print "Now working on GD10 for year ",y
        if not (os.path.exists(fn)):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        txt = "cdo -m 1e+20 mul -subc,10 "+fn+" -gtc,10 "+fn+" "+OUTTEMP+"/junk_gd10_oneyear.nc"
        os.system(txt)
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            #fx = " -selmon,"+str(j)+" "+fn
            fx = " -selmon,"+str(j)+" "+OUTTEMP+"/junk_gd10_oneyear.nc"
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 timsum "+fx+" "+ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 timsum "+fx+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
    #modify variable name and other attributes
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "tas"
    txtcmd = "ncrename -h -v "+txtnewvar + ",gd10 " + ofallmon
    os.system(txtcmd)
    #create yearly summary file
    txtcmd = "cdo -m 1e+20 yearsum "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall

#generic ref period percentile for temperatures only (precip needs to be masked)
def Tref (fname='',pctl='',varin='',styr=0,enyr=0):
    #to create a file with 366 time steps and a TNN value for each cell
    #uses a 5-day running mean
    if pctl <= 0 or pctl >=100:
        raise 'percentile out of range'
    if not styr > 1899 and enyr < 2008 and (enyr>styr):
        raise 'incorrect args passed to Tref %s %d %d' % (fname,styr,enyr)
    if varin == 'tasmin':
        varout = 'TN'+str(pctl)+'Pref'
    elif varin == 'tasmax':
        varout = 'TX'+str(pctl)+'Pref'
    elif varin == 'tas':
        varout = 'TG'+str(pctl)+'Pref'
    else:
        raise "invalid input variable in Tref\n"
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace(varin,varout)
    ofbig = OUTTEMP+"/"+fn_nodir+"all."+str(styr)+"-"+str(enyr)+".nc"
    txtcmd = "ncrcat -n "+str(nyrs)+",4,1 "+OUTTEMP+"/"+fn_nodir+str(styr)+".nc " + ofbig
    if not os.path.exists(ofbig):
        os.system(txtcmd)
        print "created combined file ",ofbig
    else:
        print "combined file already exists: ",ofbig
    ofpartflist=[]
    #slice big file spatially - 18 zonal slices
    for i in range(18):
        lat1=-90+(i*10)
        lat2=-90+((i+1)*10)
        filepart="sellonlatbox,0,360,"+str(lat1)+","+str(lat2)
        ofjunk= OUTTEMP+"/"+varin+"_ref_ofbigpart.nc"
        ofpart= OUTTEMP+"/"+varin+"_ref_ydrunpctl"+str(pctl)+"."+str(i)+".nc"
        txt="cdo -m 1e+20 sellonlatbox,0,360,"+str(lat1)+","+str(lat2)+" "+ofbig+" "+ofjunk
        os.system(txt)
        txtcmd = "cdo -m 1e+20 ydrunpctl,"+str(pctl)+",5 "+ofjunk+" -ydrunmin,5 "+ofjunk+" -ydrunmax,5 "+ofjunk+" "+ofpart
        os.system(txtcmd)
        ofpartflist.append(ofpart)
    print "finished separate files, putting them back together..."
    CdmsUtils.MosaicFiles(ofpartflist,varin,ofall)
    #done with aggregation
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncrename -h -v "+varin+","+varout.lower()+" "+ofall
    os.system(txtcmd)
    return ofall

def TP (fname='',pctl='',varin='',reffile='',styr=0,enyr=0):
    if pctl != 10 and pctl != 90:
        raise 'percentile out of range'
    if not styr > 1899 and enyr < 2008 and (enyr>styr):
        raise 'incorrect args passed to Tref %s %d %d' % (fname,styr,enyr)
    if varin == 'tasmin':
        varout = 'TN'+str(pctl)+'P'
        if pctl == 10:
            varlong="cold_nights_percent_wrt_10th_percentile_of_reference_period"
        else:
            varlong="warm_nights_percent_wrt_90th_percentile_of_reference_period"
    elif varin == 'tasmax':
        varout = 'TX'+str(pctl)+'P'
        if pctl == 10:
            varlong="very_cold_days_percent_wrt_10th_percentile_of_reference_period"
        else:
            varlong="very_warm_days_percent_wrt_90th_percentile_of_reference_period"
    elif varin == 'tas':
        varout = 'TG'+str(pctl)+'P'
        if pctl == 10:
            varlong="cold_days_percent_wrt_10th_percentile_of_reference_period"
        else:
            varlong="warm_days_percent_wrt_90th_percentile_of_reference_period"
    else:
        raise "invalid input variable in TP\n"
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace(varin,varout)
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace(varin,varout)
    fnref_nodir = string.split(reffile,"/")[-1]
    cdocmd="eca_"+varout.lower()
    for i in range(nyrs):
        y = styr+i
        print "computing "+varout+" for year ",y
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        #txtcmd = "cdo -m 1e+20 -add "+reffile+" -sub "+fn+" "+fn+" "+ reffile_with_timesteps
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 "+cdocmd+" -selmon,"+str(j)+" "+fn+" -selmon,"+str(j)+" "+reffile+" "+ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 "+cdocmd+" -selmon,"+str(j)+" "+fn+" -selmon,"+str(j)+" "+reffile+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    #change new variable name created by CDO to desired variable name:
    txtcmd = "ncrename -h -v "+varlong + ","+varout+" "+ ofallmon
    os.system(txtcmd)
    #create yearly summary file - weight by number of days per month
    txtcmd = "cdo -m 1e+20 divdpy -yearsum -muldpm "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall
