#!/usr/local/cdat5.2/bin/python

"""Module for computing precipitation extreme stats mostly using CDO utilities"""

import sys,os,string
import cdms2
import string
import datetime
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

def Ptot (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to Ptot %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    #ofall = ofall.replace('tasmax','TXX')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    #ofallmon = ofallmon.replace('tasmax','TXX')
    for i in range(nyrs):
        y = styr+i
        print "computing Ptot for year ",y
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
            txt = "cdo -m 1e+20 monsum " + fn + " " + ofallmon
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 monsum " + fn + " junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc " + ofallmon
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    #txtcmd = "ncrename -h -v tasmax,txx " + ofallmon
    #os.system(txtcmd)
    #create yearly summary file
    txtcmd = "cdo -m 1e+20 yearsum "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall

def CDD (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to CDD %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','CDD')
#    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
#    ofallmon = ofallmon.replace('pr','CDD')
    for i in range(nyrs):
        y = styr+i
        print "computing CDD for year ",y
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
            txt = "cdo -m 1e+20 eca_cdd " + fn + " " + ofall
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 eca_cdd " + fn + " junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc " + ofall
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "consecutive_dry_days_index_per_time_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",cdd " + ofall
    os.system(txtcmd)
    txtnewvar = "number_of_cdd_periods_with_more_than_5days_per_time_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",cdd5 " + ofall
    os.system(txtcmd)
    #split out second variable into new file
    ofall2 = ofall.replace('CDD','CDD5')
    txtcmd = "ncks -O -h -v cdd5 " + ofall+" "+ofall2
    os.system(txtcmd)
    #remove cdd5 from outfile - -O for forced overwrite
    txtcmd = "ncks -O -h -v cdd " + ofall+" "+ofall
    os.system(txtcmd)
    return ofall

def R02 (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R02 %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R02')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('pr','R02')
    for i in range(nyrs):
        y = styr+i
        print "computing R02 for year ",y
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
                txt = "cdo -m 1e+20 eca_pd,0.2 -selmon,"+str(j)+" "+ fn + " " + ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 eca_pd,0.2 -selmon,"+str(j)+" "+fn+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "precipitation_days_index_per_time_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",r02 " + ofallmon
    os.system(txtcmd)
    #create yearly summary file
    txtcmd = "cdo -m 1e+20 yearsum "+ofallmon+" "+ofall
    os.system(txtcmd)
    return ofall

def R5D (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R5D %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R5D')
#    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
#    ofallmon = ofallmon.replace('pr','CDD')
    for i in range(nyrs):
        y = styr+i
        print "computing R5D for year ",y
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
            txt = "cdo -m 1e+20 eca_rx5day " + fn + " " + ofall
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 eca_rx5day " + fn + " junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc " + ofall
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "highest_five_day_precipitation_amount_per_time_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",r5d " + ofall
    os.system(txtcmd)
    #remove extra variable(s) created by CDO:
    txtcmd = "ncks -O -h -v r5d " + ofall+" "+ofall
    os.system(txtcmd)
    return ofall

#90th percentile - wet days only
def R90 (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R90 %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R90')
    for i in range(nyrs):
        y = styr+i
        print "computing R90 for year ",y
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
    txtcmd = "ncrename -h -v pr,r90 " + ofall
    os.system(txtcmd)
    return ofall

#ref period only - wet days only
def R90ref (fname='',styr=0,enyr=0):
    #to create a file with 366 time steps and a 90pctl value for each cell, cat all files together:
    #uses a 5-day running mean
    if not styr > 1899 and enyr < 2006 and (enyr>styr):
        raise 'incorrect args passed to R90ref %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R90ref')
    ofbig = OUTTEMP+"/"+fn_nodir+"all."+str(styr)+"-"+str(enyr)+".nc"
    txtcmd = "ncrcat -n "+str(nyrs)+",4,1 "+OUTTEMP+"/"+fn_nodir+str(styr)+".nc " + ofbig
    if not os.path.exists(ofbig):
        os.system(txtcmd)
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
        os.system(txt)
        txt1 = "cdo ifthen -gtc,1 "+ofjunk+" "+ofjunk+" "+ofjunkm
        os.system(txt1)
        txtcmd = "cdo -m 1e+20 ydrunpctl,90,5 "+ofjunkm+" -ydrunmin,5 "+ofjunkm+" -ydrunmax,5 "+ofjunkm+" "+ofpart
        os.system(txtcmd)
        ofpartflist.append(ofpart)
    print "finished separate files, putting them back together..."
    CdmsUtils.MosaicFiles(ofpartflist,'pr',ofall)
    #done with aggregation
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncrename -h -v pr,r90ref " + ofall
    os.system(txtcmd)
    return ofall

#future periods only - wrt wet day 90th pctile
def R90P (fname='',reffile='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R90P %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R90P')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('pr','R90P')
    fnref_nodir = string.split(reffile,"/")[-1]
    for i in range(nyrs):
        y = styr+i
        print "computing R90P for year ",y
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        #mask for minimum precipitation amount
        txt1 = "cdo ifthen -gtc,1 "+fn+" "+fn+" "+OUTTEMP+"/junk_r90p.nc"
        os.system(txt1)
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 eca_r90p -selmon,"+str(j)+" "+OUTTEMP+"/junk_r90p.nc -selmon,"+str(j)+" "+reffile+" "+ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 eca_r90p -selmon,"+str(j)+" "+OUTTEMP+"/junk_r90p.nc -selmon,"+str(j)+" "+reffile+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
        print "Done with months, now doing annual\n"
        if (i==0):
            txt = "cdo -m 1e+20 eca_r90p "+OUTTEMP+"/junk_r90p.nc "+reffile+" "+ofall
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 eca_r90p "+OUTTEMP+"/junk_r90p.nc "+reffile+" junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc "+ofall
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "wet_days_wrt_90th_percentile_of_reference_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",r90p " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncrename -h -v "+txtnewvar + ",r90p " + ofall
    os.system(txtcmd)
    return ofall

#future periods only
def R90PTOT (fname='',reffile='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to R90PTOT %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','R90PTOT')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('pr','R90PTOT')
    fnref_nodir = string.split(reffile,"/")[-1]
    for i in range(nyrs):
        y = styr+i
        print "computing R90PTOT for year ",y
        fn = OUTTEMP + "/" + fn_nodir + str(y) + ".nc"
        if not os.path.exists(fn):
            if y == enyr:
                print 'infile not found: ',fn,' ...skipping last year'
                nyrs = nyrs-1
                break
            else:
                raise 'infile not found: ',fn
        #mask for minimum precipitation amount
        txt1 = "cdo ifthen -gtc,1 "+fn+" "+fn+" "+OUTTEMP + "/junk_r90p.nc"
        os.system(txt1)
        for j in [1,2,3,4,5,6,7,8,9,10,11,12]:
            if (i==0 and j==1):
                txt = "cdo -m 1e+20 eca_r90ptot -selmon,"+str(j)+" "+OUTTEMP + "/junk_r90p.nc -selmon,"+str(j)+" "+reffile+" "+ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 eca_r90ptot -selmon,"+str(j)+" "+OUTTEMP + "/junk_r90p.nc -selmon,"+str(j)+" "+reffile+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
        print "Done with months, now doing annual\n"
        if (i==0):
            txt = "cdo -m 1e+20 eca_r90ptot "+OUTTEMP + "/junk_r90p.nc "+reffile+" "+ofall
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 eca_r90ptot "+OUTTEMP + "/junk_r90p.nc "+reffile+" junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc "+ofall
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "precipitation_percent_due_to_R90p_days"
    txtcmd = "ncrename -h -v "+txtnewvar + ",r90ptot " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncrename -h -v "+txtnewvar + ",r90ptot " + ofall
    os.system(txtcmd)
    return ofall

def SDII (fname='',styr=0,enyr=0):
    if not styr > 1899 and enyr < 2101 and (enyr>styr):
        raise 'incorrect args passed to SDII %s %d %d' % (fname,styr,enyr)
    nyrs = enyr-styr+1
    fn_nodir = string.split(fname,"/")[-1]
    ofall = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".nc"
    ofall = ofall.replace('pr','SDII')
    ofallmon = OUTROOT+"/"+fn_nodir+str(styr)+"-"+str(enyr)+".monthly.nc"
    ofallmon = ofallmon.replace('pr','SDII')
    for i in range(nyrs):
        y = styr+i
        print "computing SDII for year ",y
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
                txt = "cdo -m 1e+20 eca_sdii -selmon,"+str(j)+" "+fn+" "+ofallmon
                os.system(txt)
            else:
                txt = "cdo -m 1e+20 eca_sdii -selmon,"+str(j)+" "+fn+" junk_mon.nc"
                os.system(txt)
                txt = "cdo cat junk_mon.nc "+ofallmon
                os.system(txt)
        print "Done with months, now doing annual\n"
        if (i==0):
            txt = "cdo -m 1e+20 eca_sdii "+fn+" "+ofall
            os.system(txt)
        else:
            txt = "cdo -m 1e+20 eca_sdii "+fn+" junk_mon.nc"
            os.system(txt)
            txt = "cdo cat junk_mon.nc "+ofall
            os.system(txt)
    now = datetime.datetime.now()
    txthist = "Created on " + now.strftime("%Y-%m-%d %H:%M")
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a history,global,o,c,'"+txthist + "' " + ofall
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncatted -h -a institution,global,c,c,'"+txtinst + "' " + ofall
    os.system(txtcmd)
    #new variable name created by CDO:
    txtnewvar = "simple_daily_intensitiy_index_per_time_period"
    txtcmd = "ncrename -h -v "+txtnewvar + ",sdii " + ofallmon
    os.system(txtcmd)
    txtcmd = "ncrename -h -v "+txtnewvar + ",sdii " + ofall
    os.system(txtcmd)
    return ofall

####for spi, first create a timeseries object for each on-masked grid cell:
#>>> first_date = ts.Date('D', '2009-01-01')
#>>> series = ts.time_series([1, 2, 3, 4], start_date=first_date)
#>>> series
#timeseries([1 2 3 4],
#   dates = [01-Jan-2009 ... 04-Jan-2009],
#   freq  = D)
