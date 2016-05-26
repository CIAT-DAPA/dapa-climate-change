#!/usr/local/cdat5.2/bin/python

"""Module for computing percentile-based extreme stats using CDAT"""

import sys,os,string
import cdms2
from genutil import statistics
import cdtime
import string
import datetime
import numpy as np
import MV2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

OUTROOT = '/mnt/WorldBank1/out_stats'

#added as global institution attribute to output files
txtinst = "Santa Clara U.,Climate Central,The Nature Conservancy"

#rebuilts a global file from a list of slices - cut into zonal slices
def MosaicFiles (flist='',varin='',outf=''):
    if varin == 'tasmin':
        varout = 'TN90Pref'
    elif varin == 'tasmax':
        varout = 'TX90Pref'
    elif varin == 'pr':
        varout = 'R90Pref'
    else:
        raise "invalid input variable in MosaicFiles\n"
    #arrays are shaped: (time,360,720) (time,lats,lons) for half degree global
    elat=0
    for file in flist:
        print "working on file ",file
        f=cdms2.open(file)
        v=f(varin)
        if not len(v.getLatitude()[:]) == 20:
            raise 'incorrect number of lats in slice'
        elat=elat+len(v.getLatitude()[:])
        #newtime=np.array(range(366))
        if file == flist[0]:
            #new axis to put latitudes back together
            oldtime=v.getTime()
            tsteps=len(oldtime) #can be 365 or 366
            newlat=np.zeros((360,))
            newlat[:elat]= v.getLatitude()[:]
            newtime=cdms2.createAxis(np.arange(tsteps,dtype=np.float))
            newtime.id='time'
            newtime.axis="T"
            newtime.calendar=oldtime.calendar
            if tsteps == 366:
                newtime.units='days since 1960-1-1'
            elif tsteps == 365:
                newtime.units='days since 1950-1-1'
            elif tsteps == 360:
                newtime.units='days since 1950-1-1'
            else:
                print "time steps in input files should be 360, 365 or 366"
                sys.exit()
            v.setAxis(0,newtime)
            fixedtimeaxis=v.getTime()
            vmosaic=v
        else:
            axes=v.getAxisList()
            axes[0]=fixedtimeaxis
            v.setAxisList(axes)
            vmosaic= MV2.concatenate((vmosaic,v),axis=1) # concatenate over latitudes
            newlat[elat-20:elat]= v.getLatitude()[:]
        f.close()
    newlat=cdms2.createAxis(newlat)
    newlat.units= "degrees_north"
    newlat.id= "lat"
    newlat.long_name= "Latitude"
    newlat.axis="Y"
    axes=vmosaic.getAxisList()
    axes[1]=newlat
    vmosaic.setAxisList(axes)
    vmosaic.id=varin
    vmosaic.axis='TYX'
    f=cdms2.open(outf,'w')
    f.write(vmosaic)
    f.close()
    print "created combined file ",outf
