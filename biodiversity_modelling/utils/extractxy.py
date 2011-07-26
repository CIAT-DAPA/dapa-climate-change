#!/bin/env python

# -----------------------------------------------------#
# Script to extract values from a raster               #
# Licence: GPLv3                                       #
# Written by Johannes Signer (jmsigner@gmail.com)      #
# With a lot ideas and inspiration from:               #
# www.gis.usu.edu                                      #
# -----------------------------------------------------#

# Usage:
# python extractxy.py -f raster1.asc raster2.asc -xy my_coords.csv


import os, sys, time, gdal
import argparse
from gdalconst import *

# Get files to be read
parser = argparse.ArgumentParser()
parser.add_argument('-f', nargs='+', dest='files')
parser.add_argument('-xy', nargs=1, dest='xy')

args = parser.parse_args()
# Reead coordinates
f = open(args.xy[0], 'r')

# coordinates to get pixel values for
xValues = []
yValues = []

# skip header
f.readline()

for val in f:
   try:
      (x,y) = val.split(',')
      xValues.append(float(x))
      yValues.append(float(y))
   except:
      continue

# register all of the drivers
gdal.AllRegister()

results = []

for rast in args.files:
# list to store results
   tresults = []
# open the image
   ds = gdal.Open(rast, GA_ReadOnly)
   if ds is None:
      print 'coulnt find rast'
      sys.exit(1)

# get image size
   rows = ds.RasterYSize
   cols = ds.RasterXSize
   bands = ds.RasterCount

# get georeference info
   transform = ds.GetGeoTransform()
   xOrigin = transform[0]
   yOrigin = transform[3]
   pixelWidth = transform[1]
   pixelHeight = transform[5]

# loop through the coordinates
   for i in range(len(xValues)):
# get x,y
      x = xValues[i]
      y = yValues[i]
# compute pixel offset
      xOffset = int((x - xOrigin) / pixelWidth)
      yOffset = int((y - yOrigin) / pixelHeight)

# loop through the bands
      for j in range(bands):
         band = ds.GetRasterBand(j+1) # 1-based index
# read data and add the value to the string
         data = band.ReadAsArray(xOffset, yOffset, 1, 1)
         value = data[0,0]
         tresults.append(str(value))
   results.append(tresults)

# print out the data string
for i in range(len(xValues)):
   line = []
   line.append(str(xValues[i]))
   line.append(str(yValues[i]))

   for j in range(len(results)):
      line.append(str(results[j][i]))
   print ','.join(line)
