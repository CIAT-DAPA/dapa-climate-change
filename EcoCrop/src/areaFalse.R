#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Assessing the accuracy of EcoCrop calibration runs using
	#1. Test and train omission rates with S>0, for Psuit, Tsuit, Suit
	#2. RMSQE from 1 vs 0/1 (S>0)
	#3. Maximum entropy, entropy line slope

require(rgdal)
require(raster)

