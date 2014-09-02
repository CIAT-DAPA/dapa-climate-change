#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#make weather file for the two different treatments: WET_AMB, WET_FACE, DRY_AMB, DRY_FACE (separate folders)

#source make weather function (from robustness)
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))

#i/o directories
wd <- "~/Leeds-work/AgMIP-maize-phase-2"
wth_dir <- paste(wd,"/weather",sep="")

#load input file
wth_data <- read.csv(paste(wth_dir,"/weather_all.txt",sep=""),sep="\t")
wth_data$DATE <- paste(wth_data$DATE)

#site_details
s_details <- data.frame(NAME="Braunschweig, WS 10348",INSI="GEBR",LAT=52.29,LONG=10.45,
                        ELEV=81,TAV=-99,AMP=-99,REFHT=-99,WNDHT=-99)

###
#weather data for 2007 (WET_AMB)
twth_data <- wth_data[which(wth_data$YYYY == 2007),]
twth_data$SRAD_dry_plots <- NULL; twth_data$CO2_ambient <- NULL; twth_data$CO2_FACE <- NULL
twth_data$DATE <- paste(substr(twth_data$DATE,3,7))
twth_data$YYYY <- NULL; twth_data$MM <- NULL; twth_data$DD <- NULL

#for wet plots add irrigation amounts
twth_data$RAIN[202] <- twth_data$RAIN[202]+12
twth_data$RAIN[221] <- twth_data$RAIN[221]+11
twth_data$RAIN[223] <- twth_data$RAIN[223]+11

wx <- data.frame(DATE=twth_data$DATE,JDAY=1:365,SRAD=twth_data$SRAD_wet_plots,TMAX=twth_data$TMAX,
                 TMIN=twth_data$TMIN,RAIN=twth_data$RAIN)
wx$DATE <- paste(wx$DATE)

#write data (WET_AMB)
out_wdir <- paste(wth_dir,"/wet_amb_2007",sep="")
if (!file.exists(out_wdir)) {dir.create(out_wdir)}
wthfile <- paste(out_wdir,"/gebr0010012007.wth",sep="")
wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)

###
#weather data for 2007 (WET_FACE) --same data as AMB
#write data (WET_FACE)
out_wdir <- paste(wth_dir,"/wet_face_2007",sep="")
if (!file.exists(out_wdir)) {dir.create(out_wdir)}
wthfile <- paste(out_wdir,"/gebr0010012007.wth",sep="")
wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)


###
#weather data for 2008 (WET_AMB)
twth_data <- wth_data[which(wth_data$YYYY == 2008),]
twth_data$SRAD_dry_plots <- NULL; twth_data$CO2_ambient <- NULL; twth_data$CO2_FACE <- NULL
twth_data$DATE <- paste(substr(twth_data$DATE,3,7))
twth_data$YYYY <- NULL; twth_data$MM <- NULL; twth_data$DD <- NULL
twth_data <- twth_data[1:365,]

#for wet plots add irrigation amounts
twth_data$RAIN[179] <- twth_data$RAIN[179]+10
twth_data$RAIN[182] <- twth_data$RAIN[182]+20
twth_data$RAIN[205] <- twth_data$RAIN[205]+10
twth_data$RAIN[207] <- twth_data$RAIN[207]+10
twth_data$RAIN[210] <- twth_data$RAIN[210]+12
twth_data$RAIN[212] <- twth_data$RAIN[212]+12
twth_data$RAIN[214] <- twth_data$RAIN[214]+15
twth_data$RAIN[224] <- twth_data$RAIN[224]+15
twth_data$RAIN[248] <- twth_data$RAIN[248]+10
twth_data$RAIN[260] <- twth_data$RAIN[260]+5
twth_data$RAIN[262] <- twth_data$RAIN[262]+0

wx <- data.frame(DATE=twth_data$DATE,JDAY=1:365,SRAD=twth_data$SRAD_wet_plots,TMAX=twth_data$TMAX,
                 TMIN=twth_data$TMIN,RAIN=twth_data$RAIN)
wx$DATE <- paste(wx$DATE)

#write data
out_wdir <- paste(wth_dir,"/wet_amb_2008",sep="")
if (!file.exists(out_wdir)) {dir.create(out_wdir)}
wthfile <- paste(out_wdir,"/gebr0010012008.wth",sep="")
wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)

### 
#weather data for 2008 (WET_FACE)
twth_data <- wth_data[which(wth_data$YYYY == 2008),]
twth_data$SRAD_dry_plots <- NULL; twth_data$CO2_ambient <- NULL; twth_data$CO2_FACE <- NULL
twth_data$DATE <- paste(substr(twth_data$DATE,3,7))
twth_data$YYYY <- NULL; twth_data$MM <- NULL; twth_data$DD <- NULL
twth_data <- twth_data[1:365,]

#for wet plots add irrigation amounts
twth_data$RAIN[179] <- twth_data$RAIN[179]+10
twth_data$RAIN[182] <- twth_data$RAIN[182]+20
twth_data$RAIN[205] <- twth_data$RAIN[205]+10
twth_data$RAIN[207] <- twth_data$RAIN[207]+10
twth_data$RAIN[210] <- twth_data$RAIN[210]+12
twth_data$RAIN[212] <- twth_data$RAIN[212]+12
twth_data$RAIN[214] <- twth_data$RAIN[214]+0
twth_data$RAIN[224] <- twth_data$RAIN[224]+15
twth_data$RAIN[248] <- twth_data$RAIN[248]+0
twth_data$RAIN[260] <- twth_data$RAIN[260]+0
twth_data$RAIN[262] <- twth_data$RAIN[262]+5

wx <- data.frame(DATE=twth_data$DATE,JDAY=1:365,SRAD=twth_data$SRAD_wet_plots,TMAX=twth_data$TMAX,
                 TMIN=twth_data$TMIN,RAIN=twth_data$RAIN)
wx$DATE <- paste(wx$DATE)

#write data
out_wdir <- paste(wth_dir,"/wet_face_2008",sep="")
if (!file.exists(out_wdir)) {dir.create(out_wdir)}
wthfile <- paste(out_wdir,"/gebr0010012008.wth",sep="")
wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)


### DRY treatments
#weather data for 2007 (DRY_AMB)
twth_data <- wth_data[which(wth_data$YYYY == 2007),]
twth_data$SRAD_wet_plots <- NULL; twth_data$CO2_ambient <- NULL; twth_data$CO2_FACE <- NULL
twth_data$DATE <- paste(substr(twth_data$DATE,3,7))
twth_data$YYYY <- NULL; twth_data$MM <- NULL; twth_data$DD <- NULL

#for dry plots remove rainfall; Manderscheid et al. (2014) and excel file suggest
#shelves were installed from day 236 (end of Aug) until harvest 274 (end of Sept)
#a total of 9 mm were excluded from precipitation, but this does not match with
#11 % reported in the paper or the comment in the excel sheet. 
#11 % of this period would be ~15.5 mm. Nothing is said about distribution hence i 
#will assume uniformly distributed. 9 mm of this period is ~6.3 %
rfact <- 9 / sum(twth_data$RAIN[236:274])
twth_data$RAIN[236:274] <- twth_data$RAIN[236:274] * (1-rfact)

wx <- data.frame(DATE=twth_data$DATE,JDAY=1:365,SRAD=twth_data$SRAD_dry_plots,TMAX=twth_data$TMAX,
                 TMIN=twth_data$TMIN,RAIN=twth_data$RAIN)
wx$DATE <- paste(wx$DATE)

#write data
out_wdir <- paste(wth_dir,"/dry_amb_2007",sep="")
if (!file.exists(out_wdir)) {dir.create(out_wdir)}
wthfile <- paste(out_wdir,"/gebr0010012007.wth",sep="")
wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)

###
#weather data for 2007 (DRY_FACE)
#from the excel sheet it does seem like it is the same, so just write the same
out_wdir <- paste(wth_dir,"/dry_face_2007",sep="")
if (!file.exists(out_wdir)) {dir.create(out_wdir)}
wthfile <- paste(out_wdir,"/gebr0010012007.wth",sep="")
wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)


###
#weather data for 2008 (DRY_AMB)
twth_data <- wth_data[which(wth_data$YYYY == 2008),]
twth_data$SRAD_wet_plots <- NULL; twth_data$CO2_ambient <- NULL; twth_data$CO2_FACE <- NULL
twth_data$DATE <- paste(substr(twth_data$DATE,3,7))
twth_data$YYYY <- NULL; twth_data$MM <- NULL; twth_data$DD <- NULL
twth_data <- twth_data[1:365,]

#for dry plots remove rainfall
#rainout1: 12 mm between 185 and 186
rfact <- 12 / sum(twth_data$RAIN[185:186])
twth_data$RAIN[185:186] <- twth_data$RAIN[185:186] * (1-rfact)

#rainout2: 16 mm between 199 and 205
rfact <- 16 / sum(twth_data$RAIN[199:204])
twth_data$RAIN[199:204] <- twth_data$RAIN[199:204] * (1-rfact)

#rainout3: 29 mm between 235 and 238
rfact <- 29 / sum(twth_data$RAIN[235:238])
twth_data$RAIN[235:238] <- twth_data$RAIN[235:238] * (1-rfact)

#one irrigation event on dat 212 (20 mm)
twth_data$RAIN[212] <- twth_data$RAIN[212]+20

wx <- data.frame(DATE=twth_data$DATE,JDAY=1:365,SRAD=twth_data$SRAD_dry_plots,TMAX=twth_data$TMAX,
                 TMIN=twth_data$TMIN,RAIN=twth_data$RAIN)
wx$DATE <- paste(wx$DATE)

#write data
out_wdir <- paste(wth_dir,"/dry_amb_2008",sep="")
if (!file.exists(out_wdir)) {dir.create(out_wdir)}
wthfile <- paste(out_wdir,"/gebr0010012008.wth",sep="")
wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)

###
#weather data for 2008 (DRY_FACE)
#same as for DRY_AMB
out_wdir <- paste(wth_dir,"/dry_face_2008",sep="")
if (!file.exists(out_wdir)) {dir.create(out_wdir)}
wthfile <- paste(out_wdir,"/gebr0010012008.wth",sep="")
wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)


