#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

library(raster)

#source directories
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

source(paste(src.dir,"/cmip5/09.glam-adap_test-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
maxiter <- 15 #to grab last optim values

#base and data directories
bDir <- "W:/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
runsDir <- paste(cropDir,"/runs/",runs_name,sep="")
adapDir <- paste(cropDir,"/adapt",sep="")

#load grid cells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#experimental set up
inList <- c("allin","bcrain")
CO2ExpList <- c("CO2_p1","CO2_p2","CO2_p3","CO2_p4")
sdList <- c(-7:7)

#load list of parameter sets
expList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expSel <- expList$EXPID[which(expList$ISSEL == 1)]

#list of GCMs
gcmList <- list.files(paste(runsDir,"/exp-33_outputs",sep=""),pattern="_ENS_")

###########
### details of runs
gcm_id <- 1
gcm <- gcmList[gcm_id]

typ_id <- 1
htyp <- paste("his_",inList[typ_id],sep="")
rtyp <- paste("rcp_",inList[typ_id],sep="")

co2_id <- 1
cpar <- CO2ExpList[co2_id]

exp_id <- 1
exp <- expSel[exp_id]
###


#############################################################################
#############################################################################
#############################################################################
## experiments of adaptation

#parameters i have to look at are:

#1. TE and TEN_MAX. TEN_MAX should not exceed 7 g/kg
#2. DHDT, without exceeding a maximum EOS HI of 0.66
#3. SLA (in theory), but 300 cm2/g is already in the upper bound of range, so maybe unrealistic
#4. thermal time from planting to flowering
#   a. increases should be beneficial for well-watered
#   b. decreases should be beneficial for water-stressed (if TT of pod fill is increased)
#5. increase thermal time from start of pod filling to maturity (GCPFLM+GCLMPHA)
#6. increase thermal time from flowering to LMAX (GCFLPF+GCPFLM) to get better LAI
#7. HTS


##
#durations beyond 150 days not to be considered when increases in thermal time were done
#runs with final HI values above >0.66 not to be considered

#some discussion:
#escape to terminal drought stress can be achieved through shortening crop duration
#so the question is whether or not avoiding TDS will be more benefficial than the
#resulting decrease in net assimilate production caused by reduction in cropping
#cycle


#other strategies: Reddy et al. (2003) report rainfall as the main constraint for gnut
#production. Possible analyses.

#1. fully irrigated run
#2. targeted irrigation. Basu and Ghosh (1996) at 
#   http://agropedialabs.iitk.ac.in/openaccess/sites/default/files/RA%2000287_0.pdf#page=39
#   state that:
#   a. protective irrigation (targeted to flowering and pod filling) can increase yields by 33-63%
#   b. pre-monsoon (15-30 days advance) sowing with 1-2 times initial irrigation can 
#      increase yields by 20%
#

#for discussion Narayanamoorthy (http://nrlp.iwmi.org/PDocs/DReports/Phase_01/12.%20Water%20Savings%20Technologies%20-%20Narayanmoorthy.pdf)
#indicate the potential of drip and sprinkler irrigation in India

#it may be worth trying:
# a. fully irrigated run
# b. irrigation during flowering+podfill stage (modify GLAM so that 
#    POT=T when after flowering)


#load experiments setup
adapRuns <- read.table(paste(adapDir,"/data/adapt.tab",sep=""),sep="\t",header=T)

#select location
loc <- 890 #

#load parameter set
hisDir <- paste(runsDir,"/exp-",exp,"_outputs/",gcm,"/",htyp,"_",loc,sep="")
rcpDir <- paste(runsDir,"/exp-",exp,"_outputs/",gcm,"/",rtyp,"_",cpar,"_",loc,sep="")

#load baseline
hisData <- paste(hisDir,"/output.RData",sep="")
load(hisData)
rm(ir_vls); rm(optimal); rm(optimised); rm(params); rm(setup)
ybas <- out_data[[2]]$DATA$YIELD
rm(out_data)

#load future
rcpData <- paste(rcpDir,"/output.RData",sep="")
load(rcpData)
yfut <- run_data$RUNS[[8]]$DATA$RFD$YIELD

#get parameter set
parset <- run_data$PARAMS

#get adaptation runs configured
adap_run <- cfg_adap_runs(runs_data=adapRuns,rcp_data=rcpData)

####
#update parameter set
#change min/max in range of parameters in parameter set before running if value
#being tested is larger than in pset
#

run_i <- 43
this_run <- adap_run$RUNS[run_i,]
run_id <- this_run$RUNID; 
this_run$RUNID <- NULL; 

parset_up <- update_params_adap(run_data=this_run,params=parset)


#remember that ICO2=1 modifies TEN_MAX and TE
#so just
#1. modify B_TE and B_TEN_MAX according to adapt
#2. 'bmass' TEN_MAX value = B_TEN_MAX
#3. modify TE from B_TE using the CO2 parameterisation rule







