#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#download AgMERRA data for solar radiation, tmin and tmax, for further analyses

#files:
#* http://data.giss.nasa.gov/impacts/agmipcf/agmerra/AgMERRA_1980_prate.nc4
#* http://data.giss.nasa.gov/impacts/agmipcf/agmerra/AgMERRA_1980_rhstmax.nc4
#* http://data.giss.nasa.gov/impacts/agmipcf/agmerra/AgMERRA_1980_srad.nc4
#* http://data.giss.nasa.gov/impacts/agmipcf/agmerra/AgMERRA_1980_tavg.nc4
#* http://data.giss.nasa.gov/impacts/agmipcf/agmerra/AgMERRA_1980_tmax.nc4
#* http://data.giss.nasa.gov/impacts/agmipcf/agmerra/AgMERRA_1980_tmin.nc4
#* http://data.giss.nasa.gov/impacts/agmipcf/agmerra/AgMERRA_1980_wndspd.nc4

wd <- "~/Leeds-work/p4s-csa/hh-analyses/AgMERRA_data"
setwd(wd)

baseurl <- "http://data.giss.nasa.gov/impacts/agmipcf/agmerra"
varlist <- c("srad","tmax","tmin","prate")
for (vname in varlist) {
  #vname <- varlist[2]
  for (year in 1980:2010) {
    #year <- 1981
    cat("\n...downloading variable=",vname,"for year=",year,"\n")
    if (!file.exists(paste("AgMERRA_",year,"_",vname,".nc4",sep=""))) {
      system(paste("wget ",baseurl,"/AgMERRA_",year,"_",vname,".nc4",sep=""))
    }
  }
}





