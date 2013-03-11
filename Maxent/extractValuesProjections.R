require(raster)
require("gplots")

###Arguments
spID <- "coffea_arabica"
inputDir <- "D:/CIAT/Articles/Maxent_Nicaragua"
outFolder <- paste(inputDir, "/mxe_outputs", sep="")
outName <- paste(outFolder, "/sp-", spID, "_swd", sep="")
occFile <- paste(inputDir, "/occurrence_files/", spID, ".csv", sep="")


for (i in 0:25){
 
  bslEMN <- raster(paste(outName, "/crossval/", spID, "_", i, "_Baseline.asc", sep=""))
  
  cat("\n")
  coords = read.csv(occFile)[2:3]
  data_mat = extract(bslEMN, coords)
  testMatrix=cbind(coords, data_mat)
  names(testMatrix)[3]=paste("baseline", sep="")
  
  write.csv(testMatrix,paste(outName, "/crossval/", spID, "_", i, "_Baseline.csv", sep=""), row.names=F)
}



####Extract baseline data 
bslEMN <- raster(paste(outName, "/crossval/", spID, "_avg.asc", sep=""))
cat("\n")
coords = read.csv(occFile)[2:3]
data_mat = extract(bslEMN, coords)
testMatrix=cbind(coords, data_mat)
names(testMatrix)[3]=paste("baseline", sep="")

write.csv(testMatrix,paste(outName, "/crossval/", spID, "_bsl_avg.csv", sep=""), row.names=F)


####Extract projections data 
projectionList <- c("bccr_bcm2_0","cccma_cgcm3_1_t47","cnrm_cm3","csiro_mk3_0","csiro_mk3_5","gfdl_cm2_0","gfdl_cm2_1","giss_model_er","ingv_echam4","inm_cm3_0","ipsl_cm4","miroc3_2_medres","miub_echo_g","mpi_echam5","mri_cgcm2_3_2a","ncar_ccsm3_0","ncar_pcm1","ukmo_hadcm3","ukmo_hadgem1")
cat("Extracting values of the projections...", "\n")
				
for (prj in projectionList) {
	
  for (i in 1:25){
  
   	prjEMN <- raster(paste(outName, "/projections/", spID, "_", prj, "_f", i,".asc", sep=""))
  	
    cat(paste(outName, "/projections/", spID, "_", prj, "_f", i,".asc", sep=""))
   	cat("\n")
  	# data = lapply(listasc, FUN=raster)
  	
    coords = read.csv(occFile)[2:3]
  	data_mat = extract(prjEMN, coords)
  	testMatrix=cbind(coords, data_mat)
  	names(testMatrix)[3]=paste(prj, sep="")
  	
    write.csv(testMatrix,paste(outName, "/projections/extracts/", spID, "_", prj, "_", i, ".csv", sep=""), row.names=F)
  	
  }
}	

for (prj in projectionList) {
     
  	prjESD <- raster(paste(outName, "/projections/averages/", spID, "_", prj, "_ESD.asc", sep=""))
  	
    cat(paste(outName, "/projections/averages/", spID, " ", prj, " ESD.asc", sep=""))
  	cat("\n")
  	
    # data = lapply(listasc, FUN=raster)
  	coords = read.csv(occFile)[2:3]
  	data_mat = extract(prjESD, coords)
  	testMatrix=cbind(coords, data_mat)
  	names(testMatrix)[3]=paste(prj, sep="")
  	
    write.csv(testMatrix,paste(outName, "/projections/extracts/", spID, "_", prj, "_ESD.csv", sep=""), row.names=F)
}