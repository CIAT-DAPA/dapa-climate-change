require(raster)

###Arguments
spID <- "coffea_arabica"
inputDir <- "D:/Maxent_Nicaragua"
outFolder <- paste(inputDir, "/mxe_outputs", sep="")
outName <- paste(outFolder, "/sp-", spID, "_1", sep="")
occFile <- paste(inputDir, "/occurrence_files/", spID, ".csv", sep="")



####Extract projections data 
projectionList <- c("bccr_bcm2_0","cccma_cgcm3_1_t47","cnrm_cm3","csiro_mk3_0","csiro_mk3_5","gfdl_cm2_0","gfdl_cm2_1","giss_model_er","ingv_echam4","inm_cm3_0","ipsl_cm4","miroc3_2_medres","miub_echo_g","mpi_echam5","mri_cgcm2_3_2a","ncar_ccsm3_0","ncar_pcm1","ukmo_hadcm3","ukmo_hadgem1")
cat("Extracting values of the projections...", "\n")
				
for (prj in projectionList) {

	prjEMN <- raster(paste(outName, "/projections/averages/", spID, "_", prj, "_EMN.asc", sep=""))
	cat(prj)
	cat(" EMN\n")
	# data = lapply(listasc, FUN=raster)
	coords = read.csv(occFile)[2:3]
	data_mat = extract(prjEMN, coords)
	testMatrix=cbind(coords, data_mat)
	names(testMatrix)[3]=paste(prj, sep="")
	write.csv(testMatrix,paste(outName, "/projections/averages/extracts/", spID, "_", prj, "_EMN.csv", sep=""), row.names=F)
	
	prjESD <- raster(paste(outName, "/projections/averages/", spID, "_", prj, "_ESD.asc", sep=""))
	cat(prj)
	cat(" EMN\n")
	# data = lapply(listasc, FUN=raster)
	coords = read.csv(occFile)[2:3]
	data_mat = extract(prjESD, coords)
	testMatrix=cbind(coords, data_mat)
	names(testMatrix)[3]=paste(prj, sep="")
	write.csv(testMatrix,paste(outName, "/projections/averages/extracts/", spID, "_", prj, "_ESD.csv", sep=""), row.names=F)
	
	}	