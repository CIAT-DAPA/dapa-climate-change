# Climate data revision
# H. Achicanoy
# CIAT, 2017

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

# Load future climate data

modelos <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
             "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

lapply(1:length(modelos), function(gcm_i){
  
  cat(paste("=== Processing GCM: ", modelos[gcm_i], "\n\n", sep = ""))
  
  gcm <- paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[gcm_i], "/Futuro/")
  
  # CORRECTING PROCESS
  
  # Step 1: Correct days a year when Tmax <= Tmin, it's mandatory to be consistant with Tmax > Tmin
  svDir <- paste("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[gcm_i], "/Futuro/version2017", sep = "")
  if(!dir.exists(svDir)){dir.create(path = svDir, recursive = T)}
  if(!file.exists(paste(svDir, "/Temperatura_2.RDat", sep = ""))){
    
    load(paste0(gcm, "Temperatura_2.Rdat"))
    # Information in a matrix: 8199 pixels, 30 years (but it seems that only the last year wasn't used)
    # Tmax[[year]] # matrix (8199 rows; 365-6 columns)
    # Tmin[[year]] # matrix (8199 rows; 365-6 columns)
    # And there's an algorithm to match properly pixels according to their coordinates
    
    cat("CORRECTING PROCESS. Step 1\n")
    for(i in 1:length(Tmax)){
      # Identify pixel/day combination when Tmax <= Tmin
      cellID <- which(Tmax[[i]] <= Tmin[[i]])
      rc_fromCell <- arrayInd(cellID, dim(Tmax[[i]]))
      # If exists at least one combination, to process
      if(length(rc_fromCell) > 0){
        cat("Tmax <= Tmin. Processing correction\n")
        for(j in 1:nrow(rc_fromCell)){
          # Calculate the mean using the day before and after where the problem is located
          Tmax[[i]][rc_fromCell[j,1], rc_fromCell[j,2]] <- mean(Tmax[[i]][rc_fromCell[j,1], c(rc_fromCell[j,2]-1, rc_fromCell[j,2]+1)], na.rm = T)
          Tmin[[i]][rc_fromCell[j,1], rc_fromCell[j,2]] <- mean(Tmin[[i]][rc_fromCell[j,1], c(rc_fromCell[j,2]-1, rc_fromCell[j,2]+1)], na.rm = T)
        }; rm(j)
      }
      
      cat("Check the data again.\n")
      
      if(length(which(Tmax[[i]] <= Tmin[[i]])) == 0){
        cat("Successful process. Now Tmax > Tmin\n")
      } else {
        cat("Reprocess information\n")
      }
      
    }; rm(cellID, rc_fromCell, i)
    save(Tmax, Tmin, file = paste(svDir, "/Temperatura_2.RDat", sep = ""))
  }
  
  # Step 2: Apply correction factor for Solar Radiation in cases where Srad is in another units
  if(!file.exists(paste(svDir, "/Srad.Rdat", sep = ""))){
    load(paste0(gcm, "Srad.Rdat"))
    # Nested list
    # Information by pixel and year: 8000 pixels, 29 years
    # Srad[[pixel]][[year]] # So each single object is a numeric vector (365-6 positions)
    
    cat("CORRECTING PROCESS. Step 2\n")
    for(i in 1:length(Srad)){
      for(j in 1:length(Srad[[i]])){
        if(length(which(Srad[[i]][[j]] < 0)) > 0){
          cat("Negative values of solar radiation\n")
        }
        if(length(which(Srad[[i]][[j]] > 100)) > 0){
          cat("Solar radiation without transformation. Applying correction factor\n")
          cnames <- colnames(Srad[[i]][[j]])
          Srad[[i]][[j]] <- as.numeric(Srad[[i]][[j]])/11.5740741
          names(Srad[[i]][[j]]) <- cnames; rm(cnames)
        }
        cat("Check the data again.\n")
        if(length(which(Srad[[i]][[j]] > 100)) == 0){
          cat("Successful process. Now Srad is in the proper units\n")
        } else {
          cat("Reprocess information\n")
        }
      }
    }; rm(i, j)
    save(Srad, file = paste(svDir, "/Srad.Rdat", sep = ""))
  }
  
  # Step 3: Correct days a year then Prec < 0, it's mandatory to be consistant with Prec >= 0
  if(!file.exists(paste(svDir, "/Precipitation.RDat", sep = ""))){
    load(paste0(gcm, "Precipitation.RDat"))
    # Nested list
    # Information by pixel and year: 8000 pixels, 29 years
    # Prec[[pixel]][[year]] # So each single object is a data.frame (1 rows; 365-6 columns)
    
    cat("CORRECTING PROCESS. Step 3\n")
    for(i in 1:length(Prec)){
      for(j in 1:length(Prec[[i]])){
        if(length(which(Prec[[i]][[j]] < 0)) > 0){
          cat("Negative values of daily precipitation. Replace by 0\n")
          Prec[[i]][[j]][,which(Prec[[i]][[j]] < 0)] <- 0
        }
        if(length(which(is.na(Prec[[i]][[j]]))) > 0){
          cat("Missing values of daily precipitation. Replace by 0\n")
          Prec[[i]][[j]][,which(is.na(Prec[[i]][[j]]))] <- 0
        }
        cat("Check the data again.\n")
        if(length(which(Prec[[i]][[j]] < 0)) == 0 & length(which(is.na(Prec[[i]][[j]]))) == 0){
          cat("Successful process. Now Prec has properly values\n")
        } else {
          cat("Reprocess information\n")
        }
      }
    }; rm(i, j)
    save(Prec, file = paste(svDir, "/Precipitation.RDat", sep = ""))
  }
  
  return(cat("Done\n"))
  
})
