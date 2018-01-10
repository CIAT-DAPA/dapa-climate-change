iDir <- "Z:/DATA/WP2/06_Clustering_analyses/data/bc_0_25deg_amz"
varList <- c("tmin", "tmax", "prec", "dswrf")
gcmList <- c("bcc_csm1_1", "bcc_csm1_1_m","csiro_mk3_6_0", "gfdl_cm3", "ipsl_cm5a_lr","miroc_esm", "miroc_esm_chem","miroc_miroc5", "ncc_noresm1_m", "mri_cgcm3")
periodList <- c('2020_2049', '2040_2069')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
countyList <- c("Ucayali", "Napo")

fn_ls <- c()
for(county in countyList){
  for (gcm in gcmList){
    for (period in periodList){
      for (rcp in rcpList){
        for (var in varList){
          
          fn_name <- paste(iDir, "/", gcm, "/", period, "/", rcp, "/", county, "/", var, "/bc_qmap_", var, "_", period, ".RData", sep="")
          if (!file.exists(fn_name)){
            
            fn_ls <- rbind(fn_ls, cbind(county, gcm, period, rcp, var))
            
            
          }
          
        }
      }
    }
  }
  
}

colnames(fn_ls) <- c("county", "gcm", "period", "rcp", "var")
write.csv(fn_ls, paste0(iDir, "/file_missing.csv"), quote = F, row.names = F)

