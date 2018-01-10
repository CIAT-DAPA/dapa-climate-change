iDir <- "Z:/DATA/WP2/06_Clustering_analyses/data/gcm_res_amz"
vLs <- c("tmin", "tmax", "pr", "rsds")

for (d in list.dirs(iDir, recursive=F, full.names = TRUE)){
  
  for(e in list.dirs(d, recursive=F, full.names = TRUE)[1]){
    
    #     for (f in list.dirs(e, recursive=F, full.names = TRUE)[2:3]){
    
    if(length(list.files(paste0(e, "/by-month"))) < 1200){ 
      
      print(e)
      
      #       }
      
    }
    
  }
  
}
