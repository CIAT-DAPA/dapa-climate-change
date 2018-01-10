idir <- "//dapadfs/workspace_cluster_12/Kenya_KACCAL/data/bc_quantile_0_05deg_lat"
odir <- "E:/Kenya_CP/data/bc_quantile_0_05deg_lat"

perLs <- c("1971_2000","2021_2045", "2041_2065")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
cty <- "Kajiado"

for (dir in list.files(idir, full.names = T, include.dirs = F)){
  
  for (per in perLs){
    
    if (per == "1971_2000"){
      
      system(paste0("robocopy /z /e ", dir, "/", per, "/", cty, " ", odir, "/", basename(dir), "/", per, "/", cty))
      
    } else {
      
      for (rcp in rcpLs){
        
        system(paste0("robocopy /z /e ", dir, "/", per, "/", rcp, "/", cty, " ", odir, "/", basename(dir), "/", per, "/", rcp, "/", cty))
      
      }
    }
  }
  
}

