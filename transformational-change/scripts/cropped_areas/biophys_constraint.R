###driving biophysical constraint (temp or prec) at decade of threshold crossing to transformation phase
## UR, May 2014
##corrected June 18th

require(raster); library(rgdal); library(maptools)
require(stringr)

#rcp and datasets 
dataset <- "cru" # "cru" "wcl"
rcp <- "rcp85" #rcp60 rcp85

#i/o directories
#b_dir <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/modelling/Cul_de_sacs"
#b_dir <- "/mnt/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/modelling/Cul_de_sacs"
b_dir <- "~/Leeds-work/cul-de-sacs"
#b_dir <- "/nfs/a101/earjr/cul-de-sacs"
base_run <- paste(b_dir,"/model_runs/",dataset,"_hist",sep="")

#future runs dir
fut_dir <- paste(b_dir,"/model_runs/",dataset,"_futclim_bc/",rcp,sep="")
#fut_dir <- paste("/nfs/a101/earjr/cul-de-sacs/model_runs/",dataset,"_futclim_bc/",rcp,sep="")

#output dirs
#out_dir <- paste("~/Google Drive/papers/transformational-adaptation") #mbp
out_dir <- paste(b_dir,"/analysis_outputs_cropped_areas/data_files_", dataset,"/","_biophys_constr",sep="") #mbp
if (!file.exists(out_dir)) {dir.create(out_dir,recursive=T)}
if (!file.exists(paste(out_dir,"/raster/",rcp,sep=""))) {dir.create(paste(out_dir,"/raster/",rcp,sep=""),recursive=T)}

#Africa regional shapefiles
shp_dir <- paste(b_dir,"/_shapefiles/Africa_regions/extent/mask",sep="")

#list of GCMs
if (!file.exists(paste(b_dir,"/scratch/gcm_list.RData",sep=""))) {
  gcm_list <- list.files(fut_dir)
} else {
  load(file=paste(b_dir,"/scratch/gcm_list.RData",sep=""))
}

#crops 
thresh_val <- read.csv(paste(b_dir,"/model_data/thresholds.csv", sep=""))
thresh_val <- thresh_val[which(thresh_val$dataset == dataset),]
thresh_val <- thresh_val[c(1:3,10:13,15:16),]
thresh_val$dataset <- NULL; row.names(thresh_val) <- 1:nrow(thresh_val)
thresh_val$AUC <- thresh_val$MinROCdist <- thresh_val$MaxKappa <- NULL
names(thresh_val)[2] <- "value"
thresh_val$value <- thresh_val$value * 100
thresh_val$crop[which(thresh_val$crop == "fmillet_EAF_SAF")] <- "fmillet"
thresh_val$crop[which(thresh_val$crop == "yam_WAF")] <- "yam"
thresh_val$crop <- paste(thresh_val$crop)
crops <- thresh_val$crop

#list of years
yr_list <- c(2006:2098)

#load thres cross files
dfil_crop = paste(b_dir, "/analysis_outputs_cropped_areas/data_files_", dataset, sep="")

#####PART 1 produce constraint rasters for whole SSA
tfun <- function(x) {
  if (is.na(x[1])) {
    return(NA)
  } else {
    if (x[1] == 2090) {
      return(2090) ##the 2090 now stands for crop doesnt become unsuit until 2100
    } else {
      #get decadal rasters of the thres cross time
      #x[1] gives the 20y period start year
      # the all_param stack has 1 + 93 (psuit) = 93 (tsuit) = 187 rasters
      d = as.numeric(x[1])
      #to get the right position of the starting year
      d = d-2014
      period = d:(d+19)#to get the 20 years
      
      ##call ecocrop Tsuit results for the detected decade 
      #take the mean of the 20 rasters
      eco_tsuit = mean(x[period], na.rm = TRUE)
      
      #call Psuit rasters and do same process again
      e = d+93
      period2 = e:(e+19)
      eco_psuit = mean(x[period2], na.rm = TRUE)
      
      #p = 8951
      #p = 8845
      #look if Tsuit or Psuit is lower to determine main constraint for suitabilty
      constr = as.numeric(eco_tsuit < eco_psuit) + 2
      
      #TRUE = Tsuit is constraing, FALSE = Psuit is constraint
      #1 = TRUE, 0 = FALSE then plus 2, results in 3 and 2
      #3 = temp constraint, 2 = prec constraint
      return(constr)
    }
  }
}


for (c in 1:length(crops)){
  #c = 1
  cat("...processing",crops[c],"\n")
  for (g in 1:length(gcm_list)) {
    #g = 1
    cat("...processing",dataset,"_",rcp,"_", gcm_list[g],"\n")
    if (!file.exists(paste(out_dir,"/raster/", rcp,"/",gcm_list[g],"_constr_", crops[c],".tif", sep = ""))) {
      #load processed output
      load(file=paste(dfil_crop,"/",crops[c],"/crossing_crosscheck_",rcp,"_",gcm_list[g],".RData",sep=""))
      thres_cross = cross_stk_con[[2]]
      
      #remove any previous objects and load data for this GCM
      rm(list=c("cross_stk","rs_freq_his","cross_stk_con"))
      
      #load t- and p- suitability rasters
      eco_tsuit <- stack(paste(fut_dir,"/",gcm_list[g],"/r1i1p1/",2006:2098,"/",crops[c],"_tsuit.tif",sep=""))
      eco_psuit <- stack(paste(fut_dir,"/",gcm_list[g],"/r1i1p1/",2006:2098,"/",crops[c],"_psuit.tif",sep=""))
      
      # the all_param stack has 1 (thres cross decade/year) + 93 (psuit) = 93 (tsuit) = 187 rasters
      all_param = stack(thres_cross, eco_tsuit, eco_psuit)
      
      #calculate constraints
      constraint = calc(all_param, fun = tfun)
      writeRaster(constraint, paste(out_dir,"/raster/", rcp,"/",gcm_list[g],"_constr_", crops[c],".tif", sep = ""),format="GTiff")  
      #writeRaster(thres_cross, paste(outdir, "test/","cross_", crops[c],".tif", sep = ""))
    }
  }
}

##############

## Part 2: calculate on regional basis the percentage of pixels that crossed because
## of prec or temp as most impt constraint (precentage of pixels where thres was crossed)

##########################
#####Regional analysis and percentage of pixel calc
##mask by region
region = list.files(paste(shp_dir), full.names=TRUE)
region = region[-grep("Thumbs.db",region)]
#region = region[grep(".tif",region)]
region = lapply(region, FUN=raster)
region = stack(region)
#plot(region)
region[[1]][!is.na(region[[1]][])]=1
region[[2]][!is.na(region[[2]][])]=2
region[[3]][!is.na(region[[3]][])]=3
region[[4]][!is.na(region[[4]][])]=4
region[[5]][!is.na(region[[5]][])]=5
region=merge(region)
##this is a mask for whole SSA
mask=region>=0
#plot(mask)


##for output matrix
crops_names = c("Banana_Temp_constr", "Banana_Prec_constr","Cassava_Temp_constr", "Cassava_Prec_constr",       
                "Common_Bean_Temp_constr", "Common_Bean_Prec_constr", "Finger_Millet_Temp_constr", "Finger_Millet_Prec_constr",
                "GrNut_Temp_constr", "GrNut_Prec_constr", "Pearl_Millet_Temp_constr", "Pearl_Millet_Prec_constr", 
                "Sorghum_Temp_constr", "Sorghum_Prec_constr", "Yam_Temp_constr", "Yam_Prec_constr", "Maize_Temp_constr", "Maize_Prec_constr" )

stats_names = c("CAF Mean", "EAF Mean", "SAF Mean", "SAH Mean","WAF Mean" ,"CAF Standard Deviation", "EAF Standard Deviation", "SAF Standard Deviation", "SAH Standard Deviation", "WAF Standard Deviation")

res=list() #results list for the 5 region, will contain per of both constraints for each
           #crop and model
res[[1]] = matrix(ncol = 19, nrow = 18, dimnames=list(crops_names, gcm_list))# making a dataframes that we will fill, 8 rops 19 gcms
res[[2]] = matrix(ncol = 19, nrow = 18, dimnames=list(crops_names, gcm_list))
res[[3]] = matrix(ncol = 19, nrow = 18, dimnames=list(crops_names, gcm_list))
res[[4]] = matrix(ncol = 19, nrow = 18, dimnames=list(crops_names, gcm_list))
res[[5]] = matrix(ncol = 19, nrow = 18, dimnames=list(crops_names, gcm_list))

#to combine the stats information of each region at the end
stats_summary = matrix(ncol = 10, nrow = 18, dimnames=list(crops_names, stats_names))
region_code=c("CAF","EAF","SAF","SAH","WAF")

for(i in unique(region)){
  #i = 2
  #plot(id_region)
  id_region=region#(raster with each region having a different number)
  id_region[!id_region==i]=NA #Tells R to give all regions except specified region NA
  id_region[!is.na(id_region)]=1 #Gives per i defined region a value of 1 
  #plot(id_region)
  
  for(c in 1:length(crops)){
    #c = 1
    for(g in 1:length(gcm_list)){
      #g = 1
      cat("...processing",crops[c], " and" , gcm_list[g], "\n")
    
      bio_constr = raster(paste0(out_dir, "/raster/",rcp,"/",gcm_list[g], "_constr_",crops[c], ".tif"))
      reg_constr = bio_constr*id_region
      vals = freq(reg_constr)#frequencies of the pixel values
      #print(vals)
      #plot(reg_constr)
      
      #This calculates and assigns the respective codes to the regions- 
      #0 = Never suit. 1= Suit Future. 10=Suit now/ Future no. 11=now and suit in the future
      #%in% similar function to grep
      if("2" %in% vals[,1]){ID_2 =as.numeric(vals[which(vals[,1]==2),2])} else {ID_2 = 0}
      if("3" %in% vals[,1]){ID_3 =as.numeric(vals[which(vals[,1]==3),2])} else {ID_3 = 0}
      if("2090" %in% vals[,1]){ID_2090 =as.numeric(vals[which(vals[,1]==2090),2])} else {ID_2090 = 0}
      #ID 2 means Prec is constraint
      #ID 3 means Temp is constraint
      
      #total number of pixels where thres was crossed
      tot_pix = ID_2 + ID_3    
        
      res[[i]][(c+c-1), g] = (ID_3/tot_pix)*100
      res[[i]][(c+c), g] = (ID_2/tot_pix)*100
    }
  }
  
  #calculate GCM mean and standard deveation for each crop
  mean = apply(res[[i]],1,mean, na.rm = TRUE) 
  sd = apply(res[[i]],1,sd, na.rm = TRUE) 
  res[[i]] = cbind(res[[i]],mean,sd)
  
  stats_summary[,(i)] = mean
  stats_summary[,(i+5)] = sd
  write.csv(res[[i]],paste(out_dir,"/",region_code[i],"_",rcp,"_bio_constr.csv",sep=""))
}

write.csv(stats_summary,paste(out_dir,"/",dataset,"_", rcp ,"_summ_bio_constr.csv",sep=""))
save(list=c("res"),file=paste(out_dir,"/",rcp,"_bio_constr.RData",sep=""))

load(file=paste(out_dir,"/",rcp,"_bio_constr.RData",sep=""))
xx <- res[[1]][,c(1:19)]
apply(xx, 1 , mean, na.rm=TRUE) 
apply(xx, 1 , sd, na.rm=TRUE) 

