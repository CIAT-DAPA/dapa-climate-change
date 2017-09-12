#######################################
###       Summary for all LAM       ###
#######################################
bdir<-"//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/"
load(paste(bdir,"change_suitable_areas_latam_regions_all.Rdata", sep=""))
#write.csv(out_reg, file ="//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/all_table_LAM.csv")
area_chg_mean <-aggregate(x=out_reg$area_chg, by=list(out_reg[,"crop"]), FUN=mean, na.rm =TRUE)
area_chg_sd <-aggregate(x=out_reg$area_chg, by=list(out_reg[,"crop"]), FUN=sd,na.rm =TRUE)
area_cur_mean <-aggregate(x=out_reg$hc_area, by=list(out_reg[,"crop"]), FUN=mean, na.rm =TRUE)
area_fut_mean <-aggregate(x=out_reg$fc_area, by=list(out_reg[,"crop"]), FUN=mean, na.rm =TRUE)
table <-cbind(area_chg_mean, area_chg_sd$x, area_cur_mean$x, area_fut_mean$x)
names(table)<-c("Crop","Mean suitability change (%)","SD (%)","Current area (km2)","Future area (km2)")
write.csv(table, file ="//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/summary_table_LAM.csv")

#######################################
###  Summary for LAM (by region)    ###
#######################################

load(paste(bdir,"change_suitable_areas_latam_regions.Rdata",sep=""))
write.csv(out_reg, file =paste(bdir,"all_table_regions.csv",sep=""))
area_chg_mean_reg <-aggregate(x=out_reg$area_chg, by=list(out_reg[,"crop"],out_reg[,"region"]), FUN=mean,na.rm =TRUE)
area_chg_sd_reg <-aggregate(x=out_reg$area_chg, by=list(out_reg[,"crop"],out_reg[,"region"]), FUN=sd,na.rm =TRUE)
area_cur_mean_reg <-aggregate(x=out_reg$hc_area, by=list(out_reg[,"crop"],out_reg[,"region"]), FUN=mean, na.rm =TRUE)
area_fut_mean_reg <-aggregate(x=out_reg$fc_area, by=list(out_reg[,"crop"],out_reg[,"region"]), FUN=mean, na.rm =TRUE)
table_reg <-cbind(area_chg_mean_reg, area_chg_sd_reg$x, area_cur_mean_reg$x, area_fut_mean_reg$x)
names(table_reg)<-c("Crop","Region","Mean suitability change (%)","SD (%)","Current area (km2)","Future area (km2)")
write.csv(table_reg, file =paste(bdir,"summary_table_regions.csv",sep=""))

##########################
### estimation of CI######
#########################
bdir = 'D:/tobackup/BID/'
out_reg = read.csv(paste(bdir,'all_table_regions_LAM.csv',sep=""),stringsAsFactors=F) #Create one table with regions and LAM
regions = unique(out_reg$region)
crops = unique(out_reg$crop)

agg.CI = array(NA,dim=c(6,14))
for (j in 1:6)  {
  for (c in 1:7)  {
    ind.dat = which(out_reg$region==regions[j] & out_reg$crop==crops[c] & out_reg$hc_area>6000)
    data = out_reg[ind.dat,]
    if (length(ind.dat)>0)  {
      boot = mat.or.vec(500,1)
      for (b in 1:500)  {boot.ind = sample(1:10,10,replace=T)  #sample models with replacement randomly
                         fut = mean(data[boot.ind,'fc_area'])  #select values for those models and take the mean
                         past = out_reg$hc_area[ind.dat[1]]
                         boot[b] = (fut - past )/past  * 100
      }
      #calculate % change across all 500 bootstrapped values
      agg.CI[j,(2*(c-1)+1):(2*c)] = quantile(boot,c(.025,.975))  #calculate 2.5 & 97.5 percentiles
      
    }
  } 
}

rownames(agg.CI) = c(regions)
colnames(agg.CI) = paste(rep(crops,each =2),c('_lower','_upper'),sep='') 
write.csv(agg.CI,file=paste(bdir,'EcoCrop_confInt.csv',sep=''))
