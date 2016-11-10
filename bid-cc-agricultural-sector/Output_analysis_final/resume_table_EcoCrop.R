#######################################
###       Summary for all LAM       ###
#######################################

load("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/change_suitable_areas_latam_regions_all.Rdata")

area_chg_mean <-aggregate(x=out_reg$area_chg, by=list(out_reg[,"crop"]), FUN=mean, na.rm =TRUE)
area_chg_sd <-aggregate(x=out_reg$area_chg, by=list(out_reg[,"crop"]), FUN=sd,na.rm =TRUE)
table <-cbind(area_chg_mean, area_chg_sd$x)
names(table)<-c("Crop","Mean suitability change (%)","SD (%)")
write.csv(table, file ="//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/summary_table_LAM.csv")

#######################################
###  Summary for LAM (by region)    ###
#######################################

load("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/change_suitable_areas_latam_regions.Rdata")
write.csv(out_reg, file ="//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/all_table_regions.csv")


out_reg = read.csv('D:/tobackup/BID/all_table_regions.csv',stringsAsFactors=F)
regions = unique(out_reg$region)
crops = unique(out_reg$crop)

agg.CI = array(NA,dim=c(6,14))
for (j in 1:6)  {
  for (c in 1:7)  {
    if (j<=5) {
      ind.dat = which(out_reg$region==regions[j] & out_reg$crop==crops[c] & out_reg$hc_area>6000)
    }  else{
      ind.dat = which(out_reg$crop==crops[c] & out_reg$hc_area>6000)  #tienes que hacer algo diferente para Latinoamerica!!
    }
    
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

rownames(agg.CI) = c(regions,'LAM')
colnames(agg.CI)[seq(1,14,2)] = paste(crops,'lower')
colnames(agg.CI)[seq(2,14,2)] = paste(crops,'upper')
write.csv(agg.CI,file='D:/tobackup/BID/EcoCrop_confInt.csv')


area_chg_mean_reg <-aggregate(x=out_reg$area_chg, by=list(out_reg[,"crop"],out_reg[,"region"]), FUN=mean,na.rm =TRUE)
area_chg_sd_reg <-aggregate(x=out_reg$area_chg, by=list(out_reg[,"crop"],out_reg[,"region"]), FUN=sd,na.rm =TRUE)
table_reg <-cbind(area_chg_mean_reg, area_chg_sd_reg$x)
names(table_reg)<-c("Crop","Region","Mean suitability change (%)","SD (%)")
write.csv(table_reg, file ="//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/summary_table_regions.csv")


