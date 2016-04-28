#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015
stop("!")

#cdf plots with unc. for each cluster and stress profile

#load libraries
#library(sp); library(maptools); library(raster); library(rgeos); library(rasterVis)
#library(ggplot2); library(grid); library(gridExtra)
#data(wrld_simpl)

#directories
#wd <- "/nfs/a101/earjr/rice-future-tpe"
wd <- "~/Leeds-work/rice-future-tpe"
res_dir <- paste(wd,"/oryza_output",sep="")
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_dir <- paste(wd,"/gcm_meteorology",sep="")
fig_dir <- paste(wd,"/figures",sep="")
an_dir <- paste(res_dir,"/analysis",sep="")

#construct list of final weather stations
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))
loc_list$allyears <- T
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_dir,"/loc_",wst_name,"/obs",sep="")
  if (!file.exists(wst_odir)) {loc_list$allyears[which(loc_list$id == wst)] <- F}
}
loc_list <- loc_list[which(loc_list$allyears),]
loc_list <- loc_list[which(loc_list$id != ".IPGO.00007"),]
row.names(loc_list) <- NULL

#list of RCPs, GCMs, bc methods, co2 responses
rcplist <- c("rcp26","rcp45","rcp60","rcp85")
co2list <- c("High","Low")
bclist <- c("cf","del")
gcmlist <- list.files(paste(gcm_dir,"/loc_CNPAF1/gcm",sep=""))
gcmlist <- gcmlist[which(gcmlist != "cesm1_cam5")]
gcmlist <- gcmlist[which(gcmlist != "ncar_ccsm4")]
gcmlist <- gcmlist[which(gcmlist != "mri_cgcm3")]
gcmlist <- gcmlist[which(gcmlist != "ipsl_cm5a_lr")]

#load historical
#note: cluster 1: LFE; cluster 2: HFE; cluster 3: FE
load(file=paste(an_dir,"/yield_probability_historical.RData",sep=""))
his_quant <- quant_df; rm(quant_df)
his_quant$env_name <- NA
his_quant$env_name[which(his_quant$env_cluster == 1)] <- "LFE"
his_quant$env_name[which(his_quant$env_cluster == 2)] <- "HFE"
his_quant$env_name[which(his_quant$env_cluster == 3)] <- "FE"

#note: stress 1: S1, stress 2: S2, stress 3: S3
his_quant$stress_name <- "all"
his_quant$stress_name[which(his_quant$env_name == "HFE" & his_quant$stress_cluster == 1)] <- "S1"
his_quant$stress_name[which(his_quant$env_name == "HFE" & his_quant$stress_cluster == 2)] <- "S2"
his_quant$stress_name[which(his_quant$env_name == "FE" & his_quant$stress_cluster == 1)] <- "S1"
his_quant$stress_name[which(his_quant$env_name == "FE" & his_quant$stress_cluster == 2)] <- "S2"
his_quant$stress_name[which(his_quant$env_name == "FE" & his_quant$stress_cluster == 3)] <- "S3"
his_quant$stress_name[which(his_quant$env_name == "LFE" & his_quant$stress_cluster == 1)] <- "S1"
his_quant$stress_name[which(his_quant$env_name == "LFE" & his_quant$stress_cluster == 2)] <- "S3"
his_quant$stress_name[which(his_quant$env_name == "LFE" & his_quant$stress_cluster == 3)] <- "S2"

#list of clusters
cluslist <- unique(his_quant$env_cluster)
clusname <- unique(his_quant$env_name)

#determine frequency
run_data <- read.csv(paste(res_dir,"/OP_historical.csv",sep="")) #this is the seasonal output (for end-of-season yield)
run_data$Lat[which(run_data$Station == "Catalao")] <- loc_list$lat[which(loc_list$municipio == "CATALAO")]
run_data$Long[which(run_data$Station == "Catalao")] <- loc_list$lon[which(loc_list$municipio == "CATALAO")]
run_data$Lat[which(run_data$Station == "Cristalina")] <- loc_list$lat[which(loc_list$municipio == "CRISTALINA")]
run_data$Long[which(run_data$Station == "Cristalina")] <- loc_list$lon[which(loc_list$municipio == "CRISTALINA")]

st_data <- read.csv(paste(res_dir,"/env_cluster.csv",sep=""), sep=";")
st_data$station <- as.vector(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[1]}))
st_data$soil <- as.vector(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[2]}))
st_data$sdate <- as.vector(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[3]}))
st_data$lat <- as.numeric(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[4]}))
st_data$lon <- as.numeric(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[5]}))
st_data$municipio <- as.vector(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[6]}))
st_data$year <- as.numeric(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[7]}))
st_data$sdate[which(st_data$sdate == "1/11")] <- "01/11"

run_data$Em_Dat <- paste(run_data$Em_Dat)
run_data <- merge(run_data, st_data[,c("clust","station","soil","sdate","year")],
                  by.x=c("Station","Soil","Em_Dat","Harvest_Year"),
                  by.y=c("station","soil","sdate","year"), all.x=T)
names(run_data)[grep("clust",names(run_data),fixed=T)] <- "stress_cluster"
names(run_data)[grep("Cluster",names(run_data),fixed=T)] <- "env_cluster"

freq_his <- as.data.frame(table(run_data[,c("stress_cluster","env_cluster")]))
freq_his$stress_cluster <- as.numeric(paste(freq_his$stress_cluster))
freq_his$env_cluster <- as.numeric(paste(freq_his$env_cluster))
fr_hfe <- sprintf("%2.1f",round(sum(freq_his$Freq[which(freq_his$env_cluster == 2)]) / sum(freq_his$Freq) * 100, 1))
fr_lfe <- sprintf("%2.1f",round(sum(freq_his$Freq[which(freq_his$env_cluster == 1)]) / sum(freq_his$Freq) * 100, 1))
fr_fe <- sprintf("%2.1f",round(sum(freq_his$Freq[which(freq_his$env_cluster == 3)]) / sum(freq_his$Freq) * 100, 1))

#put frequency in his_quant
his_quant$freq <- NA
his_quant$freq[which(his_quant$env_name == "HFE" & his_quant$stress_cluster == 0)] <- sum(freq_his$Freq[which(freq_his$env_cluster == 2)]) / sum(freq_his$Freq) * 100
his_quant$freq[which(his_quant$env_name == "LFE" & his_quant$stress_cluster == 0)] <- sum(freq_his$Freq[which(freq_his$env_cluster == 1)]) / sum(freq_his$Freq) * 100
his_quant$freq[which(his_quant$env_name == "FE" & his_quant$stress_cluster == 0)] <- sum(freq_his$Freq[which(freq_his$env_cluster == 3)]) / sum(freq_his$Freq) * 100

#historical plot of environments
pdf(paste(fig_dir,"/fig_4_CDF_historical.pdf",sep=""), height=7,width=10,pointsize=17)
par(mar=c(5,5,1,1),las=1,lwd=1.5)
plot(as.numeric(his_quant[1,paste("p",0:100,sep="")]),0:100*0.01,ty="l",col="blue",lwd=2.5,lty=1,xlim=c(0,8000),
     ylim=c(0,1),xlab=expression(Yield~(kg~ha^{-1})),ylab="Cumulative probability")
lines(as.numeric(his_quant[3,paste("p",0:100,sep="")]),0:100*0.01,ty="l",col="dark green",lwd=2.5,lty=1)
lines(as.numeric(his_quant[8,paste("p",0:100,sep="")]),0:100*0.01,ty="l",col="red",lwd=2.5,lty=1)
abline(h=0.5)
grid()
legend("bottomright",lty=1,col=c("blue","dark green","red"),box.lwd=NA,bg=NA,lwd=2.5,
       legend=c(paste("HFE: ",fr_hfe," %",sep=""),
                paste("FE:   ",fr_fe," %",sep=""),
                paste("LFE: ",fr_lfe," %",sep="")),cex=1.1)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_4_CDF_historical.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_4_CDF_historical.png",sep=""))
setwd("~")

###
#plot historical yield CDF of stress_cluster, for each env_cluster
for (clus in clusname) {
  #clus <- clusname[1]
  hisq <- his_quant[which(his_quant$env_name == clus & his_quant$stress_cluster != 0),]
  stress_list <- sort(unique(hisq$stress_name))
  
  pdf(paste(fig_dir,"/fig_5_CDF_",clus,"_historical.pdf",sep=""), height=7,width=10,pointsize=17)
  leg_s1 <- leg_s2 <- leg_s3 <- NULL
  for (stress in stress_list) {
    #stress <- stress_list[2]
    hisq_t <- hisq[which(hisq$stress_name == stress),]
    tfreq <- freq_his$Freq[which(freq_his$stress_cluster == hisq_t$stress_cluster & freq_his$env_cluster == hisq_t$env_cluster)]
    tfreq <- tfreq / sum(freq_his$Freq[which(freq_his$env_cluster == hisq_t$env_cluster)]) * 100
    
    #update his_quant frequency
    his_quant$freq[which(his_quant$env_name == clus & his_quant$stress_name == stress)] <- tfreq
    
    if (stress == "S1") {
      par(mar=c(5,5,1,1),las=1,lwd=1.5)
      plot(as.numeric(hisq_t[,paste("p",0:100,sep="")]),0:100*0.01,ty="l",col="blue",lwd=2.5,lty=1,xlim=c(0,8000),
           ylim=c(0,1),xlab=expression(Yield~(kg~ha^{-1})),ylab="Cumulative probability")
      leg_s1 <- paste("SP1: ",sprintf("%2.1f",tfreq)," %",sep="")
    } else if (stress == "S2") {
      lines(as.numeric(hisq_t[,paste("p",0:100,sep="")]),0:100*0.01,ty="l",col="dark green",lwd=2.5,lty=1)
      leg_s2 <- paste("SP2: ",sprintf("%2.1f",tfreq)," %",sep="")
    } else if (stress == "S3") {
      lines(as.numeric(hisq_t[,paste("p",0:100,sep="")]),0:100*0.01,ty="l",col="red",lwd=2.5,lty=1)
      leg_s3 <- paste("SP3: ",sprintf("%2.1f",tfreq)," %",sep="")
    }
  }
  abline(h=0.5)
  grid()
  legend("bottomright",lty=1,col=c("blue","dark green","red"),box.lwd=NA,bg=NA,lwd=2.5,
         legend=c(leg_s1,leg_s2,leg_s3),cex=1.1)
  dev.off()
  setwd(fig_dir)
  system(paste("convert -verbose -density 300 fig_5_CDF_",clus,"_historical.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_5_CDF_",clus,"_historical.png",sep=""))
  setwd("~")
}
###


###
#save updated probability data.frame
if (!file.exists(paste(an_dir,"/yield_probability_historical_named.RData",sep=""))) {
  save(his_quant,file=paste(an_dir,"/yield_probability_historical_named.RData",sep=""))
}


###
#plot yield CDF of env_cluster fut (separate co2) in single graph
for (rcp in rcplist) {
  #rcp <- rcplist[4]
  
  #load dataset
  load(file=paste(an_dir,"/yield_probability_",rcp,".RData",sep=""))
  fut_quant <- quant_df
  fut_quant$env_name <- NA
  fut_quant$stress_name <- "all"
  fut_quant$freq <- NA
  
  ### update fut_quant
  #for each gcm, bc_method, co2p, determine which of the env clusters is HFE, FE, LFE
  #sort by median and then assign value
  for (gcm in gcmlist) {
    for (bc in bclist) {
      for (co2p in co2list) {
        #gcm <- gcmlist[1]; bc <- bclist[1]; co2p <- co2list[1]
        tfut_df <- fut_quant[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p),]
        tdf <- tfut_df[which(tfut_df$stress_cluster == 0),c("env_cluster","p50")]
        eclus <- tdf$env_cluster[order(tdf$p50,decreasing=T)]
        
        #update fut_quant data.frame
        fut_quant$env_name[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$env_cluster == eclus[1])] <- "HFE"
        fut_quant$env_name[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$env_cluster == eclus[2])] <- "FE"
        fut_quant$env_name[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$env_cluster == eclus[3])] <- "LFE"
        
        #read simulation data
        co2i <- substr(co2p,1,1)
        fname <- paste(res_dir,"/Arquivos_J2/",co2p,"/","J2_op_",co2i,"_method_",bc,"_",rcp,"_",gcm,".csv",sep="")
        run_data <- read.csv(fname, sep=";")
        
        #calculate frequency of environments and of stress profiles
        freq_fut <- as.data.frame(table(run_data[,c("stress_cluster","env_cluster")]))
        freq_fut$stress_cluster <- as.numeric(paste(freq_fut$stress_cluster))
        freq_fut$env_cluster <- as.numeric(paste(freq_fut$env_cluster))
        fr_hfe <- sum(freq_fut$Freq[which(freq_fut$env_cluster == eclus[1])]) / sum(freq_fut$Freq) * 100
        fr_lfe <- sum(freq_fut$Freq[which(freq_fut$env_cluster == eclus[2])]) / sum(freq_fut$Freq) * 100
        fr_fe <- sum(freq_fut$Freq[which(freq_fut$env_cluster == eclus[3])]) / sum(freq_fut$Freq) * 100
        
        #put frequencies into data.frame
        fut_quant$freq[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$env_cluster == eclus[1] & fut_quant$stress_cluster == 0)] <- fr_hfe
        fut_quant$freq[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$env_cluster == eclus[2] & fut_quant$stress_cluster == 0)] <- fr_fe
        fut_quant$freq[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$env_cluster == eclus[3] & fut_quant$stress_cluster == 0)] <- fr_lfe
        
        #per env_cluster, determine order of stress pattern (1 for highest yield, and so on)
        for (clus  in cluslist) {
          #clus <- 1
          strdf <- tfut_df[which(tfut_df$env_cluster == clus & tfut_df$stress_cluster != 0),c("stress_cluster","p50")]
          sclus <- strdf$stress_cluster[order(strdf$p50,decreasing=T)]
          for (sc in 1:length(sclus)) {
            fut_quant$stress_name[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$env_cluster == clus & fut_quant$stress_cluster == sclus[sc])] <- paste("S",sc,sep="")
            fut_quant$freq[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$env_cluster == clus & fut_quant$stress_cluster == sclus[sc])] <- freq_fut$Freq[which(freq_fut$env_cluster == clus & freq_fut$stress_cluster == sclus[sc])] / sum(freq_fut$Freq[which(freq_fut$env_cluster == clus)]) * 100
          }
        }
      }
    }
  }
  
  ### save updated probability data.frame
  if (!file.exists(paste(an_dir,"/yield_probability_",rcp,"_named.RData",sep=""))) {
    save(fut_quant,file=paste(an_dir,"/yield_probability_",rcp,"_named.RData",sep=""))
  }
  
  ### loop clusters, plot CDF of yield all env_cluster
  for (co2p in co2list) {
    #co2p <- co2list[1]
    futq <- fut_quant[which(fut_quant$stress_cluster == 0 & fut_quant$co2 == co2p),]
    pdf(paste(fig_dir,"/fig_4_CDF_",co2p,"_",rcp,".pdf",sep=""), height=7,width=10,pointsize=17)
    for (clus in clusname) {
      #clus <- clusname[1]
      futq_s <- futq[which(futq$env_name == clus),]
      
      #calculate median and 5-95 % of future cdf
      futq_plw <- apply(futq_s[,paste("p",0:100,sep="")], 2, FUN=function(x) {quantile(x,probs=0.25,na.rm=T)})
      futq_p50 <- apply(futq_s[,paste("p",0:100,sep="")], 2, FUN=function(x) {quantile(x,probs=0.50,na.rm=T)})
      futq_pup <- apply(futq_s[,paste("p",0:100,sep="")], 2, FUN=function(x) {quantile(x,probs=0.75,na.rm=T)})
      
      #calculate median and 5-95 % of frequency
      clusf_p50 <- sprintf("%2.1f",median(futq_s$freq, na.rm=T))
      clusf_plw <- sprintf("%2.1f",quantile(futq_s$freq, probs=0.25, na.rm=T))
      clusf_pup <- sprintf("%2.1f",quantile(futq_s$freq, probs=0.75, na.rm=T))
      
      if (clus == "HFE") {
        par(mar=c(5,5,1,1),las=1,lwd=1.5)
        plot(as.numeric(futq_p50),0:100*0.01,ty="l",col="blue",lwd=2.5,lty=1,xlim=c(0,8000),
             ylim=c(0,1),xlab=expression(Yield~(kg~ha^{-1})),ylab="Cumulative probability")
        polygon(x=c(as.numeric(futq_plw), rev(as.numeric(futq_pup))), y=c(0:100*0.01,rev(0:100*0.01)), border=NA, col=rgb(red=0,green=100,blue=255,alpha=50,maxColorValue=255))
        leg_hfe <- paste("HFE: ",clusf_p50," [",clusf_plw," - ",clusf_pup,"] %",sep="")
      } else if (clus == "FE") {
        lines(as.numeric(futq_p50),0:100*0.01,ty="l",col="dark green",lwd=2.5,lty=1)
        polygon(x=c(as.numeric(futq_plw), rev(as.numeric(futq_pup))), y=c(0:100*0.01,rev(0:100*0.01)), border=NA, col=rgb(red=0,green=51,blue=0,alpha=50,maxColorValue=255))
        leg_fe <- paste("FE:   ",clusf_p50," [",clusf_plw," - ",clusf_pup,"] %",sep="")
      } else if (clus == "LFE") {
        lines(as.numeric(futq_p50),0:100*0.01,ty="l",col="red",lwd=2.5,lty=1)
        polygon(x=c(as.numeric(futq_plw), rev(as.numeric(futq_pup))), y=c(0:100*0.01,rev(0:100*0.01)), border=NA, col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
        leg_lfe <- paste("LFE: ",clusf_p50," [",clusf_plw," - ",clusf_pup,"] %",sep="")
      }
    }
    abline(h=0.5)
    grid()
    legend("bottomright",lty=1,col=c("blue","dark green","red"),box.lwd=NA,bg=NA,lwd=2.5,
           legend=c(leg_hfe,leg_fe,leg_lfe),cex=1.1)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 fig_4_CDF_",co2p,"_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_4_CDF_",co2p,"_",rcp,".png",sep=""))
    setwd("~")
  }
  
  ### loop clusters, plot CDF of yield per stress_cluster in each env_cluster
  for (co2p in co2list) {
    #co2p <- co2list[1]
    futq <- fut_quant[which(fut_quant$stress_cluster != 0 & fut_quant$co2 == co2p),]
    for (clus in clusname) {
      #clus <- clusname[1]
      futq_s <- futq[which(futq$env_name == clus),]
      stress_list <- sort(unique(futq_s$stress_name))
      pdf(paste(fig_dir,"/fig_5_CDF_",clus,"_",co2p,"_",rcp,".pdf",sep=""), height=7,width=10,pointsize=17)
      leg_s1 <- leg_s2 <- leg_s3 <- NULL
      for (stress in stress_list) {
        #stress <- stress_list[1]
        futq_t <- futq_s[which(futq_s$stress_name == stress),]
        
        #calculate median, and upper and lower limits % of future cdf
        futq_plw <- apply(futq_t[,paste("p",0:100,sep="")], 2, FUN=function(x) {quantile(x,probs=0.25,na.rm=T)})
        futq_p50 <- apply(futq_t[,paste("p",0:100,sep="")], 2, FUN=function(x) {quantile(x,probs=0.50,na.rm=T)})
        futq_pup <- apply(futq_t[,paste("p",0:100,sep="")], 2, FUN=function(x) {quantile(x,probs=0.75,na.rm=T)})
        
        #calculate median and 5-95 % of frequency
        clusf_p50 <- sprintf("%2.1f",median(futq_t$freq, na.rm=T))
        clusf_plw <- sprintf("%2.1f",quantile(futq_t$freq, probs=0.25, na.rm=T))
        clusf_pup <- sprintf("%2.1f",quantile(futq_t$freq, probs=0.75, na.rm=T))
        
        if (stress == "S1") {
          par(mar=c(5,5,1,1),las=1,lwd=1.5)
          plot(as.numeric(futq_p50),0:100*0.01,ty="l",col="blue",lwd=2.5,lty=1,xlim=c(0,8000),
               ylim=c(0,1),xlab=expression(Yield~(kg~ha^{-1})),ylab="Cumulative probability")
          polygon(x=c(as.numeric(futq_plw), rev(as.numeric(futq_pup))), y=c(0:100*0.01,rev(0:100*0.01)), border=NA, col=rgb(red=0,green=100,blue=255,alpha=50,maxColorValue=255))
          leg_s1 <- paste("SP1: ",clusf_p50," [",clusf_plw," - ",clusf_pup,"] %",sep="")
        } else if (stress == "S2") {
          lines(as.numeric(futq_p50),0:100*0.01,ty="l",col="dark green",lwd=2.5,lty=1)
          polygon(x=c(as.numeric(futq_plw), rev(as.numeric(futq_pup))), y=c(0:100*0.01,rev(0:100*0.01)), border=NA, col=rgb(red=0,green=51,blue=0,alpha=50,maxColorValue=255))
          leg_s2 <- paste("SP2: ",clusf_p50," [",clusf_plw," - ",clusf_pup,"] %",sep="")
        } else if (stress == "S3") {
          lines(as.numeric(futq_p50),0:100*0.01,ty="l",col="red",lwd=2.5,lty=1)
          polygon(x=c(as.numeric(futq_plw), rev(as.numeric(futq_pup))), y=c(0:100*0.01,rev(0:100*0.01)), border=NA, col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
          leg_s3 <- paste("SP3: ",clusf_p50," [",clusf_plw," - ",clusf_pup,"] %",sep="")
        }
      }
      abline(h=0.5)
      grid()
      legend("bottomright",lty=1,col=c("blue","dark green","red"),box.lwd=NA,bg=NA,lwd=2.5,
             legend=c(leg_s1,leg_s2,leg_s3),cex=1.1)
      dev.off()
      setwd(fig_dir)
      system(paste("convert -verbose -density 300 fig_5_CDF_",clus,"_",co2p,"_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_5_CDF_",clus,"_",co2p,"_",rcp,".png",sep=""))
      setwd("~")
    }
  }
}


