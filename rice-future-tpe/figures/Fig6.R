#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015
stop("!")

#stress profile plots: mean weekly stress index vs. days after emergence

#load libraries
#library(sp); library(maptools); library(raster); library(rgeos); library(rasterVis)
#library(ggplot2); library(grid); library(gridExtra)
#data(wrld_simpl)

#directories
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

#load historical probability (this contains environment and stress_profile names)
load(file=paste(an_dir,"/yield_probability_historical_named.RData",sep=""))
his_quant <- his_quant[which(his_quant$stress_cluster != 0),c("env_cluster","stress_cluster","env_name","stress_name","freq")]

#############################################################################################
### plot stress profiles for historical
#############################################################################################

if (!file.exists(paste(an_dir,"/stress_profile_historical.RData",sep=""))) {
  #load and merge data
  rs_data <- read.csv(paste(res_dir,"/RES_historical.csv",sep=""), sep=";") #this is the 5-day output (for stress profile)
  st_data <- read.csv(paste(res_dir,"/env_cluster.csv",sep=""), sep=";")
  rs_data$ID <- paste(rs_data$ID)
  st_data$ID <- paste(st_data$ID)
  rs_data <- merge(rs_data, st_data[,c("ID","clust")], by="ID", all.x=T)
  rs_data$Year <- as.numeric(sapply(rs_data$ID, FUN=function(x) {y <- unlist(strsplit(x,"_",fixed=T)); y <- as.numeric(y[length(y)]); return(y)}))
  rs_data$DOY <- as.numeric(format(as.Date(paste(rs_data$data),format="%d/%m/%Y"), "%j"))
  names(rs_data)[grep("clust",names(rs_data))] <- "stress_cluster"
  names(rs_data)[grep("Cluster",names(rs_data),fixed=T)] <- "env_cluster"
  
  #merge rs_data with names of clusters
  rs_data <- merge(rs_data, his_quant, by=c("env_cluster","stress_cluster"))
  
  #list of clusters
  clus_list <- unique(rs_data$env_name)
  
  #loop clusters
  stress_df <- data.frame()
  for (clus in clus_list) {
    #clus <- clus_list[1]
    
    #get data
    clus_data <- rs_data[which(rs_data$env_name == clus),]
    clus_data <- clus_data[which(!is.na(clus_data$stress_cluster)),]
    stress_list <- sort(unique(clus_data$stress_name))
    
    #frequency table
    clus_freq <- his_quant[which(his_quant$env_name == clus),]
    clus_freq$leg <- paste(": ",sprintf("%2.1f",clus_freq$freq)," %", sep="")
    
    pdf(paste(fig_dir,"/fig_6_profile_",clus,"_historical.pdf",sep=""), height=7,width=10,pointsize=17)
    leg_s1 <- leg_s2 <- leg_s3 <- NULL
    for (stp in stress_list) {
      #stp <- stress_list[1]
      stress_data <- clus_data[which(clus_data$stress_name == stp),]
      stp_freq <- clus_freq[which(clus_freq$stress_name == stp),]
      
      #median
      stress_data1 <- aggregate(stress_data[,c("PCEW","DVS")], by=list(index=stress_data$index),FUN=function(x) {median(x,na.rm=T)})
      stress_data1 <- stress_data1[stress_data1$index %in% c("C","D","E","F","G","H","I","J","K","L","M"),]
      stress_data1 <- stress_data1[order(stress_data1$index),]
      stress_data1$dae <- seq(21,84,by=7)
      row.names(stress_data1) <- NULL
      
      #lower limit
      stress_data2 <- aggregate(stress_data[,c("PCEW","DVS")], by=list(index=stress_data$index),FUN=function(x) {quantile(x,probs=0.25,na.rm=T)})
      stress_data2 <- stress_data2[stress_data2$index %in% c("C","D","E","F","G","H","I","J","K","L","M"),]
      stress_data2 <- stress_data2[order(stress_data2$index),]
      stress_data2$dae <- seq(21,84,by=7)
      stress_data2$DVS <- NULL; row.names(stress_data2) <- NULL; names(stress_data2)[2] <- "PCEW_ll"
      
      #upper limit
      stress_data3 <- aggregate(stress_data[,c("PCEW","DVS")], by=list(index=stress_data$index),FUN=function(x) {quantile(x,probs=0.75,na.rm=T)})
      stress_data3 <- stress_data3[stress_data3$index %in% c("C","D","E","F","G","H","I","J","K","L","M"),]
      stress_data3 <- stress_data3[order(stress_data3$index),]
      stress_data3$dae <- seq(21,84,by=7)
      stress_data3$DVS <- NULL; row.names(stress_data3) <- NULL; names(stress_data3)[2] <- "PCEW_ul"
      
      #merge the three data.frames
      stress_data <- merge(stress_data1, stress_data2[,c("index","PCEW_ll")], by="index")
      stress_data <- merge(stress_data, stress_data3[,c("index","PCEW_ul")], by="index")
      
      #add columns and bind
      stress_data <- cbind(env_cluster=clus, stress_profile=stp, stress_data)
      stress_df <- rbind(stress_df, stress_data)
      
      if (stp == "S1") {
        par(mar=c(5,5,1,1), lwd=1.5, las=1)
        plot(stress_data$dae, stress_data$PCEW, ty='l', lwd=2.5, col="blue", ylim=c(0.2,1.1),
             xlab="Days after emergence", ylab="Stress index")
        polygon(x=c(stress_data$dae,rev(stress_data$dae)),y=c(stress_data$PCEW_ll,rev(stress_data$PCEW_ul)),border=NA, col=rgb(red=0,green=100,blue=255,alpha=50,maxColorValue=255))
        leg_s1 <- paste("SP1",stp_freq$leg,sep="")
      } else if (stp == "S2") {
        lines(stress_data$dae, stress_data$PCEW, lwd=2.5, col="dark green")
        polygon(x=c(stress_data$dae,rev(stress_data$dae)),y=c(stress_data$PCEW_ll,rev(stress_data$PCEW_ul)),border=NA, col=rgb(red=0,green=51,blue=0,alpha=50,maxColorValue=255))
        leg_s2 <- paste("SP2",stp_freq$leg,sep="")
      } else if (stp == "S3") {
        lines(stress_data$dae, stress_data$PCEW, lwd=2.5, col="red")
        polygon(x=c(stress_data$dae,rev(stress_data$dae)),y=c(stress_data$PCEW_ll,rev(stress_data$PCEW_ul)),border=NA, col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
        leg_s3 <- paste("SP3",stp_freq$leg,sep="")
      }
    }
    
    #add grid, legend and vertical lines indicating phenology
    grid()
    if (clus == "HFE") {
      abline(v=c(45,71),lty=2,lwd=1.5,col="black")
    } else {
      abline(v=c(45,70),lty=2,lwd=1.5,col="black")
    }
    text(x=c(32,58,78),y=c(1.05,1.05,1.05),labels=c("Vegetative","Reproductive","Grain\nFilling"))
    legend("bottomleft", lty=1, lwd=2.5, legend=c(leg_s1,leg_s2,leg_s3), col=c("blue","dark green","red"),
           text.col=c("blue","dark green","red"), title.col="black", box.lwd=NA,bg=NA)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 fig_6_profile_",clus,"_historical.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_6_profile_",clus,"_historical.png",sep=""))
    setwd("~")
  }
  save(stress_df, file=paste(an_dir,"/stress_profile_historical.RData",sep=""))
} else {
  load(file=paste(an_dir,"/stress_profile_historical.RData",sep=""))
  his_stressdf <- stress_df; rm(stress_df)
}


##################################################################################
##### future model runs
##################################################################################

#list files in Arquivos_RCL.zip
setwd(res_dir)
rcl_flist <- unzip("Arquivos_RCL.zip",list=T)
rcl_flist <- rcl_flist$Name[grep("\\.csv",rcl_flist$Name)]
setwd("~")

#loop rcp, gcm, method, and co2p
for (rcp in rcplist) {
  #rcp <- rcplist[1]
  
  #prepare files
  if (!file.exists(paste(an_dir,"/stress_profile_",rcp,".RData",sep=""))) {
    #load fut_quant
    load(file=paste(an_dir,"/yield_probability_",rcp,"_named.RData",sep=""))
    
    stress_df <- data.frame()
    for (gcm in gcmlist) {
      for (bc in bclist) {
        for (co2p in co2list) {
          #gcm <- gcmlist[1]; bc <- bclist[1]; co2p <- co2list[1]
          co2i <- substr(co2p,1,1)
          cat("...processing rcp=",rcp,"/ gcm=",gcm,"/ bc_method=",bc,"/ co2=",co2p,"\n")
          
          #uncompress only this file
          fname <- paste("Arquivos_RCL/",co2p,"/RCL_M7_F1_res_",co2i,"_method_",bc,"_",rcp,"_",gcm,".csv",sep="")
          if (fname == "Arquivos_RCL/High/RCL_M7_F1_res_H_method_del_rcp45_gfdl_esm2g.csv") {
            fname <- "Arquivos_RCL/High/RCL_M7_F1_res_H_method_del_rcp45_esm2g.csv"
          }
          if (file.exists(paste(res_dir,"/",fname,sep=""))) {system(paste("rm -f ",res_dir,"/",fname,sep=""))}
          setwd(res_dir)
          system(paste("7z x Arquivos_RCL.zip ",fname," -r",sep=""))
          setwd("~")
          
          #load file, then delete
          res_data <- read.csv(paste(res_dir,"/",fname,sep=""),sep=";")
          if (file.exists(paste(res_dir,"/",fname,sep=""))) {system(paste("rm -f ",res_dir,"/",fname,sep=""))}
          names(res_data)[7] <- "env_cluster"
          
          #select from fut_quant, and merge with res_data
          tquant <- fut_quant[which(fut_quant$gcm == gcm & fut_quant$bc_method == bc & fut_quant$co2 == co2p & fut_quant$stress_cluster != 0),c("env_cluster","stress_cluster","env_name","stress_name","freq")]
          res_data <- merge(res_data, tquant, by=c("env_cluster","stress_cluster"), all.x=T)
          
          #list of clusters
          clus_list <- unique(res_data$env_name)
          
          for (clus in clus_list) {
            #clus <- clus_list[1]
            clus_data <- res_data[which(res_data$env_name == clus),]
            clus_data <- clus_data[which(!is.na(clus_data$stress_cluster)),]
            stress_list <- sort(unique(clus_data$stress_name))
            
            for (stp in stress_list) {
              #stp <- stress_list[1]
              stress_data <- clus_data[which(clus_data$stress_name == stp),]
              stress_data$ID2 <- paste(stress_data$ID2)
              stress_data$SDATE <- as.numeric(sapply(stress_data$ID2, FUN=function(x) {y <- unlist(strsplit(x,"_",fixed=T)); y <- as.numeric(y[4]); return(y)}))
              stress_data$DAP <- stress_data$DOY - stress_data$SDATE
              stress_data$DAP[which(stress_data$DAP < 0)] <- stress_data$DAP[which(stress_data$DAP < 0)] + 365
              
              stp_freq <- tquant$freq[which(tquant$env_name == clus & tquant$stress_name == stp)]
              
              #median
              stress_data1 <- aggregate(stress_data[,c("PCEW","DVS","DAP")], by=list(index=stress_data$index),FUN=function(x) {median(x,na.rm=T)})
              stress_data1 <- stress_data1[stress_data1$index %in% c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"),]
              stress_data1 <- stress_data1[order(stress_data1$index),]
              row.names(stress_data1) <- NULL
              
              #lower limit
              stress_data2 <- aggregate(stress_data[,c("PCEW","DVS","DAP")], by=list(index=stress_data$index),FUN=function(x) {quantile(x,probs=0.25,na.rm=T)})
              stress_data2 <- stress_data2[stress_data2$index %in% c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"),]
              stress_data2 <- stress_data2[order(stress_data2$index),]
              row.names(stress_data2) <- NULL; names(stress_data2)[2] <- "PCEW_ll"
              stress_data2$DVS <- stress_data2$DAP <- NULL
              
              #upper limit
              stress_data3 <- aggregate(stress_data[,c("PCEW","DVS","DAP")], by=list(index=stress_data$index),FUN=function(x) {quantile(x,probs=0.75,na.rm=T)})
              stress_data3 <- stress_data3[stress_data3$index %in% c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"),]
              stress_data3 <- stress_data3[order(stress_data3$index),]
              row.names(stress_data3) <- NULL; names(stress_data3)[2] <- "PCEW_ul"
              stress_data3$DVS <- stress_data3$DAP <- NULL
              
              #merge the three data.frames
              stress_data <- merge(stress_data1, stress_data2, by="index")
              stress_data <- merge(stress_data, stress_data3, by="index")
              
              #reorganise and cbind
              str_out <- as.data.frame(t(stress_data[,c("DVS","DAP","PCEW","PCEW_ll","PCEW_ul")]))
              names(str_out) <- stress_data$index
              row.names(str_out) <- NULL
              str_out <- cbind(variable=c("DVS","DAP","PCEW","PCEW_ll","PCEW_ul"), str_out)
              str_out <- cbind(rcp=rcp, gcm=gcm, bc_method=bc, co2=co2p, env_cluster=clus, 
                               stress_profile=stp, freq=stp_freq, str_out)
              if (is.null(str_out$N)) {str_out$N <- NA}
              if (is.null(str_out$O)) {str_out$O <- NA}
              if (is.null(str_out$P)) {str_out$P <- NA}
              if (is.null(str_out$Q)) {str_out$Q <- NA}
              if (is.null(str_out$R)) {str_out$R <- NA}
              if (is.null(str_out$S)) {str_out$S <- NA}
              stress_df <- rbind(stress_df, str_out)
            }
          }
        }
      }
    }
    save(stress_df, file=paste(an_dir,"/stress_profile_",rcp,".RData",sep=""))
  }
}
#clean up
system(paste("rm -rf ",res_dir,"/Arquivos_RCL",sep=""))


#loop rcp, gcm, method, and co2p
for (rcp in rcplist) {
  #rcp <- rcplist[1]
  
  #load file
  load(file=paste(an_dir,"/stress_profile_",rcp,".RData",sep=""))
  clusname <- unique(paste(stress_df$env_cluster))
  
  #loop co2 values
  for (co2p in co2list) {
    #co2p <- co2list[1]
    
    #phenology in oryza2000:
    #1. basic vegetative phase: from emergence (DVS=0.0) to start of photoperiod 
    #   sensitive phase (DVS=0.4)
    #2. photoperiod sensitive phase: from DVS=0.4 to panicle initiation (DVS=0.65)
    #3. panicle formation phase: from panicle initiation to 50 % flowering (DVS=1.0)
    #4. grain filling phase, from 50 % flowering to physiological maturity (DVS=2.0)
    for (clus in clusname) {
      #clus <- clusname[1]
      cat("...processing rcp=",rcp,"/ co2=",co2p,"/ env_cluster=",clus,"\n")
      clus_data <- stress_df[which(stress_df$co2 == co2p & stress_df$env_cluster == clus),]
      stress_list <- sort(unique(paste(clus_data$stress_profile)))
      
      pdf(paste(fig_dir,"/fig_6_profile_",clus,"_",co2p,"_",rcp,".pdf",sep=""), height=7,width=10,pointsize=17)
      leg_s1 <- leg_s2 <- leg_s3 <- NULL
      dap1 <- dap2 <- c()
      for (stp in stress_list) {
        #stp <- stress_list[1]
        stpdf <- clus_data[which(clus_data$stress_profile == stp),]
        
        #calculate median and 5-95 % of stress profile
        stpdf_ll <- aggregate(stpdf[,c("freq","C","D","E","F","G","H","I","J","K","L","M")], by=list(variable=stpdf$variable), FUN=function(x) {quantile(x,probs=0.25,na.rm=T)})
        stpdf_me <- aggregate(stpdf[,c("freq","C","D","E","F","G","H","I","J","K","L","M")], by=list(variable=stpdf$variable), FUN=function(x) {median(x,na.rm=T)})
        stpdf_ul <- aggregate(stpdf[,c("freq","C","D","E","F","G","H","I","J","K","L","M")], by=list(variable=stpdf$variable), FUN=function(x) {quantile(x,probs=0.75,na.rm=T)})
        
        #determine time at 0.65 and time at 1.0
        tdap <- as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")])
        tdvs <- as.numeric(stpdf_me[2,c("C","D","E","F","G","H","I","J","K","L","M")])
        tdf <- data.frame(dap=tdap,dvs=tdvs)
        tdf$flo <- tdf$dvs - 0.65; tdf$gfi <- tdf$dvs - 1.00
        a1 <- max(tdf$dvs[which(tdf$flo < 0)]); a2 <- min(tdf$dvs[which(tdf$flo > 0)])
        b1 <- max(tdf$dap[which(tdf$flo < 0)]); b2 <- min(tdf$dap[which(tdf$flo > 0)])
        dap1 <- c(dap1, (b1 + (0.65-a1) * (b2 - b1) / (a2 - a1)))
        a1 <- max(tdf$dvs[which(tdf$gfi < 0)]); a2 <- min(tdf$dvs[which(tdf$gfi > 0)])
        b1 <- max(tdf$dap[which(tdf$gfi < 0)]); b2 <- min(tdf$dap[which(tdf$gfi > 0)])
        dap2 <- c(dap2, (b1 + (1-a1) * (b2 - b1) / (a2 - a1)))
        
        if (stp == "S1") {
          par(mar=c(5,5,1,1),las=1,lwd=1.5)
          plot(x=as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]),
               y=as.numeric(stpdf_me[3,c("C","D","E","F","G","H","I","J","K","L","M")]),
               lwd=2.5, col="blue", ylim=c(0.2,1.1), ty="l", 
               xlab="Days after emergence", ylab="Stress index")
          polygon(x=c(as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]),rev(as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]))),
                  y=c(as.numeric(stpdf_me[4,c("C","D","E","F","G","H","I","J","K","L","M")]),rev(as.numeric(stpdf_me[5,c("C","D","E","F","G","H","I","J","K","L","M")]))),
                  border=NA, col=rgb(red=0,green=100,blue=255,alpha=50,maxColorValue=255))
          leg_s1 <- paste("SP1: ",sprintf("%2.1f",stpdf_me$freq[1])," [",sprintf("%2.1f",stpdf_ll$freq[1])," - ",sprintf("%2.1f",stpdf_ul$freq[1]),"] %", sep="")
        } else if (stp == "S2") {
          lines(x=as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]),
               y=as.numeric(stpdf_me[3,c("C","D","E","F","G","H","I","J","K","L","M")]), 
               lwd=2.5, col="dark green")
          polygon(x=c(as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]),rev(as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]))),
                  y=c(as.numeric(stpdf_me[4,c("C","D","E","F","G","H","I","J","K","L","M")]),rev(as.numeric(stpdf_me[5,c("C","D","E","F","G","H","I","J","K","L","M")]))),
                  border=NA, col=rgb(red=0,green=51,blue=0,alpha=50,maxColorValue=255))
          leg_s2 <- paste("SP2: ",sprintf("%2.1f",stpdf_me$freq[1])," [",sprintf("%2.1f",stpdf_ll$freq[1])," - ",sprintf("%2.1f",stpdf_ul$freq[1]),"] %", sep="")
        } else if (stp == "S3") {
          lines(x=as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]),
                y=as.numeric(stpdf_me[3,c("C","D","E","F","G","H","I","J","K","L","M")]), 
                lwd=2.5, col="red")
          polygon(x=c(as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]),rev(as.numeric(stpdf_me[1,c("C","D","E","F","G","H","I","J","K","L","M")]))),
                  y=c(as.numeric(stpdf_me[4,c("C","D","E","F","G","H","I","J","K","L","M")]),rev(as.numeric(stpdf_me[5,c("C","D","E","F","G","H","I","J","K","L","M")]))),
                  border=NA, col=rgb(red=255,green=0,blue=0,alpha=50,maxColorValue=255))
          leg_s3 <- paste("SP3: ",sprintf("%2.1f",stpdf_me$freq[1])," [",sprintf("%2.1f",stpdf_ll$freq[1])," - ",sprintf("%2.1f",stpdf_ul$freq[1]),"] %", sep="")
        }
      }
      
      #vertical lines indicative of vegetative, reproductive and grain-filling periods
      grid()
      abline(v=c(mean(dap1),mean(dap2)),lty=2,lwd=1.5,col="black")
      text(x=c((mean(dap1)-13),(mean(dap1)+13),(mean(dap2)+7)),y=c(1.05,1.05,1.05),
           labels=c("Vegetative","Reproductive","Grain\nFilling"))
      legend("bottomleft", lty=1, lwd=2.5, legend=c(leg_s1,leg_s2,leg_s3), col=c("blue","dark green","red"),
             text.col=c("blue","dark green","red"), title.col="black", box.lwd=NA,bg=NA)
      dev.off()
      setwd(fig_dir)
      system(paste("convert -verbose -density 300 fig_6_profile_",clus,"_",co2p,"_",rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_6_profile_",clus,"_",co2p,"_",rcp,".png",sep=""))
      setwd("~")
    }
  }
}


