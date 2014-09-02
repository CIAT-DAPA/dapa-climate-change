#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
#stop("!")

#1. read in the meta-data
#2. list and loop properties
#3. for each variable list and loop studies
#4. for each study loop treatments
#5. for each treatment perform a rectangular hyperbola fit for photo, biomass, yield. 
#   and else a linear one
#6. calculate value at 380 ppm using the fit
#7. calculate ratios for the other concentrations with respect to C_ref=380 ppm
#8. create a new table for property, type, water, concentration and ratio to 380 ppm
#9. use this one to construct box plots and response curves where possible

#working directory
wd <- "~/Leeds-work/AgMIP-maize-phase-2/parameters"
out_dir <- paste(wd,"/scaled_response",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir)}

#1. read in the meta-data
co2_resp <- read.csv(paste(wd,"/co2_response.txt",sep=""),sep="\t")

#remove untrusted measurements
#King & Greer (1986) dry yield measurement as it was likely affected by swff or similar
#Vanaja et al. (2001) stomatal conductance as too low to be digitised with confidence
co2_resp <- co2_resp[which(co2_resp$Trust == "yes"),]
row.names(co2_resp) <- 1:nrow(co2_resp)

#2. list and loop properties
prop_list <- paste(unique(co2_resp$Param_std))
for (prop in prop_list) {
  #prop <- prop_list[1]
  cat("...processing property=",prop,"\n")
  
  #3. for each variable list and loop studies
  resp_prop <- co2_resp[which(co2_resp$Param_std == prop),]
  stud_list <- paste(unique(resp_prop$Study_ID))
  
  all_df <- data.frame()
  for (stud in stud_list) {
    #stud <- stud_list[1]
    resp_stud <- resp_prop[which(resp_prop$Study_ID == stud),]
    
    #4. for each study loop treatments
    treat_list <- paste(unique(resp_stud$Treatment))
    for (treat in treat_list) {
      #treat <- treat_list[1]
      resp_treat <- resp_stud[which(resp_stud$Treatment == treat),]
      wat_stat <- paste(unique(resp_treat$Water))[1]
      exp_type <- paste(unique(resp_treat$Type))[1]
      
      #5. for each treatment use a linear fit for all variables, since ymax has implications
      #   and cannot be constrained per study with too few data
      fitdf <- data.frame(conc=c(resp_treat$CO2_amb,resp_treat$CO2_ele),
                          value=c(resp_treat$Value_amb,resp_treat$Value_ele))
      fitdf <- unique(fitdf)
      c_ref <- min(fitdf$conc)
      fitdf$value_std <- fitdf$value / fitdf$value[which(fitdf$conc == c_ref)]
      
      #hyperbola
      #fitfun <- nls(value ~ (a*conc*1.5)/(a*conc+1.5),
      #              data=fitdf, start = list(a = 0.01),trace=T,
      #              control=nls.control(maxiter=1000,printEval=F,warnOnly=F))
      
      #linear fit
      fitfun <- glm(value_std ~ conc, data=fitdf)
      
      #6. calculate value at 380 ppm using the fit
      ypred <- predict(fitfun, data.frame(conc=380))
      val_ref <- ypred*fitdf$value[which(fitdf$conc == c_ref)]
      
      #7. calculate ratios for the other concentrations with respect to C_ref=380 ppm
      fitdf$value_380 <- fitdf$value / val_ref
      
      #8. create a new table for property, type, water, concentration and ratio to 380 ppm
      for (i in 1:nrow(fitdf)) {
        out_row <- data.frame(study=stud,property=prop,type=exp_type,water=wat_stat,
                              co2_conc=fitdf$conc[i],ratio=fitdf$value_380[i])
        all_df <- rbind(all_df, out_row)
      }
    }
  }
  write.csv(all_df, paste(out_dir,"/co2_response_",prop,".csv",sep=""),quote=T,row.names=F)
  
  if (!prop %in% c("hi","rue")) {
    #plotting data.frame
    plot_df <- all_df[which(all_df$water != "mid"),]
    plot_df$water <- factor(paste(plot_df$water), levels=c("wet","dry"))
    plot_df$face_stat <- "Non-FACE"; plot_df$face_stat[which(plot_df$type == "FACE")] <- "FACE"
    plot_df$face_stat_water <- paste(plot_df$water,".",plot_df$face_stat,sep="")
    plot_df$face_stat_water <- factor(plot_df$face_stat_water, 
                                      levels=c("wet.Non-FACE","dry.Non-FACE","wet.FACE","dry.FACE"),
                                      labels=c("NF (wet)", "NF (dry)", "FC (wet)", "FC (dry)"))
    
    #remove responses < 400 ppm
    plot_df2 <- plot_df[which(plot_df$co2_conc > 400),]
    
    #plot the four factors
    maxval <- max(c(plot_df2$ratio,1.85)) #1.85
    if (prop %in% c("stom_cond","trans")) maxval <- 1.1
    minval <- min(c(plot_df2$ratio,0.8))
    pdf(paste(out_dir,"/boxplot_",prop,"_4factors.pdf",sep=""), height=8,width=6,pointsize=16)
    par(las=2,mar=c(5,5,1,1),lwd=1.5)
    boxplot(plot_df2$ratio ~ plot_df2$face_stat_water,ylab="Response ratio",pch=20,ylim=c(minval,maxval),
            horizontal=F,outcol="red",medcol="red",boxcol="blue",col="white",border="black")
    grid()
    dev.off()
    
    #plot just dry vs. wet
    pdf(paste(out_dir,"/boxplot_",prop,"_2factors.pdf",sep=""), height=6,width=4,pointsize=16)
    par(las=1,mar=c(3,5,1,1),lwd=1.5)
    boxplot(plot_df2$ratio ~ plot_df2$water,ylab="Response ratio",pch=20,ylim=c(minval,maxval),
            horizontal=F,outcol="red",medcol="red",boxcol="blue",col="white",border="black")
    grid()
    dev.off()
    
    #scatterplot
    wetplot <- plot_df[which(plot_df$water=="wet"),]
    dryplot <- plot_df[which(plot_df$water=="dry"),]
    
    if (nrow(plot_df) > 3 & nrow(dryplot) > 3 & nrow(wetplot) > 3) {
      pdf(paste(out_dir,"/scatter_",prop,"_2factors.pdf",sep=""), height=6,width=8,pointsize=16)
      par(las=1,mar=c(5,5,1,1),lwd=2)
      plot(plot_df$co2_conc, plot_df$ratio, xlim=c(200,1100),ylim=c(minval,maxval),pch=NA,
           xlab="Carbon dioxide concentration (ppm)", ylab="Response ratio")
      points(dryplot$co2_conc,dryplot$ratio,pch=20,col="red")
      points(wetplot$co2_conc,wetplot$ratio,pch=20,col="blue")
      
      terr <- try(nls(ratio ~ (a*co2_conc*ymax)/(a*co2_conc+ymax),
                      data=plot_df, start = list(a = 0.01, ymax=1.01),trace=F,
                      control=nls.control(maxiter=1000,printEval=F,warnOnly=F)), silent=T)
      if (class(terr)[1] == "try-error") {
        regall <- glm(ratio ~ co2_conc, data=plot_df)
        regwet <- glm(ratio ~ co2_conc, data=wetplot)
        regdry <- glm(ratio ~ co2_conc, data=dryplot)
      } else {
        regall <- nls(ratio ~ (a*co2_conc*ymax)/(a*co2_conc+ymax),
                      data=plot_df, start = list(a = 0.01, ymax=1.01),trace=F,
                      control=nls.control(maxiter=1000,printEval=F,warnOnly=F))
        regwet <- nls(ratio ~ (a*co2_conc*ymax)/(a*co2_conc+ymax),
                      data=wetplot, start = list(a = 0.01, ymax=1.01),trace=F,
                      control=nls.control(maxiter=1000,printEval=F,warnOnly=F))
        regdry <- nls(ratio ~ (a*co2_conc*ymax)/(a*co2_conc+ymax),
                      data=dryplot, start = list(a = 0.01, ymax=1.01),trace=F,
                      control=nls.control(maxiter=1000,printEval=F,warnOnly=F))
      }
      lines(280:1000, predict(regall, data.frame(co2_conc=280:1000)))
      lines(280:1000, predict(regwet, data.frame(co2_conc=280:1000)),col="blue")
      lines(280:1000, predict(regdry, data.frame(co2_conc=280:1000)),col="red")
      rm(list=c("regall","regwet","regdry"))
      
      #plot face points somewhere
      wetfacedots <- wetplot[which(wetplot$face_stat == "FACE"),]
      dryfacedots <- dryplot[which(dryplot$face_stat == "FACE"),]
      points(wetfacedots$co2_conc, wetfacedots$ratio, col="blue", pch=22, cex=1.5)
      points(dryfacedots$co2_conc, dryfacedots$ratio, col="red", pch=22, cex=1.5)
      
      grid()
      dev.off()
    }
  }
}


#### parameterisation
#### produce an ensemble of 100 regressions for: trans, photo, wue for wet and dry (calibration)
#### the same procedure is done for yield and biomass (evaluation)
c550_resp <- data.frame()
for (prop in c("photo","wue","trans","stom_cond","yield","biomass")) {
  #prop <- "stom_cond"
  cat("...processing property=",prop,"\n")
  
  #load the data
  prop_dat <- read.csv(paste(out_dir,"/co2_response_",prop,".csv",sep=""))
  prop_dat <- prop_dat[which(prop_dat$water != "mid"),]
  prop_dat$water <- factor(paste(prop_dat$water), levels=c("wet","dry"))
  prop_dat$face_stat <- "Non-FACE"; prop_dat$face_stat[which(prop_dat$type == "FACE")] <- "FACE"
  prop_dat$face_stat_water <- paste(prop_dat$water,".",prop_dat$face_stat,sep="")
  prop_dat$face_stat_water <- factor(prop_dat$face_stat_water, 
                                    levels=c("wet.Non-FACE","dry.Non-FACE","wet.FACE","dry.FACE"),
                                    labels=c("NF (wet)", "NF (dry)", "FC (wet)", "FC (dry)"))
  
  #select wet and dry data
  wetplot <- prop_dat[which(prop_dat$water=="wet"),]
  dryplot <- prop_dat[which(prop_dat$water=="dry"),]
  
  pred_wet <- data.frame(conc=280:1000); pred_dry <- data.frame(conc=280:1000)
  for (i in 1:100) {
    nwet <- round(nrow(wetplot)*0.9)
    ndry <- round(nrow(dryplot)*0.9)
    wetdat <- wetplot[sample(1:nrow(wetplot),nwet,replace=F),]
    drydat <- dryplot[sample(1:nrow(dryplot),ndry,replace=F),]
    
    #perform regressions (nls if response found)
    #wet
    terr1 <- try(nls(ratio ~ (a*co2_conc*ymax)/(a*co2_conc+ymax),
                    data=wetdat, start = list(a = 0.01, ymax=1.01),trace=F,
                    control=nls.control(maxiter=1000,printEval=F,warnOnly=F)), silent=T)
    if (class(terr1)[1] == "try-error") {
      regwet <- glm(ratio ~ co2_conc, data=wetdat)
    } else {
      regwet <- nls(ratio ~ (a*co2_conc*ymax)/(a*co2_conc+ymax),
                    data=wetdat, start = list(a = 0.01, ymax=1.01),trace=F,
                    control=nls.control(maxiter=1000,printEval=F,warnOnly=F))
    }
    
    #dry
    terr2 <- try(nls(ratio ~ (a*co2_conc*ymax)/(a*co2_conc+ymax),
                     data=drydat, start = list(a = 0.01, ymax=1.01),trace=F,
                     control=nls.control(maxiter=1000,printEval=F,warnOnly=F)), silent=T)
    if (class(terr2)[1] == "try-error") {
      regdry <- glm(ratio ~ co2_conc, data=drydat)
    } else {
      regdry <- nls(ratio ~ (a*co2_conc*ymax)/(a*co2_conc+ymax),
                    data=drydat, start = list(a = 0.01, ymax=1.01),trace=F,
                    control=nls.control(maxiter=1000,printEval=F,warnOnly=F))
    }
    
    pred_wet$value <- predict(regwet, data.frame(co2_conc=280:1000))
    pred_dry$value <- predict(regdry, data.frame(co2_conc=280:1000))
    names(pred_wet)[ncol(pred_wet)] <- paste("iter.",i,sep="")
    names(pred_dry)[ncol(pred_dry)] <- paste("iter.",i,sep="")
  }
  
  pred_wet$mean <- rowMeans(pred_wet[,paste("iter.",1:100,sep="")],na.rm=T)
  pred_wet$q25 <- apply(pred_wet[,paste("iter.",1:100,sep="")],1,FUN=function(x) {quantile(x,probs=0.25)})
  pred_wet$q75 <- apply(pred_wet[,paste("iter.",1:100,sep="")],1,FUN=function(x) {quantile(x,probs=0.75)})
  pred_wet$q05 <- apply(pred_wet[,paste("iter.",1:100,sep="")],1,FUN=function(x) {quantile(x,probs=0.05)})
  pred_wet$q95 <- apply(pred_wet[,paste("iter.",1:100,sep="")],1,FUN=function(x) {quantile(x,probs=0.95)})
  
  pred_dry$mean <- rowMeans(pred_dry[,paste("iter.",1:100,sep="")],na.rm=T)
  pred_dry$q25 <- apply(pred_dry[,paste("iter.",1:100,sep="")],1,FUN=function(x) {quantile(x,probs=0.25)})
  pred_dry$q75 <- apply(pred_dry[,paste("iter.",1:100,sep="")],1,FUN=function(x) {quantile(x,probs=0.75)})
  pred_dry$q05 <- apply(pred_dry[,paste("iter.",1:100,sep="")],1,FUN=function(x) {quantile(x,probs=0.05)})
  pred_dry$q95 <- apply(pred_dry[,paste("iter.",1:100,sep="")],1,FUN=function(x) {quantile(x,probs=0.95)})
  
  #produce plot
  if (prop %in% c("trans","stom_cond")) {ylims <- c(0.1,1.15)} else {ylims <- c(0.8,1.85)}
  pdf(paste(out_dir,"/bootstraped_",prop,".pdf",sep=""), height=6,width=8,pointsize=16)
  par(las=1,mar=c(5,5,1,1),lwd=1.5)
  plot(pred_wet$conc, pred_wet$mean, pch=NA, ylim=ylims,xlab="Carbon dioxide concentration (ppm)",
       ylab="Response ratio")
  polygon(x=c(pred_dry$conc,rev(pred_dry$conc)),y=c(pred_dry$q05,rev(pred_dry$q95)),
          col=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255),
          border=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255))
  polygon(x=c(pred_dry$conc,rev(pred_dry$conc)),y=c(pred_dry$q25,rev(pred_dry$q75)),
          col=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255),
          border=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255))
  polygon(x=c(pred_wet$conc,rev(pred_wet$conc)),y=c(pred_wet$q05,rev(pred_wet$q95)),
          col=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255),
          border=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255))
  polygon(x=c(pred_wet$conc,rev(pred_wet$conc)),y=c(pred_wet$q25,rev(pred_wet$q75)),
          col=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255),
          border=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255))
  lines(pred_dry$conc, pred_dry$mean, col="red")
  lines(pred_wet$conc, pred_wet$mean, col="blue")
  abline(v=c(380,550),lty=2)
  grid()
  dev.off()
  
  
  #produce plot with points
  pdf(paste(out_dir,"/bootstraped_",prop,"_points.pdf",sep=""), height=6,width=8,pointsize=16)
  par(las=1,mar=c(5,5,1,1),lwd=1.5)
  plot(pred_wet$conc, pred_wet$mean, pch=NA, ylim=ylims,xlab="Carbon dioxide concentration (ppm)",
       ylab="Response ratio")
  polygon(x=c(pred_dry$conc,rev(pred_dry$conc)),y=c(pred_dry$q05,rev(pred_dry$q95)),
          col=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255),
          border=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255))
  polygon(x=c(pred_dry$conc,rev(pred_dry$conc)),y=c(pred_dry$q25,rev(pred_dry$q75)),
          col=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255),
          border=rgb(red=255,green=0,blue=0,alpha=70,maxColorValue=255))
  polygon(x=c(pred_wet$conc,rev(pred_wet$conc)),y=c(pred_wet$q05,rev(pred_wet$q95)),
          col=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255),
          border=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255))
  polygon(x=c(pred_wet$conc,rev(pred_wet$conc)),y=c(pred_wet$q25,rev(pred_wet$q75)),
          col=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255),
          border=rgb(red=0,green=0,blue=255,alpha=70,maxColorValue=255))
  lines(pred_dry$conc, pred_dry$mean, col="red")
  lines(pred_wet$conc, pred_wet$mean, col="blue")
  abline(v=c(380,550),lty=2)
  points(dryplot$co2_conc, dryplot$ratio, pch=20,col="red")
  points(wetplot$co2_conc, wetplot$ratio, pch=20,col="blue")
  wetfacedots <- wetplot[which(wetplot$face_stat == "FACE"),]
  dryfacedots <- dryplot[which(dryplot$face_stat == "FACE"),]
  points(wetfacedots$co2_conc, wetfacedots$ratio, col="blue", pch=22, cex=1.5)
  points(dryfacedots$co2_conc, dryfacedots$ratio, col="red", pch=22, cex=1.5)
  grid()
  dev.off()
  
  #write csv files
  write.csv(pred_dry, paste(out_dir,"/predicted_response_",prop,"_dry.csv",sep=""),row.names=T, quote=T)
  write.csv(pred_wet, paste(out_dir,"/predicted_response_",prop,"_wet.csv",sep=""),row.names=T, quote=T)
  
  #### using these 100 get response ratio at 500 ppm
  c550_dry <- as.numeric(pred_dry[which(pred_dry$conc == 550),paste("iter.",1:100,sep="")])
  c550_wet <- as.numeric(pred_wet[which(pred_wet$conc == 550),paste("iter.",1:100,sep="")])
  
  row_dry <- data.frame(property=prop,cond="dry",median=median(c550_dry),mean=mean(c550_dry),
                        min=min(c550_dry),max=max(c550_dry),q05=quantile(c550_dry,probs=0.05),
                        q95=quantile(c550_dry,probs=0.95),q25=quantile(c550_dry,probs=0.25),
                        q75=quantile(c550_dry,probs=0.75))
  row_wet <- data.frame(property=prop,cond="wet",median=median(c550_wet),mean=mean(c550_wet),
                        min=min(c550_wet),max=max(c550_wet),q05=quantile(c550_wet,probs=0.05),
                        q95=quantile(c550_wet,probs=0.95),q25=quantile(c550_wet,probs=0.25),
                        q75=quantile(c550_wet,probs=0.75))
  row_df <- rbind(row_dry,row_wet)
  row.names(row_df) <- 1:nrow(row_df)
  
  #append to final data.frame
  c550_resp <- rbind(c550_resp, row_df)
}

#save response at 550 ppm
write.csv(c550_resp, paste(out_dir,"/parameter_response_ratios.csv",sep=""),row.names=T, quote=T)






