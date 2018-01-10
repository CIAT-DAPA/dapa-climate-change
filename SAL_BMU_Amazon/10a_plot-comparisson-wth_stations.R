#####################################
## Plot dispersion 
#####################################

require(raster)

iDir <- "Z:/DATA/WP2/01_Weather_Stations/MERGE/climatology/combined"
aDir <- "D:/cenavarro/lat-bmu/outputs/average"
oDir <- "D:/cenavarro/lat-bmu/outputs/performance"
xt <- extent(readOGR("Z:/GEODATA/AMAZON/SHAPEFILES/Ecoregions/tnc_terrestial_ecoregions_napo.shp", layer= "tnc_terrestial_ecoregions_napo"))
rg <- "amz_1981_2010"
ext <- "tif"

mths = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
varList <- c("rain", "tmax", "tmin")

for(var in varList){
  
#   var <- varList[2]

  # Observados
  st <- read.csv(paste0(iDir, "/", var, "_", rg, ".csv"))
  st.reg <- st[which(st$LONG >= xt@xmin & st$LONG <= xt@xmax & st$LAT >= xt@ymin & st$LAT <= xt@ymax),]
  
  if (var == "rain"){
    st.reg <- st.reg[which(st.reg$SOURCE!="chirps"),]
  } else {
    st.reg <- st.reg[which(st.reg$SOURCE!="AGMERRA"),]
  }
  
  #Cleansing from any -9999 value
  for (i in 9:20) {
    OKrow <- which(st.reg[,i] != -9999.9)
    NArow <- which(st.reg[,i] == -9999.9)
    if (length(NArow) >= 1) {
      st.reg <- st.reg[OKrow,]
    }
  }
  
  coor <- st.reg[,6:7]
  
  #Ajustados
  if (var == "rain"){
    stk <- stack(paste0(aDir, "/prec_",1:12,".", ext)) 
  } else {
    stk <- stack(paste0(aDir, "/", var, "_",1:12,".", ext)) / 10
  }
  data.adj <- extract(stk, coor)
  

  # Plot comparisson by 12 months
  tiff(paste(oDir, "/station_comparisson_", rg, "_", var, ".tiff",sep=""),width = 1600, height = 600, pointsize = 20, compression="lzw")
  
  # c(bottom, left, top, right)
  par(mfrow=c(2,6), mar=c(1, 1, 2, 1), oma=c(4, 4, 0, 0))
  
  if (var == "rain"){
    limits = c(0, 800)
    color = "blue"
    xlabel = "Observed (mm/month)"
    ylabel = "Estimated (mm/month)"
  } else if (var == "tmax") {
    limits = c(0, 35)
    color = "red"
    xlabel = "Observed (°C)"
    ylabel = "Estimated (°C)"
  } else if (var == "tmin") {
    limits = c(0, 35)
    color = "orange"
    xlabel = "Observed (°C)"
    ylabel = "Estimated (°C)"
  }
  
  
  for (mth in 1:12){
    
    obs <- st.reg[,mth+8]  
    adj <- data.adj[,mth]
    
#     if (rg == "ame" && var != "prec"){
#       obs <- obs/10
#       adj <- adj/10
#     }
    
    m1 <- lm(adj ~ obs)
    r <- summary(m1)$r.squared
    
    if (mth == 1){
      plot(obs, adj, col=color, xlab="", ylab="", xlim=limits, ylim=limits, cex=0.8, main=mths[mth], xaxt = "n")  
    } else if (mth == 7){
      plot(obs, adj, col=color, xlab="", ylab="", xlim=limits, ylim=limits, cex=0.8, main=mths[mth])
    } else if (mth > 7){
      plot(obs, adj, col=color, xlab="", ylab="", xlim=limits, ylim=limits, cex=0.8, main=mths[mth], yaxt = "n")
    } else {
      plot(obs, adj, col=color, xlab="", ylab="", xlim=limits, ylim=limits, cex=0.8, main=mths[mth], yaxt = "n", xaxt = "n")  
    }
    # plot(obs, adj, col=color, xlab="", ylab="", xlim=limits, ylim=limits, cex=1, main=mths[mth], x)
    
    
    
    grid()
    abline(0,1,lty=1)
    abline(m1,lty=2, col="red")
    legend("topleft",paste(expression(R2),round(r,4),sep="="), bty = "n", cex = 1.4)
    mtext(c(xlabel, ylabel), c(SOUTH<-1, WEST<-2), line=2, col="black", outer=TRUE)
    
  }
  
  dev.off()
  
}

